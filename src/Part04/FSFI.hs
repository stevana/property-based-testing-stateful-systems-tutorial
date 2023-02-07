{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Part04.FSFI (module Part04.FSFI) where

-- https://danluu.com/deconstruct-files/

import Control.Exception
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (fromForeignPtr, toForeignPtr)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Data.Vector as Vector
import Foreign (copyBytes, plusPtr, withForeignPtr)
import GHC.IO.Device (SeekMode(..))
import System.Directory
import System.Posix.Files
import System.Posix.IO.ByteString
import System.Posix.Types
import System.Posix.Unistd
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

------------------------------------------------------------------------

data FS = FS
  { fsOpenFd :: FilePath -> IO Fd
  , fsWrite  :: Fd -> ByteString -> IO ByteCount
  , fsFsync  :: Fd -> IO ()
  , fsSeek   :: Fd -> SeekMode -> FileOffset -> IO FileOffset
  , fsRead   :: Fd -> ByteCount -> IO (ByteString, ByteCount)
  }

realFS :: IO FS
realFS = return FS
  { fsOpenFd = \fp -> openFd (BS.pack fp) ReadWrite
                        (Just (ownerReadMode `unionFileModes` ownerWriteMode))
                        defaultFileFlags { append = True }
  , fsWrite  = \fd bs -> fdWrite fd (BS.unpack bs)
  , fsFsync  = fileSynchronise
  , fsSeek   = fdSeek
  , fsRead   = \fd bc -> fdRead fd bc >>= \(s, bc') -> return (BS.pack s, bc')
  }

fsExample :: FS -> IO ()
fsExample fs = do
  fd <- fsOpenFd fs "/tmp/test.txt"
  void (fsWrite fs fd "hej")
  fsFsync fs fd
  void (fsSeek fs fd AbsoluteSeek 0)
  -- TODO: Second fsync.
  print =<< fsRead fs fd 3

unit_realFS :: IO ()
unit_realFS = do
  fs <- realFS
  fsExample fs

------------------------------------------------------------------------

data FakeDisk = FakeDisk
  { fdFiles   :: IORef (Map FilePath ByteString)
  , fdOpenFds :: IORef (Map Fd OpenFd)
  , fdNextFd  :: IORef Fd
  }

data OpenFd = OpenFd
  { ofdOffset   :: COff
  , ofdFilePath :: FilePath
  }

newFakeDisk :: IO FakeDisk
newFakeDisk = FakeDisk <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef (Fd 0)

fakeOpenFd :: FakeDisk -> FilePath -> IO Fd
fakeOpenFd fdisk fp = do
  fd <- atomicModifyIORef' (fdNextFd fdisk) (\fd -> (fd + 1, fd))
  -- Only add a file with empty content if there already isn't a file.
  modifyIORef (fdFiles fdisk) (Map.insertWith (<>) fp BS.empty)
  modifyIORef (fdOpenFds fdisk) (Map.insert fd (OpenFd 0 fp)) -- XXX: offset depends on opening mode?
  return fd

adjustOffset :: Integral i => (i -> i) -> Fd -> Map Fd OpenFd -> Map Fd OpenFd
adjustOffset f = Map.adjust (\(OpenFd offset fp) -> OpenFd (fromIntegral (f (fromIntegral offset))) fp)

fakeWrite :: FakeDisk -> Fd -> ByteString -> IO ByteCount
fakeWrite fdisk fd s = do
  ofd <- atomicModifyIORef' (fdOpenFds fdisk) (\ofds -> (adjustOffset (+ BS.length s) fd ofds, ofds Map.! fd))
  mOldBs <- Map.lookup (ofdFilePath ofd) <$> readIORef (fdFiles fdisk)
  newBs <- write (ofdOffset ofd) s (fromMaybe BS.empty mOldBs)
  modifyIORef (fdFiles fdisk) (Map.insert (ofdFilePath ofd) newBs)
  return (fromIntegral (BS.length s))

write :: COff -> ByteString -> ByteString -> IO ByteString
write off dst src = do
  let (dfptr, doff, dlen) = toForeignPtr dst
      (sfptr, soff, slen) = toForeignPtr src
  withForeignPtr dfptr $ \dptr ->
    withForeignPtr sfptr $ \sptr -> do
      copyBytes (dptr `plusPtr` (doff + fromIntegral off)) (sptr `plusPtr` soff) slen
      return (fromForeignPtr dfptr 0 (max dlen (fromIntegral off + slen)))

prop_write :: NonNegative Int -> ASCIIString -> ASCIIString -> Property
prop_write (NonNegative off) (ASCIIString dst) (ASCIIString src) = monadicIO $ do
  pre (off + length src <= length dst)
  bs <- run (write (fromIntegral off) (BS.pack dst) (BS.pack src))
  let s' = write' dst src off
  monitor (counterexample ("'" ++ BS.unpack bs ++ "' /= '" ++ s' ++ "'"))
  Test.QuickCheck.Monadic.assert (BS.unpack bs == s')

write' :: [a] -> [a] -> Int -> [a]
write' dst src off = Vector.toList (Vector.fromList dst Vector.// zip [off..off+length src] src)

fakeFsync :: FakeDisk -> Fd -> IO ()
fakeFsync _ _ = return () -- XXX

fakeSeek :: FakeDisk -> Fd -> SeekMode -> FileOffset -> IO FileOffset
fakeSeek fdisk fd _seekMode offset = do
  -- NOTE: Simplified, only absolute seek.
  modifyIORef (fdOpenFds fdisk) (adjustOffset (const offset) fd)
  return offset

fakeRead :: FakeDisk -> Fd -> ByteCount -> IO (ByteString, ByteCount)
fakeRead fdisk fd l = do
  ofds <- readIORef (fdOpenFds fdisk)
  let ofd = ofds Map.! fd
  files <- readIORef (fdFiles fdisk)
  let contents = files Map.! ofdFilePath ofd
      s        = BS.take (fromIntegral l) (BS.drop (fromIntegral (ofdOffset ofd)) contents)
  print contents
  return (s, fromIntegral (BS.length s))

fakeFS :: IO FS
fakeFS = do
  fdisk <- newFakeDisk
  return FS
    { fsOpenFd = fakeOpenFd fdisk
    , fsWrite  = fakeWrite  fdisk
    , fsFsync  = fakeFsync  fdisk
    , fsSeek   = fakeSeek   fdisk
    , fsRead   = fakeRead   fdisk
    }

unit_fakeFS :: IO ()
unit_fakeFS = do
  fs <- fakeFS
  fsExample fs

-- TODO: Contract test realFS against fakeFS.

------------------------------------------------------------------------

data CrashingFakeDisk = CrashingFakeDisk
  { cfdFakeDisk  :: FakeDisk
  , cfdUnflushed :: IORef (Map Fd ByteString) -- Not yet flushed to the filesystem.
  , cfdStdGen    :: IORef StdGen
  -- TODO: Add a way to configure the characteristics and distribution of the faults here.
  }

newCrashingFakeDisk :: Int -> IO CrashingFakeDisk
newCrashingFakeDisk seed =
  CrashingFakeDisk <$> newFakeDisk <*> newIORef Map.empty <*> newIORef (mkStdGen seed)

partialWrite :: CrashingFakeDisk -> Fd -> ByteString -> IO ByteCount
partialWrite cfdisk fd s = do
  -- NOTE: Simplified, write always appends to the end of the file...
  modifyIORef (cfdUnflushed cfdisk) (Map.insertWith (flip (<>)) fd s)
  return (fromIntegral (BS.length s))

partialFsync :: CrashingFakeDisk -> Fd -> IO ()
partialFsync cfdisk fd = do
  mUnflushed <- atomicModifyIORef' (cfdUnflushed cfdisk) (swap . Map.updateLookupWithKey (\_k _v -> Nothing) fd)
  case mUnflushed of
    Nothing -> return () -- Nothing to flush.
    Just unflushed -> do
      -- Dungeons and dragons style die roll where something bad happens if we throw a one.
      dieRoll <- atomicModifyIORef' (cfdStdGen cfdisk) (swap . randomR (1, 6 :: Int))
      if dieRoll == 1
      then do
        -- NOTE: Simplified, doesn't have to be prefix.
        prefix <- atomicModifyIORef' (cfdStdGen cfdisk) (swap . randomR (0, BS.length unflushed - 1))
        _bc <- fakeWrite (cfdFakeDisk cfdisk) fd (BS.take prefix unflushed)
        throwIO Crash
      else do
        _bc <- fakeWrite (cfdFakeDisk cfdisk) fd unflushed
        return ()

data Crash = Crash
  deriving stock Show

instance Exception Crash

crashingFS :: Int -> IO FS
crashingFS seed = do
  cfdisk <- newCrashingFakeDisk seed
  return FS
    { fsOpenFd = fakeOpenFd (cfdFakeDisk cfdisk)
    , fsWrite  = partialWrite cfdisk
    , fsFsync  = partialFsync cfdisk
    , fsSeek   = fakeSeek   (cfdFakeDisk cfdisk)
    , fsRead   = fakeRead   (cfdFakeDisk cfdisk)
    }

unit_crashingFS :: IO ()
unit_crashingFS = do
  let seed = 6
  fs <- crashingFS seed
  fsExample fs `catch` (\Crash -> handleCrash fs)
  where
    handleCrash fs = do
      fd <- fsOpenFd fs "/tmp/test.txt"
      void (fsSeek fs fd AbsoluteSeek 0)
      (s, _l) <- fsRead fs fd 3
      assertIO (s == "h") -- Partial write happened due to the crash.

assertIO :: Bool -> IO ()
assertIO b = Control.Exception.assert b (return ())

------------------------------------------------------------------------

-- TODO: Can we introduce a latency keeping fake disk which can simulate the
-- performance improvement of using 1PC + C vs 2PC?
-- https://www.redb.org/post/2022/07/26/faster-commits-with-1pcc-instead-of-2pc/

twoPhaseCommit :: FS -> FilePath -> ByteString -> IO ()
twoPhaseCommit fs fp s = do
  fd <- fsOpenFd fs fp
  -- We use one character for the length of the string. Ideally we should
  -- dedicate, say, 4 bytes for the length and encode a unsigned 32 bit integrer
  -- instead, so that we can write much larger strings.
  void (fsSeek fs fd AbsoluteSeek 1)
  void (fsWrite fs fd s)
  fsFsync fs fd
  -- Next two steps are "promote to primary", basically we want to atomically
  -- (writing one byte) mark that the data is now available. Note that if we
  -- write one byte the partiality of write will ensure that either it's written
  -- or not, i.e. atomic. There are atomic primitives for writing, say, unsinged
  -- 32 bit integers as well, if we would have used that as length.
  void (fsSeek fs fd AbsoluteSeek 0)
  void (fsWrite fs fd (BS.pack (show (BS.length s))))
  fsFsync fs fd

recover :: FS -> Fd -> IO ()
recover fs fd = do
  -- XXX: this read seems to segfault...
  (bs, _len) <- fsRead fs fd 1
  -- If the length hasn't been written, erase the rest of the content.
  if bs == BS.pack " "
  then return () -- void (fsWrite fs fd "          ")
  else return ()

prop_recovery :: Int -> Property
prop_recovery seed = monadicIO $ do
  let fp = "/tmp/prop_recovery.txt"
  run (removePathForcibly fp)
  fs <- run (crashingFS seed)
  -- NOTE: Simplified, since we are working with strings rather than
  -- bytestrings, one character (byte) can only represent 1-9 in ASCII.
  len <- pick (choose (1, 9))
  s <- pick (BS.pack <$> vectorOf len (choose ('a', 'z')))
  e <- run ((Right <$> twoPhaseCommit fs fp s) `catch` (\Crash -> do
                                                           fd <- fsOpenFd fs fp
                                                           recover fs fd
                                                           Left <$> fsRead fs fd (fromIntegral len)))
  case e of
    Left (t, _len) ->
      Test.QuickCheck.Monadic.assert (s == t || t == "" || t == "          ") -- No partial writes.
    Right () -> do
      fd <- run (fsOpenFd fs fp)
      (t, _len) <- run (fsRead fs fd (fromIntegral len))
      Test.QuickCheck.Monadic.assert (s == t)

  -- run (removePathForcibly fp)

-- segfaults:
-- s = do { fs <- crashingFS 0; (Right <$> twoPhaseCommit fs "/tmp/recovery.txt" "hej") `catch` (\Crash -> Left <$> do { fd <- fsOpenFd fs "/tmp/recovery.txt"; recover fs fd; fsRead fs fd 3 }) }
