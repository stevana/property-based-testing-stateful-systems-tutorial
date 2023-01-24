{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Part03.Service (module Part03.Service) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (IOException, bracket, catch, throwIO)
import Control.Monad (forM_)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.IORef
import Data.String (fromString)
import Data.Text.Read (decimal)
import Database.SQLite.Simple
       ( Connection
       , Only(Only)
       , close
       , execute
       , execute_
       , lastInsertRowId
       , open
       , query
       )
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import System.Timeout (timeout)

import Part03.Queue
import Part03.QueueInterface
import Part03.QueueTest (fakeDequeue, fakeEnqueue, newModel)

------------------------------------------------------------------------

mAX_QUEUE_SIZE :: Int
mAX_QUEUE_SIZE = 128

pORT :: Int
pORT = 8050

------------------------------------------------------------------------

-- We start by implementing our queue interface using the real mutable
-- array-backed queue and the fake immutable linked list based one.

realQueue :: Int -> IO (QueueI a)
realQueue sz = do
  q <- newQueue sz
  return QueueI
    { qiEnqueue = enqueue q
    , qiDequeue = dequeue q
    }

fakeQueue :: Int -> IO (QueueI a)
fakeQueue sz = do
  ref <- newIORef (newModel sz)
  return QueueI
    { qiEnqueue = \x -> atomicModifyIORef' ref (fakeEnqueue x)
    , qiDequeue =       atomicModifyIORef' ref fakeDequeue
    }

-- ^ Notice how the fake queue uses the model together with a mutable variable
-- (`IORef`) to keep track of the current state (starting with the inital
-- model).

------------------------------------------------------------------------

-- The main function chooses between the two implementations of the interfaces
-- depending on a command-line flag.

main :: IO ()
main = do
  args  <- getArgs
  queue <- case args of
             ["--testing"] -> fakeQueue mAX_QUEUE_SIZE
             _otherwise    -> realQueue mAX_QUEUE_SIZE
  service sQLITE_DB_PATH queue

-- The web service is written against the queue interface, it doesn't care which
-- implementation of it we pass it.

service :: FilePath -> QueueI Command -> IO ()
service sqliteDbPath queue = do
  bracket (initDB sqliteDbPath) closeDB $ \conn ->
    withAsync (worker NoBug queue conn) $ \_a -> do
      _ready <- newEmptyMVar
      runFrontEnd NoBug queue _ready pORT

data Bug = NoBug | IgnoreCheckingIfEnqueueSucceeded | DontCatchDequeueError | TooShortWorkerTimeout
  deriving stock Eq

withService :: Bug -> QueueI Command -> IO () -> IO ()
withService bug = withService' bug sQLITE_DB_PATH

withService' :: Bug -> FilePath -> QueueI Command -> IO () -> IO ()
withService' bug sqliteDbPath queue io = do
  bracket (initDB sqliteDbPath) closeDB $ \conn ->
    withAsync (worker bug queue conn) $ \wPid -> do
      link wPid
      ready <- newEmptyMVar
      withAsync (runFrontEnd bug queue ready pORT) $ \fePid -> do
        link fePid
        takeMVar ready
        io

worker :: Bug -> QueueI Command -> Connection -> IO ()
worker bug queue conn = go
  where
    go :: IO ()
    go = do
      mCmd <- qiDequeue queue
                -- BUG: Without this catch the read error fault will cause a crash.
                `catch` (\(err :: IOException) -> if bug == DontCatchDequeueError
                                                  then throwIO err
                                                  else return Nothing)
      case mCmd of
        Nothing -> do
          threadDelay 1000 -- 1 ms
          go
        Just cmd -> do
          exec cmd conn
          go

data Command
  = Write ByteString (MVar Int)
  | Read Int (MVar (Maybe ByteString))
  | Reset (MVar ()) -- For testing.

prettyCommand :: Command -> String
prettyCommand (Write bs _response) = "Write " ++ show bs
prettyCommand (Read ix _response)  = "Read " ++ show ix
prettyCommand (Reset _response)    = "Reset"

exec :: Command -> Connection -> IO ()
exec (Read ix response) conn = do
  bs <- readDB conn ix
  putMVar response bs
exec (Write bs response) conn = do
  ix <- writeDB conn bs
  putMVar response ix
exec (Reset response) conn = do
  resetDB conn
  putMVar response ()

wORKER_TIMEOUT_MICROS :: Bug -> Int
-- BUG: Having a shorter worker timeout will cause the slow read fault to crash the system.
wORKER_TIMEOUT_MICROS TooShortWorkerTimeout = 100_000    -- 0.1s
wORKER_TIMEOUT_MICROS _otherwise            = 30_000_000 -- 30s

httpFrontend :: Bug -> QueueI Command -> Application
httpFrontend bug queue req respond =
  case requestMethod req of
    "GET" -> do
      case parseIndex of
        Nothing ->
          respond (responseLBS status400 [] "Couldn't parse index")
        Just ix -> do
          response <- newEmptyMVar
          success <- qiEnqueue queue (Read ix response)
          if success
          then do
            mMbs <- timeout (wORKER_TIMEOUT_MICROS bug) (takeMVar response)
            case mMbs of
              Just Nothing   -> respond (responseLBS status404 [] (BS8.pack "Not found"))
              Just (Just bs) -> respond (responseLBS status200 [] bs)
              Nothing        -> respond (responseLBS status500 [] (BS8.pack "Internal error"))
          else respond (responseLBS status503 [] "Overloaded")
    "POST" -> do
      bs <- consumeRequestBodyStrict req
      response <- newEmptyMVar
      success <- qiEnqueue queue (Write bs response)
      -- BUG: Ignoring whether the enqueuing operation was successful or not will cause a crash.
      if success || bug == IgnoreCheckingIfEnqueueSucceeded
      then do
        mIx <- timeout (wORKER_TIMEOUT_MICROS bug) (takeMVar response)
        case mIx of
          Just ix -> respond (responseLBS status200 [] (BS8.pack (show ix)))
          Nothing -> respond (responseLBS status500 [] (BS8.pack "Internal error"))
      else respond (responseLBS status503 [] "Overloaded")

    "DELETE" -> do
      response <- newEmptyMVar
      _b <- qiEnqueue queue (Reset response)
      mu <- timeout (wORKER_TIMEOUT_MICROS bug) (takeMVar response)
      case mu of
        Just () -> respond (responseLBS status200 [] (BS8.pack "Reset"))
        Nothing -> respond (responseLBS status500 [] (BS8.pack "Internal error"))
    _otherwise -> do
      respond (responseLBS status400 [] "Invalid method")
  where
    parseIndex :: Maybe Int
    parseIndex = case pathInfo req of
                   [txt] -> case decimal txt of
                     Right (ix, _rest) -> Just ix
                     _otherwise -> Nothing
                   _otherwise   -> Nothing

runFrontEnd :: Bug -> QueueI Command -> MVar () -> Port -> IO ()
runFrontEnd bug queue ready port = runSettings settings (httpFrontend bug queue)
  where
    settings
      = setPort port
      $ setBeforeMainLoop (putMVar ready ())
      $ defaultSettings

------------------------------------------------------------------------

sQLITE_DB_PATH :: FilePath
sQLITE_DB_PATH = "/tmp/part3_webservice.sqlite3"

sQLITE_FLAGS :: [String]
sQLITE_FLAGS = ["fullfsync=1", "journal_mode=WAL", "synchronous=NORMAL"]

sqlitePath :: FilePath -> String
sqlitePath fp =
  let
    flags = map (++ ";") sQLITE_FLAGS
  in
    fp ++ "?" ++ concat flags

initDB :: FilePath -> IO Connection
initDB fp = do
  conn <- open (sqlitePath fp)
  let flags = map (++ ";") sQLITE_FLAGS
  forM_ flags $ \flag -> do
    execute_ conn ("PRAGMA " <> fromString flag)
  resetDB conn
  return conn

resetDB :: Connection -> IO ()
resetDB conn = do
  execute_ conn "DROP TABLE IF EXISTS part3_webservice"
  execute_ conn "CREATE TABLE IF NOT EXISTS part3_webservice (ix INTEGER PRIMARY KEY, value BLOB)"

writeDB :: Connection -> ByteString -> IO Int
writeDB conn bs = do
  execute conn "INSERT INTO part3_webservice (value) VALUES (?)" (Only bs)
  fromIntegral . pred <$> lastInsertRowId conn

readDB :: Connection -> Int -> IO (Maybe ByteString)
readDB conn ix = do
  result <- query conn "SELECT value from part3_webservice WHERE ix = ?" (Only (ix + 1))
  case result of
    [[bs]]     -> return (Just bs)
    _otherwise -> return Nothing

closeDB :: Connection -> IO ()
closeDB = close
