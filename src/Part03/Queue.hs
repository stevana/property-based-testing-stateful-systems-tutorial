module Part03.Queue (module Part03.Queue) where

import Control.Monad
import Data.IORef
import qualified Data.Vector.Mutable as Vec

------------------------------------------------------------------------

-- In order to make the queue more performant and more tricky for ourselves to
-- implement, we'll make it array-backed where elements are not removed from the
-- array when they are dequeued, but rather overwritten by a later enqueue
-- operation.

-- In order to pull this off we'll keep track of the total capacity of the
-- queue, the current size and what the "rear" of the queue is, i.e. the index
-- which we should dequeue next.

data Queue a = Queue
  { qCapacity :: !Int
  , qSize     :: !(IORef Int)
  , qRear     :: !(IORef Int)
  , qQueue    :: !(Vec.IOVector a)
  }

newQueue :: Int -> IO (Queue a)
newQueue cap = Queue <$> pure cap <*> newIORef 0 <*> newIORef 0 <*> Vec.new cap

capacity :: Queue a -> Int
capacity = qCapacity

size :: Queue a -> IO Int
size = readIORef . qSize

-- XXX: Don't export.
rear :: Queue a -> IO Int
rear = readIORef . qRear

clear :: Queue a -> IO ()
clear q = do
  writeIORef (qSize q) 0
  writeIORef (qRear q) 0
  Vec.clear (qQueue q)

isEmpty :: Queue a -> IO Bool
isEmpty q = do
  sz <- size q
  return (sz == 0)

enqueue :: Queue a -> a -> IO Bool
enqueue q x = do
  n <- size q
  if n >= capacity q
  then return False
  else do
    j <- rear q
    Vec.unsafeWrite (qQueue q) ((j + n) `mod` capacity q) x
    modifyIORef' (qSize q) succ
    return True

dequeue :: Queue a -> IO (Maybe a)
dequeue q = do
  empty <- isEmpty q
  if empty
  then return Nothing
  else do
    j <- rear q
    x <- Vec.unsafeRead (qQueue q) j
    modifyIORef' (qSize q) (\sz -> sz - 1)
    modifyIORef' (qRear q) (\j' -> (j' + 1) `mod` capacity q)
    return (Just x)

-- We add a display function for debugging.

display :: Show a => Queue a -> IO ()
display q = do
  putStrLn "Queue"
  putStr "  { capacity = "
  putStrLn (show (capacity q))
  putStr "  , size = "
  sz <- size q
  putStrLn (show sz)
  putStr "  , rear = "
  r <- rear q
  putStrLn (show r)
  putStr "  , queue = "
  putStr "["
  r' <- rear q
  sz' <- size q
  flip mapM_ [r'..sz' - 1] $ \ix -> do
    x <- Vec.unsafeRead (qQueue q) ix
    putStr (show x)
    unless (ix == sz' - 1) $ do
      putStr ", "
  putStrLn "]"

-- If you read this far, hopefully you will appreciate that getting all this
-- right without an off-by-one error somewhere can be a bit tricky...
