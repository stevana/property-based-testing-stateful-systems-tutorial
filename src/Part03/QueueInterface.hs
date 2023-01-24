module Part03.QueueInterface (module Part03.QueueInterface) where

-- The queue interface supports enqueing, where a bool is returned indicating if
-- the queue is full or not, and dequeuing, where we get an optional value out
-- to cover the case of the queue being empty.

data QueueI a = QueueI
  { qiEnqueue :: a -> IO Bool
  , qiDequeue :: IO (Maybe a)
  }
