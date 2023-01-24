{-# LANGUAGE DerivingStrategies #-}

module Part03.QueueTest (module Part03.QueueTest) where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Part03.Queue

------------------------------------------------------------------------

-- The model of our queue will use a simple immutable linked list rather than
-- the complicated mutable array.

data Model a = Model
  { mCapacity :: Int
  , mQueue    :: [a]
  }
  deriving stock Show

newModel :: Int -> Model a
newModel cap = Model cap []

data Command a
  = Size
  | Clear
  | IsEmpty
  | Enqueue a
  | Dequeue
  deriving stock Show

prettyCommand :: Command a -> String
prettyCommand Size       = "Size"
prettyCommand Clear      = "Clear"
prettyCommand IsEmpty    = "IsEmpty"
prettyCommand Enqueue {} = "Enqueue"
prettyCommand Dequeue    = "Dequeue"

data Response a
  = Int Int
  | Unit ()
  | Maybe (Maybe a)
  | Bool Bool
  deriving stock (Show, Eq)

-- Enqueuing merely appends to the back of the list.

fakeEnqueue :: a -> Model a -> (Model a, Bool)
fakeEnqueue x m
  | length (mQueue m) >= mCapacity m = (m, False)
  | otherwise = (m { mQueue = mQueue m ++ [x]}, True)

-- While dequeuing merely returns the head of the list.

fakeDequeue :: Model a -> (Model a, Maybe a)
fakeDequeue m = case mQueue m of
  []       -> (m, Nothing)
  (x : xs) -> (m { mQueue = xs }, Just x)

step :: Command a -> Model a -> (Model a, Response a)
step Size        m = (m, Int (length (mQueue m)))
step Clear       m = (m { mQueue = [] }, Unit ())
step IsEmpty     m = (m, Bool (null (mQueue m)))
step (Enqueue x) m = Bool  <$> fakeEnqueue x m
step Dequeue     m = Maybe <$> fakeDequeue m

exec :: Command a -> Queue a -> IO (Response a)
exec Size             q = Int   <$> size q
exec Clear            q = Unit  <$> clear q
exec IsEmpty          q = Bool  <$> isEmpty q
exec (Enqueue x)      q = Bool  <$> enqueue q x
exec Dequeue          q = Maybe <$> dequeue q

genCommand :: Arbitrary a => Int -> Int -> Gen (Command a)
genCommand _cap _sz = frequency
  [ (3, pure Size)
  , (0, pure Clear) -- NOTE: If this happens too often, it causing enqueue to
                    -- rarely write to a full queue.
  , (2, pure IsEmpty)
  , (5, Enqueue <$> arbitrary)
  , (2, pure Dequeue)
  ]

genCommands :: Arbitrary a => Int -> Int -> Gen [Command a]
genCommands cap sz0 = sized (go sz0)
  where
    go _sz 0 = return []
    go sz  n = do
      cmd <- genCommand cap sz
      let sz' = case cmd of
                  Size       -> sz
                  Clear      -> 0
                  IsEmpty    -> sz
                  Enqueue {} -> sz + 1
                  Dequeue    -> sz - 1
      cmds <- go sz' (n - 1)
      return (cmd : cmds)

newtype Capacity = Capacity Int
  deriving stock Show

instance Arbitrary Capacity where
  arbitrary = Capacity <$> choose (0, 5)

-- The tests that ensure that the model is faithful to the real queue
-- implementation also doubles as contract tests once we turn or model into a
-- fake.

prop_contractTests :: Capacity -> Property
prop_contractTests (Capacity cap) =
  forAllShrink (genCommands cap 0) (shrinkList (const [])) $ \cmds -> monadicIO $ do
    let m = newModel cap
    q <- run (newQueue cap)
    monitor (tabulate "Commands" (map prettyCommand cmds))
    (result, hist) <- go cmds m q []
    mapM_ (monitor . classify') (zip cmds hist)
    return result
    where
      go :: [Command Int] -> Model Int -> Queue Int -> [Response Int] -> PropertyM IO (Bool, [Response Int])
      go []          _m _q hist = return (True, reverse hist)
      go (cmd : cmds) m  q hist = do
        let (m', resp) = step cmd m
        resp' <- run (exec cmd q)
        unless (resp == resp') $
          monitor (counterexample (show resp ++ " /= " ++ show resp'))
        go cmds m' q (resp : hist)

      classify' :: (Command a, Response a) -> Property -> Property
      classify' (Enqueue {},     Bool b) = classify b "enqueue successful"
      classify' (_, _)                   = id
