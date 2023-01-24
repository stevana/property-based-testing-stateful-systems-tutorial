{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Part03.ServiceTest (module Part03.ServiceTest) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (permutations)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.HTTP.Client
       ( Manager
       , RequestBody(RequestBodyLBS)
       , brConsume
       , defaultManagerSettings
       , httpLbs
       , method
       , newManager
       , parseRequest
       , path
       , requestBody
       , responseBody
       , throwErrorStatusCodes
       , withResponse
       )
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- We can reuse most of the concurrent testing machinary from the second
-- part.

import Part02ConcurrentSMTesting
       ( History'(History)
       , Operation'(Ok)
       , appendHistory
       , assertWithFail
       , classifyCommandsLength
       , interleavings
       , linearisable
       , prettyHistory
       , toPid
       )
import Part03.Service

------------------------------------------------------------------------

newtype Index = Index Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

data ClientRequest = WriteReq ByteString | ReadReq Index
  deriving stock Show

data ClientResponse = WriteResp Index | ReadResp ByteString
  deriving stock (Eq, Show)

newtype ConcProgram = ConcProgram { unConcProgram :: [[ClientRequest]] }
  deriving stock Show

newtype Model = Model (Vector ByteString)

initModel :: Model
initModel = Model Vector.empty

step :: Model -> ClientRequest -> (Model, ClientResponse)
step (Model vec) (WriteReq bs) =
  (Model (Vector.snoc vec bs), WriteResp (Index (Vector.length vec)))
step (Model vec) (ReadReq (Index ix)) =
  (Model vec, ReadResp (vec Vector.! ix))

type Operation = Operation' ClientRequest ClientResponse

concExec :: Manager -> TQueue Operation -> ClientRequest -> IO ()
concExec mgr hist req =
  case req of
    WriteReq bs -> do
      ix <- httpWrite mgr bs
      pid <- toPid <$> myThreadId
      appendHistory hist (Ok pid (WriteResp ix))
    ReadReq ix -> do
      bs <- httpRead mgr ix
      pid <- toPid <$> myThreadId
      appendHistory hist (Ok pid (ReadResp bs))

------------------------------------------------------------------------

httpWrite :: Manager -> ByteString -> IO Index
httpWrite mgr bs = do
  initReq <- parseRequest ("http://localhost:" ++ show pORT)
  let req = initReq { method = "POST"
                    , requestBody = RequestBodyLBS bs
                    }
  withResponse req mgr $ \resp -> do
    throwErrorStatusCodes req resp
    bss <- brConsume (responseBody resp)
    case LBS8.readInt (LBS8.fromChunks bss) of
      Nothing          -> error "httpWrite: can't read index"
      Just (ix, _rest) -> return (Index ix)

httpRead :: Manager -> Index -> IO ByteString
httpRead mgr (Index ix) = do
  initReq <- parseRequest ("http://localhost:" ++ show pORT)
  let req = initReq { method = "GET"
                    , path = path initReq <> BS8.pack (show ix)
                    }
  withResponse req mgr $ \resp -> do
    throwErrorStatusCodes req resp
    bss <- brConsume (responseBody resp)
    return (LBS8.fromChunks bss)

httpReset :: Manager -> IO ()
httpReset mgr = do
  initReq <- parseRequest ("http://localhost:" ++ show pORT)
  _resp <- httpLbs initReq { method = "DELETE" } mgr
  return ()

------------------------------------------------------------------------

genClientRequest :: Gen ClientRequest
genClientRequest = oneof
  [ WriteReq <$> (LBS.pack <$> arbitrary)
  , ReadReq <$> (Index <$> arbitrary)
  ]

validProgram :: Model -> [ClientRequest] -> Bool
validProgram _model _cmds = True

shrinkClientRequest :: ClientRequest -> [ClientRequest]
shrinkClientRequest (WriteReq bs) = [ WriteReq (LBS.pack s') | s' <- shrink (LBS.unpack bs) ]
shrinkClientRequest (ReadReq _ix) = []

genConcProgram :: Model -> Gen ConcProgram
genConcProgram m0 = sized (go m0 [])
  where
    go :: Model -> [[ClientRequest]] -> Int -> Gen ConcProgram
    go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
                | otherwise = do
                    n <- chooseInt (2, 5)
                    reqs <- vectorOf n genClientRequest `suchThat` concSafe m
                    go (advanceModel m reqs) (reqs : acc) (sz - n)

advanceModel :: Model -> [ClientRequest] -> Model
advanceModel m reqs = foldl (\ih req -> fst (step ih req)) m reqs

concSafe :: Model -> [ClientRequest] -> Bool
concSafe m = all (validProgram m) . permutations

validConcProgram :: Model -> ConcProgram -> Bool
validConcProgram m0 (ConcProgram reqss0) = go m0 True reqss0
  where
    go :: Model -> Bool -> [[ClientRequest]] -> Bool
    go _m False _              = False
    go _m acc   []             = acc
    go  m _acc  (reqs : reqss) = go (advanceModel m reqs) (concSafe m reqs) reqss

shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
shrinkConcProgram m
  = filter (validConcProgram m)
  . map ConcProgram
  . filter (not . null)
  . shrinkList (shrinkList shrinkClientRequest)
  . unConcProgram

prettyConcProgram :: ConcProgram -> String
prettyConcProgram = show

forAllConcProgram :: (ConcProgram -> Property) -> Property
forAllConcProgram k =
  forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
  where
    m = initModel

-- Finally we can write our integration tests between the queue and the web
-- service, sometimes these tests are also called "collaboration tests".

-- NOTE: We shouldn't use a model in concurrent tests before we made sure it
-- passes sequential tests.

-- NOTE: Assumes that the service is running.
prop_integrationTests :: Manager -> Property
prop_integrationTests mgr = mapSize (min 20) $
  forAllConcProgram $ \(ConcProgram reqss) -> monadicIO $ do
    monitor (classifyCommandsLength (concat reqss))
    monitor (tabulate "Client requests" (map constructorString (concat reqss)))
    monitor (tabulate "Number of concurrent client requests" (map (show . length) reqss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    replicateM_ 10 $ do
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec mgr queue)) reqss)
      hist <- History <$> run (atomically (flushTQueue queue))
      assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
      run (httpReset mgr)
  where
    constructorString :: ClientRequest -> String
    constructorString WriteReq {} = "WriteReq"
    constructorString ReadReq  {} = "ReadReq"

test :: IO ()
test = do
  -- NOTE: fake queue is used here, justified by previous contract testing.
  queue <- fakeQueue mAX_QUEUE_SIZE
  mgr   <- newManager defaultManagerSettings
  withService NoBug queue (quickCheck (prop_integrationTests mgr))
