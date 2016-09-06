{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Test.QuickCheck.Monadic   as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck     (testProperty)

import           Control.Concurrent        (threadDelay)
import           Control.Lens              ((^.), (^?))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson                as JSON
import           Data.Aeson.Lens           (key, _Object, _String, _Value)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashMap.Strict       as HM
import           Data.Int                  (Int)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust, isJust)
import           Data.Monoid
import           Data.Text                 (Text, unpack)
import           Network.Connection        (TLSSettings (..))
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           System.Process            (system)

import           Docker.Client

-- opts = defaultClientOpts

testImageName = "docker-hs-test"

toStrict1 = B.concat . BL.toChunks

runDocker f = do
    -- let opts = defaultClientOpts {baseUrl = "https://127.0.0.1:2376/"}
    -- params' <- clientParamsWithClientAuthentication "127.0.0.1" 2376 "~/.docker/test-client-key.pem" "~/.docker/test-client-cert.pem" >>= fromRight
    -- params <- clientParamsSetCA params' "~/.docker/test-ca.pem"
    -- let settings = mkManagerSettings (TLSSettings params) Nothing
    -- mgr <- newManager settings
    runDockerT (defaultClientOpts, defaultHttpHandler) f

testDockerVersion :: IO ()
testDockerVersion = runDocker $ do
    v <- getDockerVersion
    lift $ assert $ isRight v

testFindImage :: IO ()
testFindImage = runDocker $ do
        images <- listImages defaultListOpts >>= fromRight
        let xs = filter ((== [imageFullName]) . imageRepoTags) images
        lift $ assert $ length xs == 1
    where imageFullName = testImageName <> ":latest"

testRunAndReadLog :: IO ()
testRunAndReadLog = runDocker $ do
    containerId <- createContainer (defaultCreateOpts (testImageName <> ":latest")) Nothing
    c <- fromRight containerId
    status1 <- startContainer defaultStartOpts c
    _ <- inspectContainer c >>= fromRight
    lift $ threadDelay 300000 -- give 300ms for the application to finish
    lift $ assert $ status1 == Right ()
    status2 <- killContainer SIGTERM c
    logs <- getContainerLogs defaultLogOpts c >>= fromRight
    lift $ assert $ status2 == Right ()
    lift $ assert $ (C.pack "123") `C.isInfixOf` (toStrict1 logs)
    status3 <- deleteContainer (DeleteOpts True True) c
    lift $ assert $ status3 == Right ()

testLogDriverOptionsJson :: TestTree
testLogDriverOptionsJson = testGroup "Testing LogDriverOptions JSON" [ test1, test2, test3 ]
 where
  test1 = testCase "Driver option 1" $ assert $ JSON.toJSON sample ^. key key1 . _String ==  val1
  test2 = testCase "Driver option 2" $ assert $ JSON.toJSON sample ^. key key2 . _String ==  val2
  test3 = testCase "Test override" $ assert $ JSON.toJSON sample2  ^. key key1 . _String ==  "override"
  sample = [LogDriverOption key1 val1, LogDriverOption key2 val2]
  sample2 = [LogDriverOption key1 val1, LogDriverOption key1 "override"]
  key1 = "some-key"
  val1 = "some-val"
  key2 = "some-key2"
  val2 = "some-key2"

testExposedPortsJson :: TestTree
testExposedPortsJson = testGroup "Testing ExposedPorts JSON" [ testTCP, testUDP ]
 where
  testTCP = testCase "tcp port" $ assert $ JSON.toJSON  sampleEP ^. key "80/tcp" . _Object ==  HM.empty
  testUDP = testCase "udp port" $ assert $ JSON.toJSON  sampleEP ^. key "1337/tcp" . _Object == HM.empty
  sampleEP = [ExposedPort 80 TCP, ExposedPort 1337 UDP]

testLabelsJson :: TestTree
testLabelsJson = testGroup "Testing Labels JSON" [ testLS1, testLS2, testOverride ]
 where
  testLS1 = testCase "test label key1" $ assert $ JSON.toJSON  sampleLS ^. key key1 . _String == val1
  testLS2 = testCase "test label key2" $ assert $ JSON.toJSON  sampleLS ^. key key2 . _String == val2
  testOverride = testCase "test label override" $ assert $ JSON.toJSON  [Label key1 val1, Label key1 "override"] ^. key key1 . _String == "override"
  sampleLS = [Label key1 val1, Label key2 val2]
  key1 = "com.example.some-label"
  val1 = "some-value"
  key2 = "something"
  val2 = "value"

testVolumesJson :: TestTree
testVolumesJson = testGroup "Testing Volumes JSON" [ testSample1, testSample2 ]
 where
  testSample1 = testCase "Test exposing volume: /tmp" $ assert $ JSON.toJSON  sampleVolumes ^. key "/tmp" . _Object ==  HM.empty
  testSample2 = testCase "Test exposing volume: /opt" $ assert $ JSON.toJSON  sampleVolumes ^. key "/opt" . _Object ==  HM.empty
  sampleVolumes = [Volume "/tmp", Volume "/opt"]

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests" [
    testCase "Get docker version" testDockerVersion,
    testCase "Find image by name" testFindImage,
    testCase "Run a dummy container and read its log" testRunAndReadLog]

jsonTests :: TestTree
jsonTests = testGroup "JSON Tests" [testExposedPortsJson, testVolumesJson, testLabelsJson, testLogDriverOptionsJson]

setup :: IO ()
setup =  system ("docker build -t "++unpack testImageName++" tests") >> return ()

isRight (Left _) = False
isRight (Right _) = True

fromRight :: (MonadIO m, Show l) => Either l r -> m r
fromRight (Left l) = do
    liftIO $ assertFailure $ "Left: " ++ show l
    undefined
fromRight (Right r) = return r

main :: IO ()
main = do
  setup
  defaultMain $ testGroup "Tests" [jsonTests, integrationTests]

