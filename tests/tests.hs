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
    let x = filter ((== [testImageName<>":latest"]) . imageRepoTags) images
    lift $ assert $ length x == 1

testRunAndReadLog :: IO ()
testRunAndReadLog = runDocker $ do
    containerId <- createContainer (defaultCreateOpts (testImageName <> ":latest"))
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


testExposedPortsJson :: TestTree
testExposedPortsJson = testGroup "Testing ExposedPorts JSON" [ testTCP, testUDP ]
 where
  testTCP = testCase "tcp port" $ assert $ JSON.toJSON  sampleEP ^. key "80/tcp" . _Object ==  HM.empty
  testUDP = testCase "udp port" $ assert $ JSON.toJSON  sampleEP ^. key "1337/tcp" . _Object == HM.empty
  sampleEP = ExposedPorts $ M.fromList [(80, TCP), (1337, UDP)]

testVolumesJson :: TestTree
testVolumesJson = testGroup "Testing Volumes JSON" [ testSample1, testSample2 ]
 where
  testSample1 = testCase "Test exposing volume: /tmp" $ assert $ JSON.toJSON  sampleVolumes ^. key "/tmp" . _Object ==  HM.empty
  testSample2 = testCase "Test exposing volume: /opt" $ assert $ JSON.toJSON  sampleVolumes ^. key "/opt" . _Object ==  HM.empty
  sampleVolumes = Volumes ["/tmp", "/opt"]

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests" [
    testCase "Get docker version" testDockerVersion,
    testCase "Find image by name" testFindImage,
    testCase "Run a dummy container and read its log" testRunAndReadLog]

jsonTests :: TestTree
jsonTests = testGroup "JSON Tests" [testExposedPortsJson, testVolumesJson]

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

