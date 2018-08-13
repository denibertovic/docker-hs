{-# LANGUAGE OverloadedStrings #-}

module Tests.Integration (tests, runDockerHTTP, runDockerUnix) where

import           Data.Either               (rights)
import           Data.Maybe
import           Prelude                   hiding (all)
import qualified Test.QuickCheck.Monadic   as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck     (testProperty)

import           Control.Concurrent        (threadDelay)
import           Control.Lens              ((^.), (^?))
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson                ((.=))
import qualified Data.Aeson                as JSON
import           Data.Aeson.Lens           (key, _Array, _Null, _Object,
                                            _String, _Value)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashMap.Strict       as HM
import           Data.Int                  (Int)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust, isJust, isNothing, listToMaybe)
import           Data.Monoid
import           Data.Text                 (Text, unpack)
import qualified Data.Vector               as V
import           Network.Connection        (TLSSettings (..))
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           System.Directory          (getCurrentDirectory)
import           System.Environment        (lookupEnv)
import           System.Process            (system)

import           Docker.Client

-- opts = defaultClientOpts
testImageName = "docker-hs/test"
imageToDeleteFullName = "hello-world:latest"

toStrict1 = B.concat . BL.toChunks

type RunDockerCmd = DockerT IO () -> IO ()

runDockerHTTP :: RunDockerCmd
runDockerHTTP f = do
  -- let opts = defaultClientOpts {baseUrl = "https://127.0.0.1:2376/"}
  -- params' <- clientParamsWithClientAuthentication "127.0.0.1" 2376 "~/.docker/test-client-key.pem" "~/.docker/test-client-cert.pem" >>= fromRight
  -- params <- clientParamsSetCA params' "~/.docker/test-ca.pem"
  -- let settings = mkManagerSettings (TLSSettings params) Nothing
  -- mgr <- newManager settings
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) f

runDockerUnix :: RunDockerCmd
runDockerUnix cmd = do
  h <- unixHttpHandler "/var/run/docker.sock"
  runDockerT (defaultClientOpts, h) cmd

testDockerVersion :: RunDockerCmd -> IO ()
testDockerVersion runDocker = runDocker $ do
  v <- getDockerVersion
  lift $ assert $ isRight v

testStopNonexisting :: RunDockerCmd -> IO ()
testStopNonexisting runDocker = runDocker $ do
  res <- stopContainer DefaultTimeout $ fromJust $ toContainerID "thisshouldnotexist"
  lift $ assert $ isLeft res

testFindImage :: RunDockerCmd -> IO ()
testFindImage runDocker = runDocker $ do
  images <- listImages defaultListOpts >>= fromRight
  let xs = filter (elem imageFullName . imageRepoTags) images
  lift $ assert $ length xs == 1

  where imageFullName = testImageName <> ":latest"

testDeleteImage :: RunDockerCmd -> IO ()
testDeleteImage runDocker = runDocker $ do
  maybeTestImage <- findTestImage
  let img = fromMaybe (error "Couldn't find test image") maybeTestImage

  result <- deleteImage defaultImageDeleteOpts (imageId img)
  lift $ assert $ isRight result
  maybeTestImageAfter <- findTestImage
  lift $ assert $ isNothing maybeTestImageAfter
  where
    findTestImage = do
      images <- listImages defaultListOpts >>= fromRight
      return $ listToMaybe
             $ filter (elem imageToDeleteFullName . imageRepoTags) images

testListContainers :: RunDockerCmd -> IO ()
testListContainers runDocker = runDocker $ do
  containerId <- createContainer (defaultCreateOpts (testImageName <> ":latest")) Nothing
  c <- fromRight containerId
  res <- listContainers $ ListOpts { all=True }
  deleteContainer (ContainerDeleteOpts True True) c
  lift $ assert $ isRight res

testBuildFromDockerfile :: RunDockerCmd -> IO ()
testBuildFromDockerfile runDocker = do
  cur <- getCurrentDirectory
  let ctxDir = cur ++ "/tests"
  runDocker $ do
    r <- buildImageFromDockerfile (defaultBuildOpts "docker-hs/dockerfile-test") ctxDir
    lift $ assert $ isRight r


testRunAndReadLog :: RunDockerCmd -> IO ()
testRunAndReadLog runDocker = testRunAndReadLogHelper runDocker $ NetworkingConfig HM.empty

testRunAndReadLogWithNetworking :: RunDockerCmd -> IO ()
testRunAndReadLogWithNetworking runDocker = testRunAndReadLogHelper runDocker $ NetworkingConfig $ HM.fromList [("test-network", EndpointConfig ["cellar-door"])]

testRunAndReadLogHelper :: RunDockerCmd -> NetworkingConfig -> IO ()
testRunAndReadLogHelper runDocker networkingConfig = runDocker $ do
  let containerConfig = (defaultContainerConfig (testImageName <> ":latest")) {env = [EnvVar "TEST" "123"]}
  createdNetworks <- rights <$> mapM createNetworkWithName networkNames
  containerId <- createContainer (CreateOpts containerConfig defaultHostConfig networkingConfig) Nothing
  c <- fromRight containerId
  status1 <- startContainer defaultStartOpts c
  _ <- inspectContainer c >>= fromRight
  lift $ threadDelay 300000 -- give 300ms for the application to finish
  lift $ assertBool ("starting the container, unexpected status: " ++ show status1) $ isRightUnit status1
  logs <- getContainerLogs defaultLogOpts c >>= fromRight
  lift $ assert $ (C.pack "123") `C.isInfixOf` (toStrict1 logs)
  status3 <- deleteContainer (ContainerDeleteOpts True True) c
  lift $ assertBool ("deleting container, unexpected status: " ++ show status3) $ isRightUnit status3
  mapM_ removeNetwork createdNetworks

  where
    isRightUnit (Right ()) = True
    isRightUnit _          = False
    networkNames = HM.keys $ endpointsConfig networkingConfig
    createNetworkWithName name = createNetwork $
      (defaultCreateNetworkOpts name) { createNetworkCheckDuplicate = True }

testCreateInspectRemoveNetwork :: RunDockerCmd -> IO ()
testCreateInspectRemoveNetwork runDocker = runDocker $ do
  createStatus <- createNetwork $ defaultCreateNetworkOpts "test-network"
  lift $ assertBool ("creating a network, unexpected status: " ++ show createStatus) $ isRight createStatus
  nid <- fromRight createStatus

  networkDefinition <- inspectNetwork nid
  lift $ assertBool ("inspecting a network, unexpected NetworkDefinition: " ++ show networkDefinition) $ isRight networkDefinition

  removeStatus <- removeNetwork nid
  lift $ assertBool ("removing a network, unexpected status: " ++ show removeStatus) $ isRight removeStatus

tests :: RunDockerCmd -> TestTree
tests runDocker = testGroup "Integration Tests" [
  testCase "Get docker version" (testDockerVersion runDocker)
  , testCase "Build image from Dockerfile" (testBuildFromDockerfile runDocker)
  , testCase "Find image by name" (testFindImage runDocker)
  , testCase "Delete image" (testDeleteImage runDocker)
  , testCase "List containers" (testListContainers runDocker)
  , testCase "Run a dummy container and read its log" (testRunAndReadLog runDocker)
  , testCase "Run a dummy container with networking and read its log" (testRunAndReadLogWithNetworking runDocker)
  , testCase "Try to stop a container that doesn't exist" (testStopNonexisting runDocker)
  , testCase "Create, inspect, and remove a network" (testCreateInspectRemoveNetwork runDocker)
  ]

setup :: IO ()
setup = mapM_ system [ "docker pull " ++ unpack imageToDeleteFullName
                     , "docker build -t " ++ unpack testImageName ++ " tests"]

isLeft :: Either a b -> Bool
isLeft = not . isRight

isRight :: Either a b -> Bool
isRight (Left _)  = False
isRight (Right _) = True

fromRight :: (MonadIO m, Show l) => Either l r -> m r
fromRight (Left l) = do
  liftIO $ assertFailure $ "Left: " ++ show l
  undefined
fromRight (Right r) = return r

main :: IO ()
main = do
  useUnixSocket <- lookupEnv "DOCKER_UNIX_SOCKET"
  let runDocker = if isJust useUnixSocket then runDockerUnix else runDockerHTTP
  defaultMain $ tests runDocker