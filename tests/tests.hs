{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either               (rights)
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
import qualified Data.Aeson                as JSON
import           Data.Aeson                ((.=))
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

runDocker f
          -- let opts = defaultClientOpts {baseUrl = "https://127.0.0.1:2376/"}
          -- params' <- clientParamsWithClientAuthentication "127.0.0.1" 2376 "~/.docker/test-client-key.pem" "~/.docker/test-client-cert.pem" >>= fromRight
          -- params <- clientParamsSetCA params' "~/.docker/test-ca.pem"
          -- let settings = mkManagerSettings (TLSSettings params) Nothing
          -- mgr <- newManager settings
 = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) f

testDockerVersion :: IO ()
testDockerVersion =
  runDocker $
  do v <- getDockerVersion
     lift $ assert $ isRight v

testStopNonexisting :: IO ()
testStopNonexisting = runDocker $ do
    res <- stopContainer DefaultTimeout $ fromJust $ toContainerID "thisshouldnotexist"
    lift $ assert $ isLeft res

testFindImage :: IO ()
testFindImage =
  runDocker $
  do images <- listImages defaultListOpts >>= fromRight
     let xs = filter (elem imageFullName . imageRepoTags) images
     lift $ assert $ length xs == 1
  where
    imageFullName = testImageName <> ":latest"

testDeleteImage :: IO ()
testDeleteImage = runDocker $ do
  img <- fmap fromJust findTestImage
  result <- deleteImage defaultImageDeleteOpts (imageId img)
  lift $ assert $ isRight result
  maybeTestImageAfter <- findTestImage
  lift $ assert $ isNothing maybeTestImageAfter
  where
    findTestImage =
      do
        images <- listImages defaultListOpts >>= fromRight
        return $ listToMaybe
               $ filter (elem imageToDeleteFullName . imageRepoTags) images

testListContainers :: IO ()
testListContainers =
  runDocker $
  do containerId <- createContainer (defaultCreateOpts (testImageName <> ":latest")) Nothing
     c <- fromRight containerId
     res <- listContainers $ ListOpts { all=True }
     deleteContainer (ContainerDeleteOpts True True) c
     lift $ assert $ isRight res

testBuildFromDockerfile :: IO ()
testBuildFromDockerfile = do
  cur <- getCurrentDirectory
  let ctxDir = cur ++ "/tests"
  runDocker $ do
    r <- buildImageFromDockerfile (defaultBuildOpts "docker-hs/dockerfile-test") ctxDir
    lift $ assert $ isRight r


testRunAndReadLog :: IO ()
testRunAndReadLog = testRunAndReadLogHelper $ NetworkingConfig HM.empty

testRunAndReadLogWithNetworking :: IO ()
testRunAndReadLogWithNetworking = testRunAndReadLogHelper $ NetworkingConfig $ HM.fromList [("test-network", EndpointConfig ["cellar-door"])]

testRunAndReadLogHelper :: NetworkingConfig -> IO ()
testRunAndReadLogHelper networkingConfig =
  runDocker $
  do let containerConfig = (defaultContainerConfig (testImageName <> ":latest")) {env = [EnvVar "TEST" "123"]}
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

testCreateRemoveNetwork :: IO ()
testCreateRemoveNetwork = do
  runDocker $ do
    createStatus <- createNetwork $ defaultCreateNetworkOpts "test-network"
    lift $ assertBool ("creating a network, unexpected status: " ++ show createStatus) $ isRight createStatus
    nid <- fromRight createStatus
    removeStatus <- removeNetwork nid
    lift $ assertBool ("removing a network, unexpected status: " ++ show removeStatus) $ isRight removeStatus

testLogDriverOptionsJson :: TestTree
testLogDriverOptionsJson = testGroup "Testing LogDriverOptions JSON" [test1, test2, test3]
  where
    test1 = testCase "Driver option 1" $ assert $ JSON.toJSON sample ^. key key1 . _String == val1
    test2 = testCase "Driver option 2" $ assert $ JSON.toJSON sample ^. key key2 . _String == val2
    test3 =
      testCase "Test override" $ assert $ JSON.toJSON sample2 ^. key key1 . _String == "override"
    sample = [LogDriverOption key1 val1, LogDriverOption key2 val2]
    sample2 = [LogDriverOption key1 val1, LogDriverOption key1 "override"]
    key1 = "some-key"
    val1 = "some-val"
    key2 = "some-key2"
    val2 = "some-key2"

testExposedPortsJson :: TestTree
testExposedPortsJson = testGroup "Testing ExposedPorts JSON" [testTCP, testUDP]
  where
    testTCP = testCase "tcp port" $ assert $ JSON.toJSON sampleEP ^. key "80/tcp" . _Object == HM.empty
    testUDP =
      testCase "udp port" $ assert $ JSON.toJSON sampleEP ^. key "1337/tcp" . _Object == HM.empty
    sampleEP = [ExposedPort 80 TCP, ExposedPort 1337 UDP]

testLabelsJson :: TestTree
testLabelsJson = testGroup "Testing Labels JSON" [testLS1, testLS2, testOverride]
  where
    testLS1 = testCase "test label key1" $ assert $ JSON.toJSON sampleLS ^. key key1 . _String == val1
    testLS2 = testCase "test label key2" $ assert $ JSON.toJSON sampleLS ^. key key2 . _String == val2
    testOverride =
      testCase "test label override" $ assert $ JSON.toJSON [Label key1 val1, Label key1 "override"] ^.
      key key1 .
      _String ==
      "override"
    sampleLS = [Label key1 val1, Label key2 val2]
    key1 = "com.example.some-label"
    val1 = "some-value"
    key2 = "something"
    val2 = "value"

testVolumesJson :: TestTree
testVolumesJson = testGroup "Testing Volumes JSON" [testSample1, testSample2]
  where
    testSample1 =
      testCase "Test exposing volume: /tmp" $ assert $ JSON.toJSON sampleVolumes ^. key "/tmp" . _Object ==
      HM.empty
    testSample2 =
      testCase "Test exposing volume: /opt" $ assert $ JSON.toJSON sampleVolumes ^. key "/opt" . _Object ==
      HM.empty
    sampleVolumes = [Volume "/tmp", Volume "/opt"]

testEntrypointJson :: TestTree
testEntrypointJson = testGroup "Testing ContainerConfig JSON" [testSample1, testSample2, testSample3, testSample4, testSample5]
  where
    testSample1 =
      testCase "Test toJSON with empty entrypoint (null)" $ assert $ JSON.toJSON (Entrypoint []) ^. _Null == ()
    testSample2 =
      testCase "Test toJSON with entrypoint as Array" $ assert $
      JSON.toJSON (Entrypoint sampleEntrypointArr) ==
      JSON.toJSON sampleEntrypointArr
    testSample3 =
      testCase "Test decoding entrypoint as string" $ assert $ (JSON.decode "\"cmd\"") ==
      Just (Entrypoint ["cmd"])
    testSample4 =
      testCase "Test decoding as null" $ assert $ (JSON.decode "null" :: Maybe Entrypoint) ==
      Just (Entrypoint [])
    testSample5 =
      testCase "Test decoding as array" $ assert $ (JSON.decode (JSON.encode sampleEntrypointArr) :: Maybe Entrypoint) ==
      Just (Entrypoint sampleEntrypointArr)
    sampleEntrypointArr = ["cmd", "--some-flag", "--some-flag2"]

testEnvVarJson :: TestTree
testEnvVarJson = testGroup "Testing EnvVar JSON" [testSampleEncode, testSampleDecode]
  where
    testSampleEncode =
      testCase "Test toJSON" $ assert $ JSON.toJSON (EnvVar "cellar" "door") == JSON.String "cellar=door"
    testSampleDecode =
      testCase "Test fromJSON" $ assert $ (JSON.decode "\"cellar=door\"" :: Maybe EnvVar) ==
        Just (EnvVar "cellar" "door")

testNetworkingConfigJson :: TestTree
testNetworkingConfigJson = testGroup "Testing NetworkingConfig JSON" [testSampleEncode]
  where
    testSampleEncode =
      let networkingConfig = NetworkingConfig $ HM.fromList [("custom-network", EndpointConfig ["cellar", "door"])]
       in testCase "Test toJSON" $ assert $ JSON.toJSON networkingConfig ==
        JSON.object
          [ "EndpointsConfig" .= JSON.object
            [ "custom-network" .= JSON.object
              [ "Aliases" .= (["cellar", "door"] :: [Text])
              ]
            ]
          ]

integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration Tests"
    [ testCase "Get docker version" testDockerVersion
    , testCase "Build image from Dockerfile" testBuildFromDockerfile
    , testCase "Find image by name" testFindImage
    , testCase "Delete image" testDeleteImage
    , testCase "List containers" testListContainers
    , testCase "Run a dummy container and read its log" testRunAndReadLog
    , testCase "Run a dummy container with networking and read its log" testRunAndReadLogWithNetworking
    , testCase "Try to stop a container that doesn't exist" testStopNonexisting
    , testCase "Create and remove a network" testCreateRemoveNetwork
    ]

jsonTests :: TestTree
jsonTests =
  testGroup
    "JSON Tests"
    [ testExposedPortsJson
    , testVolumesJson
    , testLabelsJson
    , testLogDriverOptionsJson
    , testEntrypointJson
    , testEnvVarJson
    , testNetworkingConfigJson
    ]

setup :: IO ()
setup =
  mapM_ system
        [ "docker pull " ++ unpack imageToDeleteFullName
        , "docker build -t " ++ unpack testImageName ++ " tests"
        ]

isLeft = not . isRight

isRight (Left _)  = False
isRight (Right _) = True

fromRight
  :: (MonadIO m, Show l)
  => Either l r -> m r
fromRight (Left l) = do
  liftIO $ assertFailure $ "Left: " ++ show l
  undefined
fromRight (Right r) = return r

main :: IO ()
main = do
  stackage  <- lookupEnv "RUN_INTEGRATION_TESTS"
  case  stackage of
    Nothing -> defaultMain $ testGroup "Tests" [jsonTests]
    Just _ -> do
      setup
      defaultMain $ testGroup "Tests" [jsonTests, integrationTests]
