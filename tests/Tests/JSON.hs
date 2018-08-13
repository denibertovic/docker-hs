{-# LANGUAGE OverloadedStrings #-}

module Tests.JSON (tests) where

import           Control.Lens              ((^.))
import           Data.Aeson                ((.=))
import qualified Data.Aeson                as JSON
import           Data.Aeson.Lens           (key, _Null, _Object, _String)
import qualified Data.HashMap.Strict       as HM
import           Data.Text                 (Text)
import           Docker.Client
import           Test.Tasty
import           Test.Tasty.HUnit

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

tests :: TestTree
tests = testGroup "JSON Tests" [
  testExposedPortsJson
  , testVolumesJson
  , testLabelsJson
  , testLogDriverOptionsJson
  , testEntrypointJson
  , testEnvVarJson
  , testNetworkingConfigJson
  ]

main :: IO ()
main = defaultMain tests
