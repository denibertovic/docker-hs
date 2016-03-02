module Docker.Types where

import           Data.Aeson       (FromJSON, ToJSON, genericParseJSON,
                                   genericToJSON, parseJSON, toJSON)
import           Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import           Data.Char        (toUpper)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)

type URL = Text
type ApiVersion = Text

data DockerClientOpts = DockerClientOpts {
      apiVersion :: ApiVersion
    , baseUrl    :: URL
    } deriving (Show)

defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts
                { apiVersion = "v1.22"
                , baseUrl = "http://127.0.0.1:2375/"
                }

data DockerVersion = DockerVersion
                  { version       :: Text
                  , gitCommit     :: Text
                  , goVersion     :: Text
                  , arch          :: Text
                  , kernelVersion :: Text
                  } deriving (Show, Eq, Generic)


instance ToJSON DockerVersion where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

instance FromJSON DockerVersion where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

