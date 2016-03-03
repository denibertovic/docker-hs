module Docker.Types where

import           Control.Monad        (mzero)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson           (FromJSON, ToJSON, encode,
                                       genericParseJSON, genericToJSON, object,
                                       parseJSON, toJSON, (.:), (.=))
import           Data.Aeson.Types     (defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (toUpper)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import qualified Network.HTTP.Client  as HTTP

type Endpoint = Text
type URL = Text
type ApiVersion = Text
type ContainerID = Text
type ImageID = Text
type Timeout = Integer

type Request = HTTP.Request
type Response = HTTP.Response BL.ByteString

data Signal = SIGINT | SIGKILL deriving (Eq, Show)

data DockerClientOpts m = DockerClientOpts {
      http    :: Request -> m Response
    , apiVer  :: ApiVersion
    , baseUrl :: URL
    }

type DockerT m a = ReaderT (DockerClientOpts m) (ExceptT String m) a

runDockerT :: (Monad m) => DockerClientOpts m -> DockerT m a -> m (Either String a)
runDockerT opts a = runExceptT $ runReaderT a opts
-- runDockerT opts a = (runExceptT .) . flip runReaderT opts a

data ListOpts = ListOpts { all :: Bool } deriving (Eq, Show)

defaultClientOpts :: DockerClientOpts IO
defaultClientOpts = DockerClientOpts {
                  http = undefined
                , apiVer = "v1.22"
                , baseUrl = "http://127.0.0.1:2375/"
                }

data DockerVersion = DockerVersion {
                    version       :: Text
                  , apiVersion    :: ApiVersion
                  , gitCommit     :: Text
                  , goVersion     :: Text
                  , os            :: Text
                  , arch          :: Text
                  , kernelVersion :: Text
                  , buildTime     :: Text
                  } deriving (Show, Eq, Generic)


instance ToJSON DockerVersion where
    toJSON = genericToJSON defaultOptions {
         fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

instance FromJSON DockerVersion where
    parseJSON = genericParseJSON defaultOptions {
            fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

data Container = Container

type Digest = Text
type Tag = Text

newtype Labels = Labels (M.Map Text Text) deriving (Eq, Show)

instance FromJSON Labels where
    parseJSON val = Labels <$> parseJSON val

instance ToJSON Labels where
    toJSON (Labels kvs) =  object [k .= v | (k,v) <- (M.toList kvs)]

data Image = DockerImage {
      imageId          :: ImageID
    , imageCreated     :: Integer
    , imageParentId    :: Maybe ImageID
    , imageRepoTags    :: [Tag]
    , imageRepoDigests :: [Digest]
    , imageSize        :: Integer
    , imageVirtualSize :: Integer
    , imageLabels      :: Labels
    } deriving (Show, Eq, Generic)

dropImagePrefix = drop 5

instance FromJSON Image where
    parseJSON = genericParseJSON defaultOptions {
            fieldLabelModifier = dropImagePrefix}

data CreateOpts = CreateOpts
data StartOpts = StartOpts

data DeleteOpts = DeleteOpts {
                  deleteVolumes :: Bool
                , force         :: Bool
                } deriving (Eq, Show)

type Timestamp = Integer
data TailLogOpt = Tail Integer | All deriving (Eq, Show)

data LogOpts = LogOpts {
               follow     :: Bool
             , stdout     :: Bool
             , stderr     :: Bool
             , since      :: Timestamp
             , timestamps :: Bool
             , tail       :: TailLogOpt
             } deriving (Eq, Show)

