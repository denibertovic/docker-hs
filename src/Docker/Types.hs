module Docker.Types where

import           Control.Monad        (mzero)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson           (FromJSON, ToJSON, decode, encode,
                                       genericParseJSON, genericToJSON, object,
                                       parseJSON, toJSON, (.:), (.=))
import qualified Data.Aeson           as JSON
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
newtype Labels = Labels (M.Map Name Value) deriving (Eq, Show)

instance FromJSON Labels where
    parseJSON val = Labels <$> parseJSON val

instance ToJSON Labels where
    toJSON (Labels kvs) =  object [k .= v | (k,v) <- (M.toList kvs)]

type Tag = Text

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

data VolumePermission = Rw | Ro deriving (Eq, Show, Generic)

instance ToJSON VolumePermission
instance FromJSON VolumePermission

data Volume = Volume { hostSrc          :: Text
                     , containerDest    :: Text
                     , volumePermission :: Maybe VolumePermission
                     } deriving (Eq, Show)

type ContainerName = Text

data VolumeFrom = VolumeFrom ContainerName VolumePermission deriving (Eq, Show)

instance ToJSON Volume where
    toJSON (Volume src dest mode) = toJSON $ case mode of
                        Nothing -> T.concat[src, ":", dest]
                        Just m ->  T.concat[src, ":", dest, ":", (T.pack $ show m)]

data Link = Link Text (Maybe Text) deriving (Eq, Show)

instance ToJSON Link where
    toJSON (Link n1 n2) = toJSON $ case n2 of
                        Nothing -> T.concat[n1, ":", n1]
                        Just n ->  T.concat[n1, ":", n]

-- { "Type": "<driver_name>", "Config": {"key1": "val1"} }
data LogDriverType = JsonFile | Syslog | Journald | Gelf | AwsLogs | Splunk | LoggingDisabled deriving (Eq, Show)

data LogDriverOptions = Map Text Text deriving (Eq, Show)

data LogDriverConfig = LogDriverConfig LogDriverType (Maybe LogDriverOptions) deriving (Eq, Show)

-- TODO: Add container:<name|id> mode
data NetworkMode = Bridge | Host | NetworkDisabled deriving (Eq, Show)

data PortType = TCP | UDP deriving (Eq, Show)

newtype NetworkInterface = NetworkInterface Text deriving (Eq, Show)

-- { <port>/<protocol>: [{ "HostPort": "<port>"  }] }
data PortBinding = PortBinding {
                   containerPort :: Integer
                 , portType      :: PortType
                 , hostPort      :: [HostPort]
                 } deriving (Eq, Show)

data HostPort = HostPort NetworkInterface Port deriving (Eq, Show)

-- { "Name": "on-failure" , "MaximumRetryCount": 2}
type RetryCount = Integer
data RestartPolicy = RestartAlways | RestartUnlessStopped | RestartOnFailure RetryCount | RestartOff  deriving (Eq, Show)

data Isolation = Default | Process | Hyperv  deriving (Eq, Show)

newtype UTSMode = UTSMode Text deriving (Eq, Show)

-- TODO: Add Tmpfs : List of tmpfs (mounts) used for the container
-- TODO: Add UTSMode : UTS namespace to use for the container
data HostConfig = HostConfig
                { binds           :: [Volume]
                , containerIDFile :: FilePath
                , logConfig       :: LogDriverConfig
                , networkMode     :: NetworkMode
                , portBindings    :: [PortBinding]
                , restartPolicy   :: RestartPolicy
                , volumeDriver    :: Text
                , volumesFrom     :: [VolumeFrom]
                , capAdd          :: [Text]
                , capDrop         :: [Text]
                , dns             :: [Text]
                , dnsOptions      :: [Text]
                , dnsSearch       :: [Text]
                , extraHosts      :: [Text]
                , groupAdd        :: [Integer]
                , ipcMode         :: Text
                , links           :: [Link]
                , oomScoreAdj     :: Integer
                , pidMode         :: Text
                , privileged      :: Bool
                , publishAllPorts :: Bool
                , readOnlyRootfs  :: Bool
                , securityOpt     :: [Text]
                , storageOpt      :: [Text]
                , utsMode         :: UTSMode
                , shmSize         :: Integer
                , consoleSize     :: Integer
                , isolation       :: Isolation
                , resources       :: ContainerResources
                } deriving (Eq, Show, Generic)

-- { "Name": <name>, "Soft": <soft limit>, "Hard": <hard limit>  }
-- { "Name": "nofile", "Soft": 1024, "Hard": 2048  }
data Ulimit = Ulimit {
              ulimitName :: Text
            , ulimitSoft :: Integer
            , ulimitHard :: Integer
            } deriving (Eq, Show)

data ContainerResources = ContainerResources {
                          cpuShares            :: Integer
                        , cgroupParent         :: Text
                        , blkioWeight          :: Text
                        , blkioWeightDevice    :: Text
                        , blkioDeviceReadBps   :: Text
                        , blkioDeviceWriteBps  :: Text
                        , blkioDeviceReadIOps  :: Text
                        , blkioDeviceWriteIOps :: Text
                        , cpuPeriod            :: Integer
                        , cpuQuota             :: Integer
                        , cpusetCpus           :: Integer
                        , cpusetMems           :: Text
                        , devices              :: [Volume]
                        , diskQuota            :: Integer
                        , kernelMemory         :: Integer
                        , memory               :: Integer
                        , memoryReservation    :: Integer
                        , memorySwap           :: Integer
                        , memorySwappiness     :: Integer
                        , oomKillDisable       :: Bool
                        , pidsLimit            :: Integer
                        , ulimits              :: [Ulimit]
                        } deriving (Eq, Show)

type Port = Integer

type Name = Text
type Value = Text

data EnvVar = EnvVar Name Value deriving (Eq, Show)

data ContainerConfig = ContainerConfig {
                       hostname        :: Text
                     , domainname      :: Text
                     , user            :: Text
                     , attachStdin     :: Bool
                     , attachStdout    :: Bool
                     , attachStderr    :: Bool
                     -- ExposedPorts are used for the PublishAllPorts flag
                     -- to know which ports to publish. Also it's used for
                     -- the deamon to know wich Environment variables to
                     -- inject into a container linking to our container.
                     -- Example linking a Postgres container named db:
                     -- DB_PORT_5432_TCP_PORT="5432"
                     -- DB_PORT_5432_TCP_PROTO="tcp"
                     -- DB_PORT_5432_TCP="tcp://172.17.0.1:5432"
                     , exposedPorts    :: [Port]
                     , publishService  :: Text
                     , tty             :: Bool
                     , openStdin       :: Bool
                     , stdinOnce       :: Bool
                     , env             :: [EnvVar]
                     , cmd             :: Text
                     , argsEscaped     :: Bool
                     , image           :: Text
                     , volumes         :: [Volume]
                     , workingDir      :: FilePath
                     , entrypoint      :: Text
                     , networkDisabled :: Bool
                     , macAddress      :: Text
                     , onBuild         :: Text
                     , labels          :: Labels
                     , stopSignal      :: Signal
                     } deriving (Eq, Show)
