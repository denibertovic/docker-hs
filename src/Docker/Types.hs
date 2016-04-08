module Docker.Types where

import           Control.Monad        (mzero)
import           Control.Monad.Catch
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Data.Aeson           (FromJSON, ToJSON, decode, encode,
                                       genericParseJSON, genericToJSON, object,
                                       parseJSON, toJSON, (.:), (.:?), (.=))
import qualified Data.Aeson           as JSON
import           Data.Aeson.Types     (defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (toUpper)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import qualified Network.HTTP.Client  as HTTP
import           Network.HTTP.Types   (StdMethod)
import           Prelude              hiding (all)

type HttpVerb = StdMethod

data Endpoint =
        VersionEndpoint
      | ListContainersEndpoint ListOpts
      | ListImagesEndpoint ListOpts
      | CreateContainerEndpoint
      | StartContainerEndpoint StartOpts ContainerID
      | StopContainerEndpoint Timeout ContainerID
      | KillContainerEndpoint Signal ContainerID
      | RestartContainerEndpoint Timeout ContainerID
      | PauseContainerEndpoint ContainerID
      | UnpauseContainerEndpoint ContainerID
      | ContainerLogsEndpoint ContainerID LogOpts
      | DeleteContainerEndpoint DeleteOpts ContainerID

type URL = Text
type ApiVersion = Text
type ContainerID = Text
type ImageID = Text

data Timeout = Timeout Integer | DefaultTimeout deriving (Eq, Show)

type Request = HTTP.Request
type Response = HTTP.Response BL.ByteString

-- TODO: Add more Signals or use an existing lib
data Signal = SIGHUP
            | SIGINT
            | SIGQUIT
            | SIGSTOP
            | SIGTERM
            | SIGUSR1
            | SIG Integer
            | SIGKILL deriving (Eq, Show)

data DockerClientOpts = DockerClientOpts {
      apiVer  :: ApiVersion
    , baseUrl :: URL
    }

type HttpHandler m = Request -> m Response

type DockerT m a = ReaderT (DockerClientOpts, HttpHandler m) (ExceptT String m) a

runDockerT :: Monad m => (DockerClientOpts, HttpHandler m) -> DockerT m a -> m (Either String a)
runDockerT (opts, h) r = runExceptT $ runReaderT r (opts, h)

data ListOpts = ListOpts { all :: Bool } deriving (Eq, Show)

defaultListOpts = ListOpts { all=False }

defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts {
                  apiVer = "v1.22"
                , baseUrl = "http://127.0.0.1:2375"
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


data ContainerPortInfo = ContainerPortInfo {
                     ipAddressInfo   :: Maybe Text
                   , privatePortInfo :: Port
                   , publicPortInfo  :: Maybe Port
                   , portTypeInfo    :: Maybe PortType
                   } deriving (Eq, Show)

instance FromJSON ContainerPortInfo where
        parseJSON (JSON.Object v) =
            ContainerPortInfo <$> (v .:? "IP")
                <*> (v .:  "PrivatePort")
                <*> (v .:? "PublicPort")
                <*> (v .:? "Type")

-- data NetworkOptions = NetworkOptions {
--                       ipamConfig          :: Maybe Text
--                     , links               :: Maybe Text
--                     , aliases             :: Maybe Text
--                     , networkId           :: Text
--                     , endpointId          :: Text
--                     , gateway             :: Text
--                     , ipAddress           :: Text
--                     , ipPrefixLen         :: Text
--                     , ipV6Gateway         :: Maybe Text
--                     , globalIPv6Address   :: Maybe Text
--                     , globalIPv6PrefixLen :: Maybe Text
--                     , macAddress          :: Text
--                     } deriving (Eq, Show)

-- data Network = Network (M.Map NetworkMode NetworkOptions) deriving (Eq, Show)

-- data NetworkSettings = NetworkSettings {
--                        bridge
--                      , sandboxId
--                      , hairpinMode
--                      , linkLocalIPv6Address
--                      , linkLocalIPv6PrefixLen
--                      , ports
--                      , sandboxKey
--                      , secondaryIPAddresses
--                      , secondaryIPv6Addresses
--                      , endpointID
--                      , gateway
--                      , globalIPv6Address
--                      , globalIPv6PrefixLen
--                      , ipAddress
--                      , ipPrefixLen
--                      , ipv6Gateway
--                      , macAddress
--                      , networks :: [Network]
--                      }

-- data NetworkSettings = NetworkSettings (M.Map Text Text) deriving (Eq, Show, Generic)

-- instance FromJSON NetworkSettings

-- TODO: Add NetworkSettings
data Container = Container
               { containerId        :: ContainerID
               , containerNames     :: [Text]
               , containerImageName :: Text
               , containerImageId   :: ImageID
               , containerCommand   :: Text
               , containerCreatedAt :: Int
               , containerStatus    :: Text
               , containerPorts     :: [ContainerPortInfo]
               , containerLabels    :: Labels
               -- , containerNetworkSettings :: NetworkSettings
               } deriving (Show, Eq)

instance FromJSON Container where
        parseJSON (JSON.Object v) =
            Container <$> (v .: "Id")
                <*> (v .: "Names")
                <*> (v .: "Image")
                <*> (v .: "ImageID")
                <*> (v .: "Command")
                <*> (v .: "Created")
                <*> (v .: "Status")
                <*> (v .: "Ports")
                <*> (v .: "Labels")
                -- <*> (v .: "NetworkSettings")

data Status = Created | Restarting | Running | Paused | Exited | Dead deriving (Eq, Show, Generic)

instance FromJSON Status
instance ToJSON Status

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

data CreateOpts = CreateOpts {
                  containerConfig    :: ContainerConfig
                , hostConfig         :: HostConfig
                , containerResources :: ContainerResources
                } deriving (Eq, Show)

-- detachKeys – Override the key sequence for detaching a container.
-- Format is a single character [a-Z] or ctrl-<value> where <value> is one of: a-z, @, ^, [, , or _.
data DetachKeys = WithCtrl Char | WithoutCtrl Char | DefaultDetachKey deriving (Eq, Show)

data StartOpts = StartOpts { detachKeys :: DetachKeys } deriving (Eq, Show)

defaultStartOpts = StartOpts { detachKeys = DefaultDetachKey }

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

data Device = Device {
              pathOnHost        :: FilePath
            , pathInContainer   :: FilePath
            , cgroupPermissions :: Text
            } deriving (Eq, Show, Generic)

instance ToJSON Device where
    toJSON = genericToJSON defaultOptions {
         fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

instance FromJSON Device where
    parseJSON = genericParseJSON defaultOptions {
            fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

type ContainerName = Text

data VolumeFrom = VolumeFrom ContainerName VolumePermission deriving (Eq, Show)

instance ToJSON Volume where
    toJSON (Volume src dest mode) = toJSON $ case mode of
                        Nothing -> T.concat[src, ":", dest]
                        Just m ->  T.concat[src, ":", dest, ":", (T.pack $ show m)]

data Link = Link Text (Maybe Text) deriving (Eq, Show)

instance ToJSON Link where
    toJSON (Link n1 n2) = toJSON $ case n2 of
                        Nothing -> T.concat[n1, ":", n1] -- use same name in container
                        Just n ->  T.concat[n1, ":", n] -- used specified name in container

-- { "Type": "<driver_name>", "Config": {"key1": "val1"} }
data LogDriverType = JsonFile | Syslog | Journald | Gelf | AwsLogs | Splunk | LoggingDisabled deriving (Eq, Show)

data LogDriverOptions = Map Text Text deriving (Eq, Show)

data LogDriverConfig = LogDriverConfig LogDriverType (Maybe LogDriverOptions) deriving (Eq, Show)

-- TODO: Add container:<name|id> mode
data NetworkMode = Bridge | Host | NetworkDisabled deriving (Eq, Show)

data PortType = TCP | UDP deriving (Eq, Show, Generic)

instance ToJSON PortType where
    toJSON TCP = "tcp"
    toJSON UDP = "udp"

instance FromJSON PortType where
    parseJSON val = return $ case val of
                    "tcp" -> TCP
                    "udp" -> UDP

newtype NetworkInterface = NetworkInterface Text deriving (Eq, Show)

-- { <port>/<protocol>: [{ "HostPort": "<port>"  }] }
data PortBinding = PortBinding {
                   containerPort :: Port
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
-- TODO: Sysctls map[string]string `json:",omitempty"` // List of Namespaced sysctls used for the container
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
                        , devices              :: [Device]
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
