{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Client.Types (
      Endpoint(..)
    , URL
    , ApiVersion
    , ContainerID
    , fromContainerID
    , toContainerID
    , ImageID
    , fromImageID
    , toImageID
    , Timeout(..)
    , Signal(..)
    , ContainerDetails(..)
    , DockerClientOpts(..)
    , defaultClientOpts
    , ListOpts(..)
    , defaultListOpts
    , DockerVersion(..)
    , ContainerPortInfo(..)
    , Container(..)
    , ContainerState(..)
    , Status(..)
    , Digest
    , Label(..)
    , Tag
    , Image(..)
    , dropImagePrefix
    , CreateOpts(..)
    , defaultCreateOpts
    , DetachKeys(..)
    , StartOpts(..)
    , defaultStartOpts
    , DeleteOpts(..)
    , defaultDeleteOpts
    , Timestamp
    , TailLogOpt(..)
    , LogOpts(..)
    , defaultLogOpts
    , VolumePermission(..)
    , Bind(..)
    , Volume(..)
    , Device(..)
    , ContainerName
    , VolumeFrom(..)
    , Link(..)
    , LogDriverType(..)
    , LogDriverOption(..)
    , LogDriverConfig(..)
    , NetworkMode(..)
    , PortType(..)
--    , NetworkInterface(..)
    , Network(..)
    , NetworkSettings(..)
    , NetworkOptions(..)
    , Mount(..)
    , PortBinding(..)
    , HostPort(..)
    , RetryCount
    , RestartPolicy(..)
    , Isolation(..)
    , UTSMode(..)
    , HostConfig(..)
    , defaultHostConfig
    , Ulimit(..)
    , ContainerResources(..)
    , defaultContainerResources
    , Port
    , Name
    , Value
    , EnvVar(..)
    , ContainerConfig(..)
    , defaultContainerConfig
    , ExposedPort(..)
    , DeviceWeight(..)
    , DeviceRate(..)
    , addPortBinding
    , addExposedPort
    , addBind
    , setCmd
    , addLink
    , addVolume
    , addVolumeFrom
    , MemoryConstraint(..)
    , MemoryConstraintSize(..)
    ) where

import           Data.Aeson          (FromJSON, ToJSON, genericParseJSON,
                                      genericToJSON, object, parseJSON, toJSON,
                                      (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson          as JSON
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import           Data.Char           (isAlphaNum, toUpper)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid         ((<>))
import           Data.Scientific     (floatingOrInteger)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics        (Generic)
import           Prelude             hiding (all, tail)
import           Text.Read           (readMaybe)

-- | List of Docker Engine API endpoints
data Endpoint =
        VersionEndpoint
      | ListContainersEndpoint ListOpts
      | ListImagesEndpoint ListOpts
      | CreateContainerEndpoint CreateOpts (Maybe ContainerName)
      | StartContainerEndpoint StartOpts ContainerID
      | StopContainerEndpoint Timeout ContainerID
      | KillContainerEndpoint Signal ContainerID
      | RestartContainerEndpoint Timeout ContainerID
      | PauseContainerEndpoint ContainerID
      | UnpauseContainerEndpoint ContainerID
      | ContainerLogsEndpoint LogOpts Bool ContainerID -- ^ Second argument (Bool) is whether to follow which is currently hardcoded to False.
      -- See note in 'Docker.Client.Api.getContainerLogs' for explanation why.
      | DeleteContainerEndpoint DeleteOpts ContainerID
      | InspectContainerEndpoint ContainerID
    deriving (Eq, Show)

-- | We should newtype this
type URL = Text

-- | We should newtype this
type ApiVersion = Text


-- | ID of a contianer
newtype ContainerID = ContainerID Text
    deriving (Eq, Show)

-- | Used for extracting the id of the container from the newtype
fromContainerID :: ContainerID -> Text
fromContainerID (ContainerID t) = t

-- | Used for parsing a Text value into a ContainerID. We apply some basic
-- validation here.
toContainerID :: Text -> Maybe ContainerID
toContainerID t =
    if T.all (\c -> isAlphaNum c || c == ':') t then -- Note: Can we improve this whitelist?
        Just $ ContainerID t
    else
        Nothing

-- ID of an image.
newtype ImageID = ImageID Text
    deriving (Eq, Show)

-- | Used for extracting the id of the image from the newtype.
fromImageID :: ImageID -> Text
fromImageID (ImageID t) = t

-- | Helper function used for parsing a Text value into an ImageID. For now
-- just basic validation is used.
toImageID :: Text -> Maybe ImageID
toImageID t =
    if T.all (\c -> isAlphaNum c || c == ':') t then -- Note: Can we improve this whitelist?
        Just $ ImageID t
    else
        Nothing

-- | Timeout used for stopping a container. DefaultTimeout is 10 seconds.
data Timeout = Timeout Integer | DefaultTimeout deriving (Eq, Show)

-- TODO: Add more Signals or use an existing lib
-- | Signal used for sending to the process running in the container.
-- The default signal is SIGTERM.
data Signal = SIGHUP
            | SIGINT
            | SIGQUIT
            | SIGSTOP
            | SIGTERM
            | SIGUSR1
            | SIG Integer
            | SIGKILL deriving (Eq, Show)

instance FromJSON Signal where
    parseJSON (JSON.String "SIGTERM") = return SIGTERM
    parseJSON (JSON.String "SIGHUP") = return SIGHUP -- Note: Guessing on the string values for these.
    parseJSON (JSON.String "SIGINT") = return SIGINT
    parseJSON (JSON.String "SIGQUIT") = return SIGQUIT
    parseJSON (JSON.String "SIGSTOP") = return SIGSTOP
    parseJSON (JSON.String "SIGUSR1") = return SIGUSR1
    parseJSON _ = fail "Unknown Signal"

instance ToJSON Signal where
    toJSON SIGHUP = "SIGHUP"
    toJSON SIGINT = "SIGINT"
    toJSON SIGQUIT = "SIGQUIT"
    toJSON SIGSTOP = "SIGSTOP"
    toJSON SIGTERM = "SIGTERM"
    toJSON SIGUSR1 = "SIGUSR1"
    toJSON (SIG i) = toJSON i
    toJSON SIGKILL = "SIGKILL"

data ContainerDetails = ContainerDetails {
      appArmorProfile            :: Text
    , args                       :: [Text]
    , containerDetailsConfig     :: ContainerConfig
    , created                    :: UTCTime
    , driver                     :: Text
    -- , execIDs -- Not sure what this is in 1.24 spec.
    , containerDetailsHostConfig :: HostConfig
    , hostnamePath               :: FilePath
    , hostsPath                  :: FilePath
    , logPath                    :: FilePath
    , containerDetailsId         :: ContainerID
    , containerDetailsImage      :: ImageID
    , mountLabel                 :: Text
    , name                       :: Text
    , networkSettings            :: NetworkSettings
    , path                       :: FilePath
    , processLabel               :: Text
    , resolveConfPath            :: FilePath
    , restartCount               :: Int
    , state                      :: ContainerState
    , mounts                     :: [Mount]
    }
    deriving (Eq, Show, Generic)

-- | Data type used for parsing the mount information from a container
-- list.
data Mount = Mount {
      mountName        :: Text
    , mountSource      :: FilePath
    , mountDestination :: FilePath
    , mountDriver      :: Text
    , mountMode        :: Maybe VolumePermission -- apparently this can be null
    , mountRW          :: Bool
    , mountPropogation :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Mount where
    parseJSON (JSON.Object o) = do
        name <- o .: "Name"
        src <- o .: "Source"
        dest <- o .: "Destination"
        driver <- o .: "Driver"
        mode <- o .: "Mode"
        rw <- o .: "RW"
        prop <- o .: "Propagation"
        return $ Mount name src dest driver mode rw prop
    parseJSON _ = fail "Mount is not an object"

-- | Data type used for parsing the container state from a list of
-- containers.
data ContainerState = ContainerState {
      containerError :: Text
    , exitCode       :: Int
    , finishedAt     :: Maybe UTCTime -- Note: Is this a maybe?
    , oomKilled      :: Bool
    , dead           :: Bool
    , paused         :: Bool
    , pid            :: Int
    , restarting     :: Bool
    , running        :: Bool
    , startedAt      :: UTCTime
    , status         :: Status
    }
    deriving (Eq, Show, Generic)

instance FromJSON ContainerState where
    parseJSON (JSON.Object o) = do
        err <- o .: "Error"
        exit <- o .: "ExitCode"
        finished <- o .:? "FinishedAt"
        oomKilled <- o .: "OOMKilled"
        dead <- o .: "Dead"
        paused <- o .: "Paused"
        pid <- o .: "Pid"
        restarting <- o .: "Restarting"
        running <- o .: "Running"
        started <- o .: "StartedAt"
        st <- o .: "Status"
        return $ ContainerState err exit finished oomKilled dead paused pid restarting running started st
    parseJSON _ = fail "ContainerState is not an object"


-- | Client options used to configure the remote engine we're talking to
data DockerClientOpts = DockerClientOpts {
      apiVer  :: ApiVersion
    , baseUrl :: URL
    }
    deriving (Eq, Show)

-- | Default "DockerClientOpts" used for talking to the docker engine.
defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts {
                  apiVer = "v1.24"
                , baseUrl = "http://127.0.0.1:2375"
                }

-- | List options used for filtering the list of container or images.
data ListOpts = ListOpts { all :: Bool } deriving (Eq, Show)

-- | Default "ListOpts". Doesn't list stopped containers.
defaultListOpts :: ListOpts
defaultListOpts = ListOpts { all=False }

-- | Data type used for represneting the version of the docker engine
-- remote API.
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

instance FromJSON ContainerDetails where
    parseJSON v@(JSON.Object o) = do
        appArmor <- o .: "AppArmorProfile"
        args <- o .: "Args"
        config <- o .: "Config"
        created <- o .: "Created"
        driver <- o .: "Driver"
        hostConfig <- o .: "HostConfig"
        hostnamePath <- o .: "HostnamePath"
        hostsPath <- o .: "HostsPath"
        logPath <- o .: "LogPath"
        id <- parseJSON v
        image <- o .: "Image"
        mountLabel <- o .: "MountLabel"
        name <- o .: "Name"
        networkSettings <- o .: "NetworkSettings"
        path <- o .: "Path"
        processLabel <- o .: "ProcessLabel"
        resolveConfPath <- o .: "ResolvConfPath"
        restartCount <- o .: "RestartCount"
        state <- o .: "State"
        mounts <- o .: "Mounts"
        return $ ContainerDetails appArmor args config created driver hostConfig hostnamePath hostsPath logPath id image mountLabel name networkSettings path processLabel resolveConfPath restartCount state mounts
    parseJSON _ = fail "ContainerDetails is not an object"

instance ToJSON ContainerID where
    toJSON (ContainerID cid) = object ["Id" .= cid]

instance FromJSON ContainerID where
    parseJSON (JSON.Object o) = do
        cid <- o .: "Id"
        case toContainerID cid of
            Nothing ->
                fail "Invalid ContainerID"
            Just cid ->
                return cid
    parseJSON _ = fail "ContainerID is not an object."

instance ToJSON ImageID where
    toJSON (ImageID iid) = JSON.String iid

instance FromJSON ImageID where
    parseJSON (JSON.String t) = case toImageID t of
        Nothing ->
            fail "Invalid ImageID"
        Just iid ->
            return iid
    parseJSON _ = fail "ImageID is not an object."

-- | Data type used for representing the information of various ports that
-- a contianer may expose.
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
        parseJSON _ = fail "ContainerPortInfo: Not a JSON object."

-- For inspecting container details.
-- | Data type used for parsing the network information of each container
-- when listing them.
data NetworkOptions = NetworkOptions {
--                       ipamConfig          :: Maybe Text -- Don't see in 1.24
--                     , links               :: Maybe Text -- Don't see in 1.24
--                     , aliases             :: Maybe Text -- Don't see in 1.24
                      networkOptionsId                  :: Text
                    , networkOptionsEndpointId          :: Text
                    , networkOptionsGateway             :: Text
                    , networkOptionsIpAddress           :: Text -- Note: Parse this?
                    , networkOptionsIpPrefixLen         :: Int
                    , networkOptionsIpV6Gateway         :: Maybe Text
                    , networkOptionsGlobalIPv6Address   :: Maybe Text
                    , networkOptionsGlobalIPv6PrefixLen :: Maybe Int
                    , networkOptionsMacAddress          :: Text
                    } deriving (Eq, Show)

instance FromJSON NetworkOptions where
    parseJSON (JSON.Object o) = do
        networkId <- o .: "NetworkID"
        endpointId <- o .: "EndpointID"
        gateway <- o .: "Gateway"
        ip <- o .: "IPAddress"
        ipLen <- o .: "IPPrefixLen"
        ip6Gateway <- o .:? "IPv6Gateway"
        globalIP6 <- o .:? "GlobalIPv6Address"
        globalIP6Len <- o .:? "GlobalIPv6PrefixLen"
        mac <- o .: "MacAddress"
        return $ NetworkOptions networkId endpointId gateway ip ipLen ip6Gateway globalIP6 globalIP6Len mac
    parseJSON _ = fail "NetworkOptions is not an object"

-- TODO: Not sure what this is used for anymore.
data Network = Network NetworkMode NetworkOptions
    deriving (Eq, Show)

instance {-# OVERLAPPING #-} FromJSON [Network] where
    parseJSON (JSON.Object o) = HM.foldlWithKey' f (return []) o
        where
            f accM k' v' = do
                acc <- accM
                k <- parseJSON $ JSON.String k'
                v <- parseJSON v'
                return $ (Network k v):acc
    parseJSON _ = fail "Networks is not an object"

-- | Data type reprsenting the various network settings a container can have.
data NetworkSettings = NetworkSettings {
                       networkSettingsBridge                 :: Text
                     , networkSettingsSandboxId              :: Text
                     , networkSettingsHairpinMode            :: Bool
                     , networkSettingsLinkLocalIPv6Address   :: Text
                     , networkSettingsLinkLocalIPv6PrefixLen :: Int
                     , networkSettingsPorts                  :: [PortBinding]
                     , networkSettingsSandboxKey             :: Text
                     , networkSettingsSecondaryIPAddresses   :: Maybe [Text] -- TODO: 1.24 spec is unclear
                     , networkSettingsSecondaryIPv6Addresses :: Maybe [Text] -- TODO: 1.24 spec is unclear
                     , networkSettingsEndpointID             :: Text
                     , networkSettingsGateway                :: Text
                     , networkSettingsGlobalIPv6Address      :: Text
                     , networkSettingsGlobalIPv6PrefixLen    :: Int
                     , networkSettingsIpAddress              :: Text
                     , networkSettingsIpPrefixLen            :: Int
                     , networkSettingsIpv6Gateway            :: Text
                     , networkSettingsMacAddress             :: Text
                     , networkSettingsNetworks               :: [Network]
                     }
                     deriving (Eq, Show)

instance FromJSON NetworkSettings where
    parseJSON (JSON.Object o) = do
        bridge <- o .: "Bridge"
        sandbox <- o .: "SandboxID"
        hairpin <- o .: "HairpinMode"
        localIP6 <- o .: "LinkLocalIPv6Address"
        localIP6Len <- o .: "LinkLocalIPv6PrefixLen"
        ports <- o .: "Ports" -- .!= []
        sandboxKey <- o .: "SandboxKey"
        secondaryIP <- o .: "SecondaryIPAddresses"
        secondayIP6 <- o .: "SecondaryIPv6Addresses"
        endpointID <- o .: "EndpointID"
        gateway <- o .: "Gateway"
        globalIP6 <- o .: "GlobalIPv6Address"
        globalIP6Len <- o .: "GlobalIPv6PrefixLen"
        ip <- o .: "IPAddress"
        ipLen <- o .: "IPPrefixLen"
        ip6Gateway <- o .: "IPv6Gateway"
        mac <- o .: "MacAddress"
        networks <- o .: "Networks"
        return $ NetworkSettings bridge sandbox hairpin localIP6 localIP6Len ports sandboxKey secondaryIP secondayIP6 endpointID gateway globalIP6 globalIP6Len ip ipLen ip6Gateway mac networks
    parseJSON _ = fail "NetworkSettings is not an object."

-- | Data type used for parsing a list of containers.
data Container = Container
               { containerId        :: ContainerID
               , containerNames     :: [Text]
               , containerImageName :: Text
               , containerImageId   :: ImageID
               , containerCommand   :: Text
               , containerCreatedAt :: Int
               , containerStatus    :: Status
               , containerPorts     :: [ContainerPortInfo]
               , containerLabels    :: [Label]
               , containerNetworks  :: [Network]
               , containerMounts    :: [Mount]
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
                <*> (v .: "NetworkSettings" >>= parseNetworks)
                <*> (v .: "Mounts")
            where
                parseNetworks (JSON.Object v) =
                    (v .: "Networks") >>= parseJSON
                parseNetworks _ = fail "Container NetworkSettings: Not a JSON object."

        parseJSON _ = fail "Container: Not a JSON object."

-- | Represents the status of the container life cycle.
data Status = Created | Restarting | Running | Paused | Exited | Dead
    deriving (Eq, Show, Generic)

instance FromJSON Status where
    parseJSON (JSON.String "running") = return Running
    parseJSON (JSON.String "created") = return Created -- Note: Guessing on the string values of these.
    parseJSON (JSON.String "restarting") = return Restarting
    parseJSON (JSON.String "paused") = return Paused
    parseJSON (JSON.String "exited") = return Exited
    parseJSON (JSON.String "dead") = return Dead
    parseJSON _ = fail "Unknown Status"

-- | Alias for representing a RepoDigest. We could newtype this and add
-- some validation.
type Digest = Text

-- | Container and Image Labels.
data Label = Label Name Value deriving (Eq, Show)


-- If there are multiple lables with the same Name in the list
-- then the last one wins.
instance {-# OVERLAPPING #-} ToJSON [Label] where
    toJSON [] = emptyJsonObject
    toJSON (l:ls) = toJsonKeyVal (l:ls) key val
        where key (Label k _) = T.unpack k
              val (Label _ v) = v

instance {-# OVERLAPPING #-} FromJSON [Label] where
    parseJSON (JSON.Object o) = HM.foldlWithKey' f (return []) o
        where f accM k v = do
                acc <- accM
                value <- parseJSON v
                return $ (Label k value):acc
    parseJSON JSON.Null = return []
    parseJSON _ = fail "Failed to parse Labels. Not an object."

-- | Alias for Tags.
type Tag = Text

-- | Data type used for parsing information from a list of images.
data Image = DockerImage {
      imageId          :: ImageID
    , imageCreated     :: Integer
    , imageParentId    :: Maybe ImageID
    , imageRepoTags    :: [Tag]
    , imageRepoDigests :: [Digest]
    , imageSize        :: Integer
    , imageVirtualSize :: Integer
    , imageLabels      :: [Label]
    } deriving (Show, Eq, Generic)

-- | Helper function used for dropping the "image" prefix when serializing
-- the Image data type to and from json.
dropImagePrefix :: [a] -> [a]
dropImagePrefix = drop 5


instance FromJSON Image where
    parseJSON (JSON.Object o) = do
        imageId <- o .: "Id"
        imageCreated <- o .: "Created"
        imageParentId <- o .:? "ParentId"
        imageRepoTags <- o .:? "RepoTags" .!= []
        imageRepoDigests <- o .:? "RepoDigests" .!= []
        imageSize <- o .: "Size"
        imageVirtualSize <- o .: "VirtualSize"
        imageLabels <- o .:? "Labels" .!= []
        return $ DockerImage imageId imageCreated imageParentId imageRepoTags imageRepoDigests imageSize imageVirtualSize imageLabels
    parseJSON _ = fail "Failed to parse DockerImage."


-- | Options used for creating a Container.
data CreateOpts = CreateOpts {
                  containerConfig :: ContainerConfig
                , hostConfig      :: HostConfig
                } deriving (Eq, Show)

instance ToJSON CreateOpts where
    toJSON (CreateOpts cc hc) = do
        let ccJSON = toJSON cc
        let hcJSON = toJSON hc
        case ccJSON of
            JSON.Object (o :: HM.HashMap T.Text JSON.Value) -> do
                JSON.Object $ HM.insert "HostConfig" hcJSON o
            _ -> error "ContainerConfig is not an object." -- This should never happen.

-- | Container configuration used for creating a container with sensible
-- defaults.
defaultContainerConfig :: Text -> ContainerConfig
defaultContainerConfig imageName = ContainerConfig {
                       hostname=Nothing
                     , domainname=Nothing
                     , user=Nothing
                     , attachStdin=False
                     , attachStdout=False
                     , image=imageName
                     , attachStderr=False
                     , exposedPorts=[]
                     , tty=False
                     , openStdin=False
                     , stdinOnce=False
                     , env=[]
                     , cmd=[]
                     , volumes=[]
                     , workingDir=Nothing
                     , entrypoint=Nothing
                     , networkDisabled=Nothing
                     , macAddress=Nothing
                     , labels=[]
                     , stopSignal=SIGTERM
                     }

-- | Default host confiratuon used for creating a container.
defaultHostConfig :: HostConfig
defaultHostConfig = HostConfig {
                       binds=[]
                     , containerIDFile=Nothing
                     , logConfig=LogDriverConfig JsonFile []
                     , networkMode=Bridge
                     , portBindings=[]
                     , restartPolicy=RestartOff
                     , volumeDriver=Nothing
                     , volumesFrom=[]
                     , capAdd=[]
                     , capDrop=[]
                     , dns=[]
                     , dnsOptions=[]
                     , dnsSearch=[]
                     , extraHosts=[]
                     , ipcMode=Nothing
                     , links=[]
                     , oomScoreAdj=Nothing
                     , privileged=False
                     , publishAllPorts=False
                     , readonlyRootfs=False
                     , securityOpt=[]
                     , shmSize=Nothing
                     , resources=defaultContainerResources
                     }

-- Default container resource contstraints (None).
defaultContainerResources :: ContainerResources
defaultContainerResources = ContainerResources {
                          cpuShares=Nothing
                        , blkioWeight=Nothing
                        , blkioWeightDevice=Nothing
                        , blkioDeviceReadBps=Nothing
                        , blkioDeviceWriteBps=Nothing
                        , blkioDeviceReadIOps=Nothing
                        , blkioDeviceWriteIOps=Nothing
                        , cpuPeriod=Nothing
                        , cpusetCpus=Nothing
                        , cpusetMems=Nothing
                        , devices=[]
                        , kernelMemory=Nothing
                        , memory=Nothing
                        , memoryReservation=Nothing
                        , memorySwap=Nothing
                        , oomKillDisable=False
                        , ulimits=[]
                        }

-- | Default create options when creating a container. You only need to
-- specify an image name and the rest is all sensible defaults.
defaultCreateOpts :: T.Text -> CreateOpts
defaultCreateOpts imageName = CreateOpts { containerConfig = defaultContainerConfig imageName, hostConfig = defaultHostConfig }

-- | Override the key sequence for detaching a container.
-- Format is a single character [a-Z] or ctrl-<value> where <value> is one of: a-z, @, ^, [, , or _.
data DetachKeys = WithCtrl Char | WithoutCtrl Char | DefaultDetachKey deriving (Eq, Show)

-- | Options for starting a container.
data StartOpts = StartOpts { detachKeys :: DetachKeys } deriving (Eq, Show)

-- | Default options for staring a container.
defaultStartOpts :: StartOpts
defaultStartOpts = StartOpts { detachKeys = DefaultDetachKey }

-- | Options for deleting a container.
data DeleteOpts = DeleteOpts {
                  deleteVolumes :: Bool -- ^ Automatically cleanup volumes that the container created as well.
                , force         :: Bool -- ^ If the container is still running force deletion anyway.
                } deriving (Eq, Show)

-- | Default options for deleting a container. Most of the time we DON'T
-- want to delete the container's volumes or force delete it if it's
-- running.
defaultDeleteOpts :: DeleteOpts
defaultDeleteOpts = DeleteOpts { deleteVolumes = False, force = False }

-- | Timestamp alias.
type Timestamp = Integer

-- | Used for requesting N number of lines when tailing a containers log
-- output.
data TailLogOpt = Tail Integer | All deriving (Eq, Show)


-- | Log options used when requesting the log output from a container.
data LogOpts = LogOpts {
               stdout     :: Bool
             , stderr     :: Bool
             , since      :: Maybe Timestamp
             , timestamps :: Bool
             , tail       :: TailLogOpt
             } deriving (Eq, Show)

-- | Sensible default for log options.
defaultLogOpts :: LogOpts
defaultLogOpts = LogOpts { stdout = True
                         , stderr = True
                         , since = Nothing
                         , timestamps = True
                         , tail = All
                         }

-- TOOD: Add support for SELinux Volume labels (eg. "ro,z" or "ro/Z")
-- | Set permissions on volumes that you mount in the container.
data VolumePermission = ReadWrite | ReadOnly deriving (Eq, Show, Generic)

instance ToJSON VolumePermission where
    toJSON ReadWrite = "rw"
    toJSON ReadOnly = "ro"

instance FromJSON VolumePermission where
    parseJSON "rw" = return ReadWrite
    parseJSON "ro" = return ReadOnly
    parseJSON _ = fail "Failed to parse VolumePermission"

-- | Used for marking a directory in the container as "exposed" hence
-- taking it outside of the COW filesystem and making it mountable
-- in other containers using "VolumesFrom". The volume usually get's
-- created somewhere in @/var/lib/docker/volumes@ (depending on the volume
-- driver used).
-- The CLI example is:
--
-- @
-- docker run --name app -v \/opt\/data -it myapp:latest
-- docker run --name app2 --volumes-from app \/bin\/bash -c "ls -l \/opt\/data"
-- @
newtype Volume = Volume FilePath deriving (Eq, Show)

instance {-# OVERLAPPING #-} ToJSON [Volume] where
    toJSON [] = emptyJsonObject
    toJSON (v:vs) = toJsonKey (v:vs) getKey
        where getKey (Volume v) = v

instance {-# OVERLAPPING #-} FromJSON [Volume] where
    parseJSON (JSON.Object o) = return $ map (Volume . T.unpack) $ HM.keys o
    parseJSON (JSON.Null) = return []
    parseJSON _ = fail "Volume is not an object"


data Bind = Bind { hostSrc          :: Text
                 , containerDest    :: Text
                 , volumePermission :: Maybe VolumePermission
                 } deriving (Eq, Show)

instance FromJSON Bind where
    parseJSON (JSON.String t) = case T.split (== ':') t of
        [src, dest] -> return $ Bind src dest Nothing
        [src, dest, "rw"] -> return $ Bind src dest $ Just ReadWrite
        [src, dest, "ro"] -> return $ Bind src dest $ Just ReadOnly
        _ -> fail "Could not parse Bind"
    parseJSON _ = fail "Bind is not a string"

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

data VolumeFrom = VolumeFrom ContainerName (Maybe VolumePermission) deriving (Eq, Show)

instance FromJSON VolumeFrom where
    parseJSON (JSON.String t) = case T.split (== ':') t of
        [vol] -> return $ VolumeFrom vol Nothing
        [vol, "rw"] -> return $ VolumeFrom vol $ Just ReadWrite
        [vol, "ro"] -> return $ VolumeFrom vol $ Just ReadOnly
        _ -> fail "Could not parse VolumeFrom"
    parseJSON _ = fail "VolumeFrom is not a string"

instance ToJSON VolumeFrom where
    toJSON (VolumeFrom n p) = case p of
        Nothing -> toJSON $ n <> ":" <> "rw"
        Just per -> toJSON $ n <> ":" <> (T.pack $ show per)

instance ToJSON Bind where
    toJSON (Bind src dest mode) = toJSON $ case mode of
                        Nothing -> T.concat[src, ":", dest]
                        Just m ->  T.concat[src, ":", dest, ":", (T.pack $ show m)]

data Link = Link Text (Maybe Text) deriving (Eq, Show)

instance FromJSON Link where
    parseJSON (JSON.String t) = case T.split (== ':') t of
        [f] -> return $ Link f Nothing
        [f,s] -> return $ Link f $ Just s
        _ -> fail "Could not parse Link"
    parseJSON _ = fail "Link is not a string"

instance ToJSON Link where
    toJSON (Link n1 n2) = toJSON $ case n2 of
                        Nothing -> T.concat[n1, ":", n1] -- use same name in container
                        Just n ->  T.concat[n1, ":", n] -- used specified name in container

-- { "Type": "<driver_name>", "Config": {"key1": "val1"} }
data LogDriverType = JsonFile | Syslog | Journald | Gelf | Fluentd | AwsLogs | Splunk | Etwlogs | LoggingDisabled deriving (Eq, Show)

instance FromJSON LogDriverType where
    parseJSON (JSON.String "json-file") = return JsonFile
    parseJSON (JSON.String "syslog") = return Syslog
    parseJSON (JSON.String "journald") = return Journald
    parseJSON (JSON.String "gelf") = return Gelf
    parseJSON (JSON.String "fluentd") = return Fluentd
    parseJSON (JSON.String "awslogs") = return AwsLogs
    parseJSON (JSON.String "splunk") = return Splunk
    parseJSON (JSON.String "etwlogs") = return Etwlogs
    parseJSON (JSON.String "none") = return LoggingDisabled
    parseJSON _ = fail "Unknown LogDriverType"

instance ToJSON LogDriverType where
    toJSON JsonFile = JSON.String "json-file"
    toJSON Syslog = JSON.String "syslog"
    toJSON Journald = JSON.String "journald"
    toJSON Gelf = JSON.String "gelf"
    toJSON Fluentd = JSON.String "fluentd"
    toJSON AwsLogs = JSON.String "awslogs"
    toJSON Splunk = JSON.String "splunk"
    toJSON Etwlogs = JSON.String "etwlogs"
    toJSON LoggingDisabled = JSON.String "none"

data LogDriverOption = LogDriverOption Name Value deriving (Eq, Show)

instance {-# OVERLAPPING #-} ToJSON [LogDriverOption] where
    toJSON [] = emptyJsonObject
    toJSON (o:os) = toJsonKeyVal (o:os) key val
        where key (LogDriverOption n _) = T.unpack n
              val (LogDriverOption _ v) = v

instance {-# OVERLAPPING #-} FromJSON [LogDriverOption] where
    parseJSON (JSON.Object o) = HM.foldlWithKey' f (return []) o
        where f accM k v = do
                acc <- accM
                value <- parseJSON v
                return $ (LogDriverOption k value):acc
    parseJSON JSON.Null = return []
    parseJSON _ = fail "Failed to parse LogDriverOptions"

data LogDriverConfig = LogDriverConfig LogDriverType [LogDriverOption] deriving (Eq, Show)

instance ToJSON LogDriverConfig where
    toJSON (LogDriverConfig driverType []) = object ["Type" .= driverType]
    toJSON (LogDriverConfig driverType driverOptions) = object ["Type" .= driverType, "Config" .= driverOptions]

instance FromJSON LogDriverConfig where
    parseJSON (JSON.Object o) = do
        typ <- o .: "Type"
        opts <- o .: "Config"
        return $ LogDriverConfig typ opts
    parseJSON _ = fail "LogDriverConfig is not an object"

-- TODO: Add container:<name|id> mode
data NetworkMode = Bridge | Host | NetworkDisabled
    deriving (Eq, Show, Ord)

instance FromJSON NetworkMode where
    parseJSON (JSON.String "bridge") = return Bridge
    parseJSON (JSON.String "host") = return Host -- Note: Guessing on these.
    parseJSON (JSON.String "none") = return NetworkDisabled
    parseJSON _ = fail "Unknown NetworkMode"

instance ToJSON NetworkMode where
    toJSON Bridge = JSON.String "bridge"
    toJSON Host = JSON.String "host"
    toJSON NetworkDisabled  = JSON.String "none"

data PortType = TCP | UDP deriving (Eq, Generic, Read, Ord)

instance Show PortType where
    show TCP = "tcp"
    show UDP = "udp"

instance ToJSON PortType where
    toJSON TCP = "tcp"
    toJSON UDP = "udp"

instance FromJSON PortType where
    parseJSON val = case val of
                    "tcp" -> return TCP
                    "udp" -> return UDP
                    _ -> fail "PortType: Invalid port type."

-- newtype NetworkInterface = NetworkInterface Text deriving (Eq, Show)
--
-- instance FromJSON NetworkInterface where
--     parseJSON (JSON.String v) = return $ NetworkInterface v
--     parseJSON _  = fail "Network interface is not a string."
--
-- instance ToJSON NetworkInterface where
--     toJSON (NetworkInterface i) = JSON.String i

-- | This datastructure models mapping a Port from the container onto the
-- host system s that the service running in the container can be accessed from
-- the outside world. We either map a port onto all interfaces (default) or onto a specific
-- interface like `127.0.0.1`.
-- __NOTE__: We should disallow duplicate port bindings as the ToJSON
-- instance will only send the last one.
-- { <port>/<protocol>: [{ "HostPort": "<port>"  }] }
data PortBinding = PortBinding {
                   containerPort :: Port
                 , portType      :: PortType
                 , hostPorts     :: [HostPort]
                 } deriving (Eq, Show)

portAndType2Text :: Port -> PortType -> Text
portAndType2Text p t = (T.pack $ show p) <> "/" <> (T.pack $ show t)


-- | A helper function to more easily add a bind mount to existing
-- "CreateOpts" records.
addBind :: Bind -> CreateOpts -> CreateOpts
addBind b c = c{hostConfig=hc{binds=obs <> [b]}}
    where hc = hostConfig c
          obs = binds $ hostConfig c

-- | Helper function for adding a Command to and existing
-- CreateOpts record.
setCmd :: Text -> CreateOpts -> CreateOpts
setCmd ccmd c = c{containerConfig=cc{cmd=[ccmd]}}
    where cc = containerConfig c

-- | Helper function for adding a "Link" to and existing
-- CreateOpts record.
addLink :: Link -> CreateOpts -> CreateOpts
addLink l c =  c{hostConfig=hc{links=ols <> [l]}}
    where hc = hostConfig c
          ols = links $ hostConfig c

-- | Helper function for adding a "Volume" to and existing
-- CreateOpts record.
addVolume :: Volume -> CreateOpts -> CreateOpts
addVolume v c = c{containerConfig=cc{volumes=oldvs <> [v]}}
    where cc = containerConfig c
          oldvs = volumes cc

-- | Helper function for adding a "VolumeFrom" to and existing
-- CreateOpts record.
addVolumeFrom :: VolumeFrom -> CreateOpts -> CreateOpts
addVolumeFrom vf c = c{hostConfig=hc{volumesFrom=oldvfs <> [vf]}}
    where hc = hostConfig c
          oldvfs = volumesFrom hc

-- | A convenience function that adds PortBindings to and exiting
-- "CreateOpts" record.  Useful with 'defaultCreateOpts'
-- Example:
--
-- >>> let pb = PortBinding 80 TCP [HostPort "0.0.0.0" 8000]
-- >>> addPortBinding pb $ defaultCreateOpts "nginx:latest"
addPortBinding :: PortBinding -> CreateOpts -> CreateOpts
addPortBinding pb c = c{hostConfig=hc{portBindings=pbs <> [pb]}}
    where hc = hostConfig c
          pbs = portBindings $ hostConfig c

-- | Helper function for adding a "ExposedPort" to and existing
-- CreateOpts record.
addExposedPort :: ExposedPort -> CreateOpts -> CreateOpts
addExposedPort ep c = c{containerConfig=cc{exposedPorts=oldeps <> [ep]}}
    where cc = containerConfig c
          oldeps = exposedPorts cc


instance {-# OVERLAPPING #-} FromJSON [PortBinding] where
    parseJSON (JSON.Object o) = HM.foldlWithKey' f (return []) o
        where
            f accM k v = case T.split (== '/') k of
                [port', portType'] -> do
                    port <- parseIntegerText port'
                    portType <- parseJSON $ JSON.String portType'
                    acc <- accM
                    hps <- parseJSON v
                    return $ (PortBinding port portType hps):acc
                _ -> fail "Could not parse PortBindings"
    parseJSON (JSON.Null) = return []
    parseJSON _ = fail "PortBindings is not an object"

instance {-# OVERLAPPING #-} ToJSON [PortBinding] where
    toJSON [] = emptyJsonObject
    toJSON (p:ps) = toJsonKeyVal (p:ps) key val
        where key p =  T.unpack $ portAndType2Text (containerPort p) (portType p)
              val p =  hostPorts p

data HostPort = HostPort {
      hostIp   :: Text
    , hostPost :: Port
    }
    deriving (Eq, Show)

instance ToJSON HostPort where
    toJSON (HostPort i p) = object ["HostPort" .= show p, "HostIp" .= i]

instance FromJSON HostPort where
    parseJSON (JSON.Object o) = do
        p <- o .: "HostPort" >>= parseIntegerText
        i <- o .: "HostIp"
        return $ HostPort i p
    parseJSON _ = fail "HostPort is not an object."

-- { "Name": "on-failure" , "MaximumRetryCount": 2}
type RetryCount = Integer
data RestartPolicy = RestartAlways | RestartUnlessStopped | RestartOnFailure RetryCount | RestartOff  deriving (Eq, Show)

instance FromJSON RestartPolicy where
    parseJSON (JSON.Object o) = do
        (name :: Text) <- o .: "Name"
        case name of
            "always" -> return RestartAlways
            "unless-stopped" -> return RestartUnlessStopped
            "on-failure" -> do
                retry <- o .: "MaximumRetryCount"
                return $ RestartOnFailure retry
            "off" -> return RestartOff
            _ -> fail "Could not parse RestartPolicy"
    parseJSON _ = fail "RestartPolicy is not an object"

instance ToJSON RestartPolicy where
    toJSON RestartAlways = object ["Name" .= JSON.String "always"]
    toJSON RestartUnlessStopped = object ["Name" .= JSON.String "unless-stopped"]
    toJSON (RestartOnFailure c) = object ["Name" .= JSON.String "on-failure", "MaximumRetryCount" .= c]
    toJSON RestartOff = object ["Name" .= JSON.String "off"]

data Isolation = Default | Process | Hyperv  deriving (Eq, Show)

newtype UTSMode = UTSMode Text deriving (Eq, Show)

-- TODO: Add Tmpfs : List of tmpfs (mounts) used for the container
-- TODO: Add UTSMode : UTS namespace to use for the container
-- TODO: Sysctls map[string]string `json:",omitempty"` // List of Namespaced sysctls used for the container
data HostConfig = HostConfig
                { binds           :: [Bind]
                , containerIDFile :: Maybe FilePath -- 1.24: Only in responses, not create
                , logConfig       :: LogDriverConfig
                , networkMode     :: NetworkMode
                , portBindings    :: [PortBinding]
                , restartPolicy   :: RestartPolicy
                , volumeDriver    :: Maybe Text
                , volumesFrom     :: [VolumeFrom]
                , capAdd          :: [Text]
                , capDrop         :: [Text]
                , dns             :: [Text]
                , dnsOptions      :: [Text]
                , dnsSearch       :: [Text]
                , extraHosts      :: [Text]
                -- , groupAdd        :: [Integer] -- 1.24: Missing from inspecting container details... Going to omit for now.
                , ipcMode         :: Maybe Text -- 1.24: Only in inspect, not create
                , links           :: [Link]
                , oomScoreAdj     :: Maybe Integer
                -- , pidMode         :: Text -- 1.24: Don't see pidMode, just pidsLimit
                , privileged      :: Bool
                , publishAllPorts :: Bool
                , readonlyRootfs  :: Bool
                , securityOpt     :: [Text]
                -- , utsMode         :: UTSMode -- 1.24: Don't see this
                , shmSize         :: Maybe Integer
                -- , consoleSize     :: Integer -- 1.24: Don't see this
                -- , isolation       :: Isolation -- 1.24: Don't see this
                , resources       :: ContainerResources
                } deriving (Eq, Show, Generic)

instance FromJSON HostConfig where
    parseJSON v@(JSON.Object o) = HostConfig
        <$> o .: "Binds"
        <*> o .: "ContainerIDFile"
        <*> o .: "LogConfig"
        <*> o .: "NetworkMode"
        <*> o .: "PortBindings"
        <*> o .: "RestartPolicy"
        <*> o .: "VolumeDriver"
        <*> o .: "VolumesFrom"
        <*> o .: "CapAdd"
        <*> o .: "CapDrop"
        <*> o .: "Dns"
        <*> o .: "DnsOptions"
        <*> o .: "DnsSearch"
        <*> o .: "ExtraHosts"
        <*> o .: "IpcMode"
        <*> o .:? "Links" .!= []
        <*> o .: "OomScoreAdj"
        <*> o .: "Privileged"
        <*> o .: "PublishAllPorts"
        <*> o .: "ReadonlyRootfs"
        <*> o .: "SecurityOpt"
        <*> o .: "ShmSize"
        <*> parseJSON v
    parseJSON _ = fail "HostConfig is not an object."

instance ToJSON HostConfig where
    toJSON HostConfig{..} =
        let arr = [
                "Binds" .= binds
              , "ContainerIDFile" .= containerIDFile
              , "LogConfig" .= logConfig
              , "NetworkMode" .= networkMode
              , "PortBindings" .= portBindings
              , "RestartPolicy" .= restartPolicy
              , "VolumeDriver" .= volumeDriver
              , "VolumesFrom" .= volumesFrom
              , "CapAdd" .= capAdd
              , "CapDrop" .= capDrop
              , "Dns" .= dns
              , "DnsOptions" .= dnsOptions
              , "DnsSearch" .= dnsSearch
              , "ExtraHosts" .= extraHosts
              , "IpcMode" .= ipcMode
              , "Links" .= links
              , "OomScoreAdj" .= oomScoreAdj
              , "Privileged" .= privileged
              , "PublishAllPorts" .= publishAllPorts
              , "ReadonlyRootfs" .= readonlyRootfs
              , "SecurityOpt" .= securityOpt
              , "ShmSize" .= shmSize
              ]
        in
        object $ arr <> ( resourcesArr resources)

        where
            -- JP: Not sure if this is better than a separate ToJSON instance with a bunch of `HM.insert`s.
            resourcesArr ContainerResources{..} = [
                "CpuShares" .= cpuShares
              , "BlkioWeight" .= blkioWeight
              , "BlkioWeightDevice" .= blkioWeightDevice
              , "BlkioDeviceReadBps" .= blkioDeviceReadBps
              , "BlkioDeviceWriteBps" .= blkioDeviceWriteBps
              , "BlkioDeviceReadIOps" .= blkioDeviceReadIOps
              , "BlkioDeviceWriteIOps" .= blkioDeviceWriteIOps
              , "CpuPeriod" .= cpuPeriod
              , "CpusetCpus" .= cpusetCpus
              , "CpusetMems" .= cpusetMems
              , "Devices" .= devices
              , "KernelMemory" .= kernelMemory
              , "Memory" .= memory
              , "MemoryReservation" .= memoryReservation
              , "MemorySwap" .= memorySwap
              , "OomKillDisable" .= oomKillDisable
              , "Ulimits" .= ulimits
              ]




-- { "Name": <name>, "Soft": <soft limit>, "Hard": <hard limit>  }
-- { "Name": "nofile", "Soft": 1024, "Hard": 2048  }
data Ulimit = Ulimit {
              ulimitName :: Text
            , ulimitSoft :: Integer
            , ulimitHard :: Integer
            } deriving (Eq, Show, Generic)

instance FromJSON Ulimit where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop 5}

instance ToJSON Ulimit where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop 5}

data DeviceWeight = DeviceWeight {
      deviceWeightPath   :: FilePath
    , deviceWeightWeight :: Text
    }
    deriving (Show, Eq)

instance FromJSON DeviceWeight where
    parseJSON (JSON.Object o) = DeviceWeight
        <$> o .: "Path"
        <*> o .: "Weight"
    parseJSON _ = fail "DeviceWeight is not an object."

instance ToJSON DeviceWeight where
    toJSON (DeviceWeight p w) = object [
          "Path" .= p
        , "Weight" .= w
        ]

data DeviceRate = DeviceRate {
      deviceRatePath :: FilePath
    , deviceRateRate :: Text
    }
    deriving (Show, Eq)

instance FromJSON DeviceRate where
    parseJSON (JSON.Object o) = DeviceRate
        <$> o .: "Path"
        <*> o .: "Rate"
    parseJSON _ = fail "DeviceRate is not an object."

instance ToJSON DeviceRate where
    toJSON (DeviceRate p r) = object [
          "Path" .= p
        , "Rate" .= r
        ]

data MemoryConstraintSize = B | MB | GB deriving (Eq, Show)

data MemoryConstraint = MemoryConstraint Integer MemoryConstraintSize deriving (Eq, Show)

instance ToJSON MemoryConstraint where
    toJSON (MemoryConstraint x B) = toJSON x
    toJSON (MemoryConstraint x MB) = toJSON $ x * 1024 * 1024
    toJSON (MemoryConstraint x GB) = toJSON $ x * 1024 * 1024 * 1024

instance FromJSON MemoryConstraint where
    parseJSON (JSON.Number x) = case (floatingOrInteger x) of
                                    Left (_ :: Double) -> fail "Failed to parse MemoryConstraint"
                                    Right i -> return $ MemoryConstraint i B
                                    -- The docker daemon will always return the number as bytes (integer), regardless of how we set them (using MB or GB)
    parseJSON _ = fail "Failed to parse MemoryConstraint"

data ContainerResources = ContainerResources {
                          cpuShares            :: Maybe Integer
                        -- , cgroupParent      :: Text -- 1.24: Missing from inspecting container details... Going to omit for now.
                        , blkioWeight          :: Maybe Integer
                        , blkioWeightDevice    :: Maybe [DeviceWeight]
                        , blkioDeviceReadBps   :: Maybe [DeviceRate] -- TODO: Not Text
                        , blkioDeviceWriteBps  :: Maybe [DeviceRate] -- TODO: Not Text
                        , blkioDeviceReadIOps  :: Maybe [DeviceRate] -- TODO: Not Text
                        , blkioDeviceWriteIOps :: Maybe [DeviceRate] -- TODO: Not Text
                        , cpuPeriod            :: Maybe Integer
                        -- , cpuQuota          :: Integer -- 1.24: Missing from inspecting container details... Going to omit for now.
                        , cpusetCpus           :: Maybe Text
                        , cpusetMems           :: Maybe Text
                        , devices              :: [Device]
                        -- , diskQuota         :: Integer -- Don't see this ins 1.24.
                        , kernelMemory         :: Maybe MemoryConstraint
                        , memory               :: Maybe MemoryConstraint
                        , memoryReservation    :: Maybe MemoryConstraint
                        , memorySwap           :: Maybe MemoryConstraint
                        -- , memorySwappiness  :: Integer -- 1.24: Missing from inspecting container details... Going to omit for now.
                        , oomKillDisable       :: Bool
                        -- , pidsLimit         :: Integer -- 1.24: Missing from inspecting container details... Going to omit for now.
                        , ulimits              :: [Ulimit]
                        -- TODO: Missing from 1.24
                        -- StorageOpt :: [(Text, Text)]
                        -- VolumeDriver :: ??
                        -- EndpointsConfig :: ??
                        -- TODO: Only in inspect container in 1.24
                        -- CpuPercent :: Int +
                        -- MaximumIOps :: Int +
                        -- MaximumIOBps :: Int +
                        -- LxcConf :: [??] +
                        } deriving (Eq, Show, Generic)

-- instance ToJSON ContainerResources where
--     toJSON = genericToJSON defaultOptions {
--          fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

instance FromJSON ContainerResources where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

type Port = Integer

type Name = Text
type Value = Text

data EnvVar = EnvVar Name Value
    deriving (Eq, Show)

instance FromJSON EnvVar where
    parseJSON (JSON.String env) =
        let (n, v') = T.break (== '=') env in
        let v = T.drop 1 v' in
        return $ EnvVar n v
    parseJSON _ = fail "EnvVar is not a string"

instance ToJSON EnvVar where
    toJSON (EnvVar n v) = object [n .= v]

-- | ExposedPort represents a port (and it's type)
-- that a container should expose to other containers or the host system.
-- `NOTE`: This does not automatically expose the port onto the host
-- system but rather it just tags it. It's best to be used with
-- the PublishAllPorts flag. It is also useful for
-- the daemon to know which Environment variables to
-- inject into a container linking to our container.
-- Example linking a Postgres container named db would inject the following
-- environment variables automatically if we set the corresponding
--
-- ExposedPort:
--
-- @
-- DB_PORT_5432_TCP_PORT="5432"
-- DB_PORT_5432_TCP_PROTO="tcp"
-- DB_PORT_5432_TCP="tcp://172.17.0.1:5432"
-- @
data ExposedPort = ExposedPort Port PortType deriving (Eq, Show)

instance {-# OVERLAPPING #-} FromJSON [ExposedPort] where
    parseJSON (JSON.Object o) = HM.foldlWithKey' f (return []) o
        where
            f accM k _ = case T.split (== '/') k of
                [port', portType'] -> do
                    port <- parseIntegerText port'
                    portType <- parseJSON $ JSON.String portType'
                    acc <- accM
                    return $ (ExposedPort port portType):acc
                _ -> fail "Could not parse ExposedPorts"
    parseJSON (JSON.Null) = return []
    parseJSON _ = fail "ExposedPorts is not an object"

instance {-# OVERLAPPING #-} ToJSON [ExposedPort] where
    toJSON [] = emptyJsonObject
    toJSON (p:ps) = toJsonKey (p:ps) key
        where key (ExposedPort p t) = show p <> slash <> show t
              slash = T.unpack "/"

data ContainerConfig = ContainerConfig {
                       hostname        :: Maybe Text
                     , domainname      :: Maybe Text
                     , user            :: Maybe Text
                     , attachStdin     :: Bool
                     , attachStdout    :: Bool
                     , attachStderr    :: Bool
                     , exposedPorts    :: [ExposedPort]
                     -- , publishService  :: Text -- Don't see this in 1.24
                     , tty             :: Bool
                     , openStdin       :: Bool
                     , stdinOnce       :: Bool
                     , env             :: [EnvVar]
                     , cmd             :: [Text]
                     -- , argsEscaped     :: Bool -- Don't see this in 1.24
                     , image           :: Text
                     , volumes         :: [Volume]
                     , workingDir      :: Maybe FilePath
                     , entrypoint      :: Maybe Text -- Can be null?
                     , networkDisabled :: Maybe Bool -- Note: Should we expand the JSON instance and take away the Maybe? Null is False?
                     , macAddress      :: Maybe Text
                     -- , onBuild         :: Maybe Text -- For 1.24, only see this in the inspect response.
                     , labels          :: [Label]
                     , stopSignal      :: Signal
                     } deriving (Eq, Show, Generic)

instance ToJSON ContainerConfig where
    toJSON = genericToJSON defaultOptions {
         fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

instance FromJSON ContainerConfig where
    parseJSON (JSON.Object o) = do
        hostname <- o .:? "Hostname"
        domainname <- o .:? "Domainname"
        user <- o .:? "User"
        attachStdin <- o .: "AttachStdin"
        attachStdout <- o .: "AttachStdout"
        attachStderr <- o .: "AttachStderr"
        exposedPorts <- o .:? "ExposedPorts" .!= []
        tty <- o .: "Tty"
        openStdin <- o .: "OpenStdin"
        stdinOnce <- o .: "StdinOnce"
        env <- o .: "Env"
        cmd <- o .: "Cmd"
        image <- o .: "Image"
        volumes <- o .: "Volumes"
        workingDir <- o .:? "WorkingDir"
        entrypoint <- o .:? "Entrypoint"
        networkDisabled <- o .:? "networkDisabled"
        macAddress <- o .:? "MacAddress"
        labels <- o .:? "Labels" .!= []
        stopSignal <- o .: "StopSignal"
        return $ ContainerConfig hostname domainname user attachStdin attachStdout attachStderr exposedPorts tty openStdin stdinOnce env cmd image volumes workingDir entrypoint networkDisabled
            macAddress labels stopSignal
    parseJSON _ = fail "NetworkSettings is not an object."

parseIntegerText :: (Monad m) => Text -> m Integer
parseIntegerText t = case readMaybe $ T.unpack t of
    Nothing ->
        fail "Could not parse Integer"
    Just i ->
        return i

-- | Helper function for converting a data type [a] to a json dictionary
-- like so {"something": {}, "something2": {}}
toJsonKey :: Foldable t => t a -> (a -> String) -> JSON.Value
toJsonKey vs getKey = JSON.Object $ foldl f HM.empty vs
        where f acc x = HM.insert (T.pack $ getKey x) (JSON.Object HM.empty) acc

-- | Helper function for converting a data type [a] to a json dictionary
-- like so {"something": "val1", "something2": "val2"}
toJsonKeyVal :: (Foldable t, JSON.ToJSON r) => t a -> (a -> String) -> (a -> r) -> JSON.Value
toJsonKeyVal vs getKey getVal = JSON.Object $ foldl f HM.empty vs
        where f acc x = HM.insert (T.pack $ getKey x) (toJSON $ getVal x) acc

-- | Helper function that return an empty dictionary "{}"
emptyJsonObject :: JSON.Value
emptyJsonObject = JSON.Object HM.empty

