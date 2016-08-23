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
    , Labels(..)
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
    , Volume(..)
    , Volumes(..)
    , Device(..)
    , ContainerName
    , VolumeFrom(..)
    , Link(..)
    , LogDriverType(..)
    , LogDriverOptions(..)
    , LogDriverConfig(..)
    , NetworkMode(..)
    , PortType(..)
--    , NetworkInterface(..)
    , Networks(..)
    , NetworkSettings(..)
    , NetworkOptions(..)
    , Mount(..)
    , PortBinding(..)
    , PortBindings(..)
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
    , ExposedPorts(..)
    , DeviceWeight(..)
    , DeviceRate(..)
    , addPortBinding
    ) where

import           Data.Aeson          (FromJSON, ToJSON, genericParseJSON,
                                      genericToJSON, object, parseJSON, toJSON,
                                      (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson          as JSON
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import           Data.Char           (isAlphaNum, toUpper)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           GHC.Generics        (Generic)
import           Prelude             hiding (all, tail)
import           Text.Read           (readMaybe)

data Endpoint =
        VersionEndpoint
      | ListContainersEndpoint ListOpts
      | ListImagesEndpoint ListOpts
      | CreateContainerEndpoint CreateOpts
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

type URL = Text
type ApiVersion = Text
newtype ContainerID = ContainerID Text
    deriving (Eq, Show)

fromContainerID :: ContainerID -> Text
fromContainerID (ContainerID t) = t

toContainerID :: Text -> Maybe ContainerID
toContainerID t =
    if T.all (\c -> isAlphaNum c || c == ':') t then -- Note: Can we improve this whitelist?
        Just $ ContainerID t
    else
        Nothing

newtype ImageID = ImageID Text
    deriving (Eq, Show)

fromImageID :: ImageID -> Text
fromImageID (ImageID t) = t

toImageID :: Text -> Maybe ImageID
toImageID t =
    if T.all (\c -> isAlphaNum c || c == ':') t then -- Note: Can we improve this whitelist?
        Just $ ImageID t
    else
        Nothing

data Timeout = Timeout Integer | DefaultTimeout deriving (Eq, Show)

-- TODO: Add more Signals or use an existing lib
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

data DockerClientOpts = DockerClientOpts {
      apiVer  :: ApiVersion
    , baseUrl :: URL
    }
    deriving (Eq, Show)

defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts {
                  apiVer = "v1.24"
                , baseUrl = "http://127.0.0.1:2375"
                }

data ListOpts = ListOpts { all :: Bool } deriving (Eq, Show)

defaultListOpts :: ListOpts
defaultListOpts = ListOpts { all=False }

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

newtype Networks = Networks (M.Map NetworkMode NetworkOptions) -- Note: Is it ever possible that there will be duplicates of network modes?
    deriving (Eq, Show)

instance FromJSON Networks where
    parseJSON (JSON.Object o) = do
        Networks <$> HM.foldlWithKey' f (return M.empty) o

        where
            f accM k' v' = do
                acc <- accM
                k <- parseJSON $ JSON.String k'
                v <- parseJSON v'
                return $ M.insert k v acc

    parseJSON _ = fail "Networks is not an object"

data NetworkSettings = NetworkSettings {
                       networkSettingsBridge                 :: Text
                     , networkSettingsSandboxId              :: Text
                     , networkSettingsHairpinMode            :: Bool
                     , networkSettingsLinkLocalIPv6Address   :: Text
                     , networkSettingsLinkLocalIPv6PrefixLen :: Int
                     , networkSettingsPorts                  :: PortBindings
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
                     , networkSettingsNetworks               :: Networks
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

data Container = Container
               { containerId        :: ContainerID
               , containerNames     :: [Text]
               , containerImageName :: Text
               , containerImageId   :: ImageID
               , containerCommand   :: Text
               , containerCreatedAt :: Int
               , containerStatus    :: Status
               , containerPorts     :: [ContainerPortInfo]
               , containerLabels    :: Labels
               , containerNetworks  :: Networks
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
    , imageRepoDigests :: Maybe [Digest]
    , imageSize        :: Integer
    , imageVirtualSize :: Integer
    , imageLabels      :: Maybe Labels
    } deriving (Show, Eq, Generic)

dropImagePrefix :: [a] -> [a]
dropImagePrefix = drop 5

instance FromJSON Image where
    parseJSON = genericParseJSON defaultOptions {
            fieldLabelModifier = dropImagePrefix}

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

defaultContainerConfig :: Text -> ContainerConfig
defaultContainerConfig imageName = ContainerConfig {
                       hostname=Nothing
                     , domainname=Nothing
                     , user=Nothing
                     , attachStdin=False
                     , attachStdout=False
                     , image=imageName
                     , attachStderr=False
                     , exposedPorts=Nothing -- ExposedPorts M.empty
                     , tty=False
                     , openStdin=False
                     , stdinOnce=False
                     , env=[]
                     , cmd=[]
                     , volumes=Nothing
                     , workingDir=Nothing
                     , entrypoint=Nothing
                     , networkDisabled=Nothing
                     , macAddress=Nothing
                     , labels=Nothing
                     , stopSignal=SIGTERM
                     }

defaultHostConfig :: HostConfig
defaultHostConfig = HostConfig {
                       binds=[]
                     , containerIDFile=Nothing
                     , logConfig=LogDriverConfig JsonFile Nothing
                     , networkMode=Bridge
                     , portBindings=PortBindings []
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


defaultCreateOpts :: T.Text -> CreateOpts
defaultCreateOpts imageName = CreateOpts { containerConfig = defaultContainerConfig imageName, hostConfig = defaultHostConfig }

-- detachKeys – Override the key sequence for detaching a container.
-- Format is a single character [a-Z] or ctrl-<value> where <value> is one of: a-z, @, ^, [, , or _.
data DetachKeys = WithCtrl Char | WithoutCtrl Char | DefaultDetachKey deriving (Eq, Show)

data StartOpts = StartOpts { detachKeys :: DetachKeys } deriving (Eq, Show)

defaultStartOpts :: StartOpts
defaultStartOpts = StartOpts { detachKeys = DefaultDetachKey }

data DeleteOpts = DeleteOpts {
                  deleteVolumes :: Bool -- ^ Automatically cleanup volumes that the container created as well.
                , force         :: Bool -- ^ If the container is still running force deletion anyway.
                } deriving (Eq, Show)

defaultDeleteOpts :: DeleteOpts
defaultDeleteOpts = DeleteOpts { deleteVolumes = False, force = False }

type Timestamp = Integer
data TailLogOpt = Tail Integer | All deriving (Eq, Show)

data LogOpts = LogOpts {
               stdout     :: Bool
             , stderr     :: Bool
             , since      :: Maybe Timestamp
             , timestamps :: Bool
             , tail       :: TailLogOpt
             } deriving (Eq, Show)

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
newtype Volumes = Volumes [FilePath] deriving (Eq, Show)

instance FromJSON Volumes where
    parseJSON (JSON.Object o) = return $ Volumes $ map T.unpack $ HM.keys o
    parseJSON _ = fail "Volumes is not an object"

instance ToJSON Volumes where
    toJSON (Volumes []) = JSON.Object HM.empty
    toJSON (Volumes (v:vs)) = JSON.Object $ foldl f HM.empty (v:vs)
        where f acc k = HM.insert (T.pack k) (JSON.Object HM.empty) acc

data Volume = Volume { hostSrc          :: Text
                     , containerDest    :: Text
                     , volumePermission :: Maybe VolumePermission
                     } deriving (Eq, Show)

instance FromJSON Volume where
    parseJSON (JSON.String t) = case T.split (== ':') t of
        [src, dest] -> return $ Volume src dest Nothing
        [src, dest, "rw"] -> return $ Volume src dest $ Just ReadWrite
        [src, dest, "ro"] -> return $ Volume src dest $ Just ReadOnly
        _ -> fail "Could not parse Volume"
    parseJSON _ = fail "Volume is not a string"

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

instance ToJSON Volume where
    toJSON (Volume src dest mode) = toJSON $ case mode of
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

newtype LogDriverOptions = LogDriverOptions (M.Map Text Text) deriving (Eq, Show)

instance FromJSON LogDriverOptions where
    parseJSON (JSON.Object o) = do
        LogDriverOptions <$> HM.foldlWithKey' f (return M.empty) o

        where
            f accM k (JSON.String v) = do
                acc <- accM
                return $ M.insert k v acc
            f _ _ _ = fail "Value of LogDriverOptions is not a string"

    parseJSON JSON.Null = return $ LogDriverOptions M.empty
    parseJSON _ = fail "LogDriverOptions is not an object"

instance ToJSON LogDriverOptions where
    toJSON (LogDriverOptions mp) = toJSON mp

data LogDriverConfig = LogDriverConfig LogDriverType (Maybe LogDriverOptions) deriving (Eq, Show)

instance ToJSON LogDriverConfig where
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
newtype PortBindings = PortBindings [PortBinding]
    deriving (Eq, Show)

instance Monoid PortBindings where
    mempty = PortBindings []
    mappend (PortBindings p1) (PortBindings p2) = PortBindings (p1 ++ p2)

-- { <port>/<protocol>: [{ "HostPort": "<port>"  }] }
data PortBinding = PortBinding {
                   containerPort :: Port
                 , portType      :: PortType
                 , hostPorts     :: [HostPort]
                 } deriving (Eq, Show)

portAndType2Text :: Port -> PortType -> Text
portAndType2Text p t = (T.pack $ show p) <> "/" <> (T.pack $ show t)

-- | A convenience function that adds PortBindings to and exiting
-- "CreateOpts" record.  Useful with 'defaultCreateOpts'
-- Example:
--
-- >>> let pb = PortBinding 80 TCP [HostPort "0.0.0.0" 8000]
-- >>> addPortBinding (defaultCreateOpts "nginx:latest") pb
addPortBinding :: CreateOpts -> PortBinding -> CreateOpts
addPortBinding c pb = c{hostConfig=hc{portBindings=pbs <> PortBindings [pb]}}
    where hc = hostConfig c
          pbs = portBindings $ hostConfig c

instance ToJSON PortBinding where
    toJSON (PortBinding {..}) = object [portAndType2Text containerPort portType .= hostPorts]

instance FromJSON PortBindings where
    parseJSON (JSON.Object o) = do
        PortBindings <$> HM.foldlWithKey' f (return []) o

        where
            f accM k v = case T.split (== '/') k of
                [port', portType'] -> do
                    port <- parseIntegerText port'
                    portType <- parseJSON $ JSON.String portType'
                    acc <- accM
                    hps <- parseJSON v
                    return $ (PortBinding port portType hps):acc
                _ ->
                    fail "Could not parse PortBindings"
    parseJSON _ = fail "PortBindings is not an object"

instance ToJSON PortBindings where
    toJSON (PortBindings []) = JSON.Object HM.empty
    toJSON (PortBindings (p:ps)) = JSON.Object $ foldl f HM.empty (p:ps)
        where mKey p = portAndType2Text (containerPort p) (portType p)
              mVal p = toJSON $ hostPorts p
              f acc p = HM.insert (mKey p) (mVal p) acc

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
                { binds           :: [Volume]
                , containerIDFile :: Maybe FilePath -- 1.24: Only in responses, not create
                , logConfig       :: LogDriverConfig
                , networkMode     :: NetworkMode
                , portBindings    :: PortBindings
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
                        , kernelMemory         :: Maybe Integer
                        , memory               :: Maybe Integer
                        , memoryReservation    :: Maybe Integer
                        , memorySwap           :: Maybe Integer
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

-- | ExposedPorts represent a enumeraton of all the ports (and their type)
-- that a container should expose to other containers or the host system.
-- `NOTE`: This does not automatically expose the ports onto the host
-- system but rather it just tags them. It's best to be used with
-- the PublishAllPorts flag. It is also useful for
-- the daemon to know which Environment variables to
-- inject into a container linking to our container.
-- Example linking a Postgres container named db would inject the following
-- environment variables automatically if we set the corresponding
--
-- ExposedPorts:
--
-- @
-- DB_PORT_5432_TCP_PORT="5432"
-- DB_PORT_5432_TCP_PROTO="tcp"
-- DB_PORT_5432_TCP="tcp://172.17.0.1:5432"
-- @
newtype ExposedPorts = ExposedPorts (M.Map Port PortType) deriving (Eq, Show)
-- JP: Should this be ExposedPorts [(Port, PortType)]?

instance FromJSON ExposedPorts where
    parseJSON (JSON.Object o) = do
        ExposedPorts <$> HM.foldlWithKey' f (return M.empty) o

        where
            f accM k _ = case T.split (== '/') k of
                [port', portType'] -> do
                    port <- parseIntegerText port'
                    portType <- parseJSON $ JSON.String portType'
                    acc <- accM
                    return $ M.insert port portType acc
                _ ->
                    fail "Could not parse ExposedPorts"
    parseJSON _  = fail "ExposedPorts is not an object."

instance ToJSON ExposedPorts where
    toJSON (ExposedPorts kvs) =  object [((T.pack $ show p) <> "/" <> (T.pack $ show t)) .= JSON.Object HM.empty | (p,t) <- (M.toList kvs)]


data ContainerConfig = ContainerConfig {
                       hostname        :: Maybe Text
                     , domainname      :: Maybe Text
                     , user            :: Maybe Text
                     , attachStdin     :: Bool
                     , attachStdout    :: Bool
                     , attachStderr    :: Bool
                     , exposedPorts    :: Maybe ExposedPorts -- Note: Should we expand the JSON instance and take away the Maybe?
                     -- , publishService  :: Text -- Don't see this in 1.24
                     , tty             :: Bool
                     , openStdin       :: Bool
                     , stdinOnce       :: Bool
                     , env             :: [EnvVar]
                     , cmd             :: [Text]
                     -- , argsEscaped     :: Bool -- Don't see this in 1.24
                     , image           :: Text
                     , volumes         :: Maybe Volumes
                     , workingDir      :: Maybe FilePath
                     , entrypoint      :: Maybe Text -- Can be null?
                     , networkDisabled :: Maybe Bool -- Note: Should we expand the JSON instance and take away the Maybe? Null is False?
                     , macAddress      :: Maybe Text
                     -- , onBuild         :: Maybe Text -- For 1.24, only see this in the inspect response.
                     , labels          :: Maybe Labels
                     , stopSignal      :: Signal
                     } deriving (Eq, Show, Generic)

instance ToJSON ContainerConfig where
    toJSON = genericToJSON defaultOptions {
         fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

instance FromJSON ContainerConfig where
    parseJSON = genericParseJSON defaultOptions {
         fieldLabelModifier = (\(x:xs) -> toUpper x : xs)}

parseIntegerText :: (Monad m) => Text -> m Integer
parseIntegerText t = case readMaybe $ T.unpack t of
    Nothing ->
        fail "Could not parse Integer"
    Just i ->
        return i
