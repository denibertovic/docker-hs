{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Docker.Client.Types.Network where


import           Data.Aeson
import qualified Data.Aeson          as JSON
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import           Data.Char           (toLower)
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           Docker.Client.Types.Core
import           Docker.Client.Types.Util
import           GHC.Generics        (Generic)
import           Prelude             hiding (all, tail)

-- | Alias for endpoint name
type EndpointName = Text

-- | Alias for Aliases.
type Alias = Text

data NetworkFilterType = NetworkFilterTypeCustom | NetworkFilterTypeBuiltIn
  deriving (Eq, Show)

instance ToJSON NetworkFilterType where
    toJSON NetworkFilterTypeCustom = JSON.object [("custom", JSON.Bool True)]
    toJSON NetworkFilterTypeBuiltIn = JSON.object [("builtin", JSON.Bool True)]


data NetworkFilterOpts = NetworkFilterOpts { networkFilterDriver :: Maybe Text
                                           , networkFilterId :: Maybe Text
                                           , networkFilterLabel :: Maybe Text
                                           , networkFilterName :: Maybe Text
                                           , networkFilterType :: Maybe NetworkFilterType }
  deriving (Eq, Show, Generic)

instance ToJSON NetworkFilterOpts where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = (fmap toLower) . (L.drop (L.length ("networkFilter" :: String))) }

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

data NetworkContainer = NetworkContainer { networkContainerEndpointID :: Text
                                         , networkContainerMacAddress :: Text
                                         , networkContainerIPv4Address :: Text
                                         , networkContainerIPv6Address :: Text
                                         } deriving (Eq, Show, Generic)

instance FromJSON NetworkContainer where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = L.drop (L.length ("networkContainer" :: String)) }

data IPAM = IPAM { ipamDriver :: Text
                 , ipamConfig :: Maybe [IPAMConfig]
                 , ipamOptions :: Maybe (M.Map Text Text) -- Note: this turns into a list of maps
                 } deriving (Eq, Show, Generic)

instance FromJSON IPAM where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = L.drop (L.length ("ipam" :: String)) }

data IPAMConfig = IPAMConfig { ipamConfigSubnet :: Maybe Text -- TODO: CIDR, parse to Data.IP.AddrRange?
                             , ipamConfigIPRange :: Maybe Text -- TODO: CIDR, parse to Data.IP.AddrRange?
                             , ipamConfigGateway :: Maybe Text
                             , ipamConfigAuxAddress :: Maybe Text } deriving (Eq, Show, Generic)

instance FromJSON IPAMConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = L.drop (L.length ("ipamConfig" :: String)) }

data NetworkDefinition = NetworkDefinition { networkDefinitionName :: Text
                                           , networkDefinitionId :: Text
                                           , networkDefinitionCreated :: UTCTime
                                           , networkDefinitionScope :: Text
                                           , networkDefinitionDriver :: Text
                                           , networkDefinitionEnableIPv6 :: Bool
                                           , networkDefinitionIPAM :: IPAM
                                           , networkDefinitionInternal :: Bool
                                           , networkDefinitionContainers :: M.Map Text NetworkContainer
                                           , networkDefinitionOptions :: M.Map Text Text
                                           , networkDefinitionLabels :: M.Map Text Text
                                           } deriving (Eq, Show, Generic)

instance FromJSON NetworkDefinition where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = L.drop (L.length ("networkDefinition" :: String)) }

-- | EndpointsConfig is container configuration for a specific network
newtype EndpointConfig = EndpointConfig [Alias] deriving (Eq, Show)

instance ToJSON EndpointConfig where
  toJSON (EndpointConfig aliases) = JSON.object [ "Aliases" .= aliases ]

-- | Data type for the NetworkingConfig section of the container settings
newtype NetworkingConfig = NetworkingConfig
  { endpointsConfig :: HM.HashMap EndpointName EndpointConfig
  } deriving (Eq, Show)

instance ToJSON NetworkingConfig where
  toJSON (NetworkingConfig endpointsConfig) = JSON.object
    [ "EndpointsConfig" .= endpointsConfig
    ]

-- | Options for creating a network
data CreateNetworkOpts = CreateNetworkOpts
  { createNetworkName           :: Text -- ^ The network's name
  , createNetworkCheckDuplicate :: Bool -- ^ Check for networks with duplicate names.
  , createNetworkDriver         :: Text -- ^ Name of the network driver plugin to use.
  , createNetworkInternal       :: Bool -- ^ Restrict external access to the network.
  , createNetworkEnableIPv6     :: Bool -- ^ Enable IPv6 on the network.
  } deriving (Eq, Show)

-- | Sensible defalut for create network options
defaultCreateNetworkOpts :: Text -> CreateNetworkOpts
defaultCreateNetworkOpts name =
  CreateNetworkOpts
  { createNetworkName = name
  , createNetworkCheckDuplicate = False
  , createNetworkDriver = "bridge"
  , createNetworkInternal = True
  , createNetworkEnableIPv6 = False
  }

instance ToJSON CreateNetworkOpts where
  toJSON opts =
    object
      [ "Name" .= createNetworkName opts
      , "CheckDuplicate" .= createNetworkCheckDuplicate opts
      , "Driver" .= createNetworkDriver opts
      , "Internal" .= createNetworkInternal opts
      , "EnableIPv6" .= createNetworkEnableIPv6 opts
      ]

-- TODO: Add container:<name|id> mode
data NetworkMode = NetworkBridge | NetworkHost | NetworkDisabled | NetworkNamed Text
    deriving (Eq, Show, Ord)

instance FromJSON NetworkMode where
    parseJSON (JSON.String "bridge") = return NetworkBridge
    parseJSON (JSON.String "host")   = return NetworkHost -- Note: Guessing on these.
    parseJSON (JSON.String "none")   = return NetworkDisabled
    parseJSON (JSON.String n)        = return $ NetworkNamed n
    parseJSON _                      = fail "Unknown NetworkMode"

instance ToJSON NetworkMode where
    toJSON NetworkBridge    = JSON.String "bridge"
    toJSON NetworkHost      = JSON.String "host"
    toJSON NetworkDisabled  = JSON.String "none"
    toJSON (NetworkNamed n) = JSON.String n

newtype NetworkID = NetworkID Text
    deriving (Eq, Show)

-- | Used for extracting the id of the container from the newtype
fromNetworkID :: NetworkID -> Text
fromNetworkID (NetworkID t) = t

-- | Used for parsing a Text value into a NetworkID.
toNetworkID :: Text -> Maybe NetworkID
toNetworkID t = Just $ NetworkID t

instance FromJSON NetworkID where
  parseJSON (JSON.Object o) = do
    nid <- o .: "Id"
    return $ NetworkID nid
  parseJSON _ = fail "NetworkID is not an object."

instance ToJSON NetworkID where
  toJSON (NetworkID nid) = object ["Id" .= nid]

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
                    _     -> fail "PortType: Invalid port type."

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

portAndType2Text :: Port -> PortType -> Text
portAndType2Text p t = (T.pack $ show p) <> "/" <> (T.pack $ show t)


data HostPort = HostPort {
      hostIp   :: Text
    , hostPort :: Port
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
