{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Docker.Client.Types.Stats where


import           Data.Aeson
import qualified Data.Aeson          as JSON
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Text           (Text)
import           Docker.Client.Types.Util
import           GHC.Generics        (Generic)

data ContainerStats = ContainerStats { statsRead :: Text
                                     , statsPidsStats :: PidsStats
                                     , statsNetworks :: Maybe (Maybe (M.Map Text NetworkStats))
                                     , statsMemoryStats :: Maybe MemoryStats
                                     , statsBlkioStats :: BlockIOStats
                                     , statsCpuStats :: CpuStats
                                     , statsPrecpuStats :: CpuStats
                                     } deriving (Generic, Eq, Show)
instance FromJSON ContainerStats where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("stats" :: String)))})

data PidsStats = PidsStats { pidsStatsCurrent :: Maybe Int } deriving (Generic, Eq, Show)
instance FromJSON PidsStats where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("pidsStats" :: String)))})

data NetworkStats = NetworkStats { networkStatsRxBytes :: Int
                                 , networkStatsRxDropped :: Int
                                 , networkStatsRxErrors :: Int
                                 , networkStatsRxPackets :: Int

                                 , networkStatsTxBytes :: Int
                                 , networkStatsTxDropped :: Int
                                 , networkStatsTxErrors :: Int
                                 , networkStatsTxPackets :: Int
                                 } deriving (Generic, Eq, Show)
instance FromJSON NetworkStats where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("networkStats" :: String)))})

data MemoryStats = MemoryStats { memoryStatsDetails :: Maybe JSON.Value
                               , memoryStatsMaxUsage :: Maybe Int
                               , memoryStatsUsage :: Maybe Int
                               , memoryStatsFailcnt :: Maybe Int
                               , memoryStatsLimit :: Maybe Int
                               } deriving (Generic, Eq, Show)
memoryStatsOptions = (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("memoryStats" :: String))) })
instance FromJSON MemoryStats where
  parseJSON = genericParseJSON memoryStatsOptions
instance ToJSON MemoryStats where
  toJSON = genericToJSON memoryStatsOptions

type BlockIOStats = JSON.Value -- TODO

data CpuStats = CpuStats { cpuStatsCpuUsage :: Maybe CpuUsage
                         , cpuStatsThrottlingData :: Maybe ThrottlingData
                         , cpuStatsSystemCpuUsage :: Maybe Int } deriving (Generic, Eq, Show)
instance FromJSON CpuStats where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("cpuStats" :: String)))})

data CpuUsage = CpuUsage { cpuUsagePercpuUsage :: Maybe [Int]
                         , cpuUsageUsageInUsermode :: Int
                         , cpuUsageTotalUsage :: Int
                         , cpuUsageUsageInKernelmode :: Int } deriving (Generic, Eq, Show)
instance FromJSON CpuUsage where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("cpuUsage" :: String)))})

data ThrottlingData = ThrottlingData { throttlingDataPeriods :: Int
                                     , throttlingDataThrottledPeriods :: Int
                                     , throttlingDataThrottledTime :: Int } deriving (Generic, Eq, Show)
instance FromJSON ThrottlingData where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = (dropNAndToSnake (L.length ("throttlingData" :: String)))})
