module Network.Docker.Options where

import           Data.Aeson.TH
import           Network.Docker.Utils

dopts :: Options
dopts = defaultOptions {
      fieldLabelModifier = strip_underscore
    }



