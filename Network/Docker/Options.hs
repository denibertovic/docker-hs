module Network.Docker.Options where

import           Data.Aeson.TH
import           Network.Docker.Utils

dopts :: Options
dopts = Options {
      fieldLabelModifier = strip_underscore
    , constructorTagModifier = id
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , sumEncoding = defaultTaggedObject
    }



