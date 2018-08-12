{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Docker.Client.Types.Util where

import           Data.Aeson
import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Read           (readMaybe)


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
