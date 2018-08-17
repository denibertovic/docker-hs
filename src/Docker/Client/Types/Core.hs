{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Docker.Client.Types.Core where

import           Data.Char           (isAlphaNum)
import           Data.Text           (Text)
import qualified Data.Text           as T

type Port = Integer

type Name = Text
type Value = Text

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
