module Network.Docker.Utils where

import           Data.Char

strip_underscore :: String -> String
strip_underscore (_:xs) = xs
