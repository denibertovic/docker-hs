{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module Docker.Client.Types.Util where

import           Data.Aeson
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.TH       as JSON
import           Data.Char
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
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


dropToUnderscoreAndConvertFromCamel :: String -> String
dropToUnderscoreAndConvertFromCamel = toSnake . dropToUnder where
  dropToUnder x | '_' `elem` x = L.drop 1 $ L.dropWhile (/= '_') x
  dropToUnder x = x

dropNAndToSnake :: Int -> String -> String
dropNAndToSnake n = toSnake . drop n

dropToUnderscoreIfPresent :: String -> String
dropToUnderscoreIfPresent s | '_' `elem` s = L.drop 1 $ L.dropWhile (/= '_') s
dropToUnderscoreIfPresent s = s

dropFieldsToUnderscoreAndToSnakeOptions :: JSON.Options
dropFieldsToUnderscoreAndToSnakeOptions = JSON.defaultOptions { JSON.fieldLabelModifier = dropToUnderscoreAndConvertFromCamel }

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case L.break p s' of
      (b', [])     -> [ m:b' ]
      (b', x:xs) -> ( m:b' ) : go x xs
  in case L.break p s of
    (b,  [])    -> [ b ]
    ([], h:t) -> go h t
    (b,  h:t) -> b : go h t

toSnake :: String -> String
toSnake = fmap toLower . L.concat . underscores . splitR isUpper
  where
    underscores :: [String] -> [String]
    underscores [] = []
    underscores (h:t) = h : fmap ('_':) t
