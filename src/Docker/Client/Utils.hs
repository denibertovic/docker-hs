{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Docker.Client.Utils where

import qualified Codec.Archive.Tar           as Tar
import qualified Codec.Compression.GZip      as GZip
import           Control.Monad               (filterM)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy        as BS
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           System.Directory            (doesDirectoryExist, doesFileExist,
                                              findFile, getTemporaryDirectory)
import           System.FilePath             (isAbsolute, makeRelative, (</>))
import           System.FilePath.Find        (always, find)
import           System.FilePath.GlobPattern ((~~))
import           System.IO.Error             (ioError, tryIOError)
-- import           System.IO.Temp              (withSystemTempDirectory)
--
import           Docker.Client.Http

newtype ExclusionPattern = ExclusionPattern T.Text deriving (Eq, Show)
newtype InclusionPattern = InclusionPattern T.Text deriving (Eq, Show)

data DockerIgnore = DockerIgnore { exclusionPatterns :: [ExclusionPattern]
                                 , inclusionPatterns :: [InclusionPattern]
                                 } deriving (Eq, Show)

newtype BuildContextRootDir = BuildContextRootDir FilePath deriving (Eq, Show)

makeBuildContext :: forall m. MonadIO m => BuildContextRootDir -> m (Either DockerError FilePath)
makeBuildContext base = liftIO $ tryIOError (makeBuildContext' base) >>= \res -> case res of
    Left e -> return $ Left $ DockerClientError (T.pack $ show e)
    Right c -> return $ Right c

makeBuildContext' :: forall m. MonadIO m => BuildContextRootDir -> m FilePath
makeBuildContext' (BuildContextRootDir base) = do
    uuid <- liftIO $ UUID.nextRandom
    fs <- liftIO $ getBuildContext $ BuildContextRootDir base
    let relFs = map (makeRelative base) fs
    tmpDir <- liftIO $ getTemporaryDirectory
    let tmpF = tmpDir </> ("docker.context-" <> (UUID.toString uuid)  <> ".tar.gz")
    liftIO $ BS.writeFile tmpF . GZip.compress . Tar.write =<< Tar.pack base relFs
    return tmpF

filterIfDockerIgnoreFile :: BuildContextRootDir -> [FilePath] -> IO [FilePath]
filterIfDockerIgnoreFile base fs = do
    di <- findFile fs ".dockerignore"
    case di of
        Nothing -> return fs
        Just f -> do
            c <- TIO.readFile f
            return $ filterFiles base fs (parseDockerIgnoreFile c)

parseDockerIgnoreFile :: T.Text -> DockerIgnore
parseDockerIgnoreFile c = DockerIgnore{ exclusionPatterns=parseExclusions
                                      , inclusionPatterns=parseInclusions}
    where lines = filter (not . T.isPrefixOf "#") (T.lines c) -- Ignore comments
          parseExclusions = map ExclusionPattern $ filter (not . T.isPrefixOf "!") lines
          parseInclusions = map InclusionPattern $ filter (T.isPrefixOf "!") lines

filterFiles :: BuildContextRootDir -> [FilePath] -> DockerIgnore -> [FilePath]
filterFiles (BuildContextRootDir base) fs d = filter ft relativePaths
    where excls :: [ExclusionPattern]
          excls = exclusionPatterns d
          incls :: [InclusionPattern]
          incls = map (\(InclusionPattern p) -> InclusionPattern $ T.tail p) (inclusionPatterns d)
          ft ::  FilePath -> Bool
          ft f = case (exclusionCheck f, inclusionCheck f) of
                     -- If it's in any of the exclusion patterns but also
                     -- in any of the inclusion patterns then laeave it
                     (True, True)   -> True
                     (True, False)  -> False
                     (False, True)  -> True
                     (False, False) -> True
          normalizeFileName f = T.unpack $ last $ T.split (== '/') f
          exclusionCheck :: FilePath -> Bool
          exclusionCheck f = any id (map (\(ExclusionPattern p) -> (normalizeFileName $ T.pack f) ~~ (T.unpack p)) excls)
          inclusionCheck :: FilePath -> Bool
          inclusionCheck f = any id (map (\(InclusionPattern p) -> (normalizeFileName $ T.pack f) ~~ (T.unpack p)) incls)
          relativePaths :: [FilePath]
          relativePaths = map (makeRelative base) fs

getBuildContext :: BuildContextRootDir -> IO [FilePath]
getBuildContext (BuildContextRootDir base) = do
    -- The base dir needs to be a path to a directory and not a path to
    -- a file
    exists <- doesDirectoryExist base
    let abs = isAbsolute base
    if (exists && abs) then return () else fail "Path to context needs to be a directory that: exists, is readable, and is an absolute path."
    -- This will traverse the directory recursively
    fs <- find always always base
    -- Filter out empty dirs
    fs' <- filterM doesFileExist fs
    -- For some reason base is in there as well and we don't need that
    return $ filter (not . (==) base) fs'

