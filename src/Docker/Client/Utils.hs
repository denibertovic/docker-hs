{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Docker.Client.Utils where

import qualified Codec.Archive.Tar           as Tar
import qualified Codec.Compression.GZip      as GZip
import           Control.Monad               (filterM, liftM, unless)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy        as BS
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           System.Directory            (doesDirectoryExist, doesFileExist,
                                              getTemporaryDirectory)
import           System.FilePath             (isAbsolute, makeRelative, (</>))
import           System.FilePath.Find        (FilterPredicate,
                                              RecursionPredicate, always,
                                              fileName, find, (==?))
import           System.FilePath.GlobPattern ((~~))
import           System.IO.Error             (tryIOError)
-- import           System.IO.Temp              (withSystemTempDirectory)
--
import           Docker.Client.Http

type File = FilePath
data DirTree = DirTree [File] [DirTree]


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
    uuid <- liftIO UUID.nextRandom
    fs <- liftIO $ getBuildContext $ BuildContextRootDir base
    let relFs = map (makeRelative base) fs
    tmpDir <- liftIO getTemporaryDirectory
    let tmpF = tmpDir </> "docker.context-" <> UUID.toString uuid  <> ".tar.gz"
    liftIO $ BS.writeFile tmpF . GZip.compress . Tar.write =<< Tar.pack base relFs
    return tmpF

parseDockerIgnoreFile :: T.Text -> DockerIgnore
parseDockerIgnoreFile c = DockerIgnore{ exclusionPatterns=parseExclusions
                                      , inclusionPatterns=parseInclusions}
    where lines = filter (not . T.isPrefixOf "#") (T.lines c) -- Ignore comments
          parseExclusions = map ExclusionPattern $ filter (\l -> not (T.isPrefixOf "!" l) && (l /= "")) lines
          parseInclusions = map (InclusionPattern . T.drop 1) $ filter (\l -> T.isPrefixOf "!" l && (l /= "")) lines

getBuildContext :: BuildContextRootDir -> IO [FilePath]
getBuildContext (BuildContextRootDir base) = do
    -- The base dir needs to be a path to a directory and not a path to
    -- a file
    exists <- doesDirectoryExist base
    let abs = isAbsolute base
    unless (exists && abs) $ fail "Path to context needs to be a directory that: exists, is readable, and is an absolute path."
    di <- find always (fileName ==? ".dockerignore") base
    dockerignore <- case di of
        [] -> return $ DockerIgnore [] []
        -- This should not return more than one result though
        (x:_) -> do
            c <- TIO.readFile x
            return $ parseDockerIgnoreFile c
    -- This will traverse the directory recursively
    fs <- find (shouldRecurse dockerignore) (shouldInclude dockerignore) base
    -- fs is a list of directories *and* files in those directories. So
    -- an example result would look like ["/tmp/project/files",
    -- "/tmp/project/files/file1.txt"] and we want just the individual
    -- files otherwise tar duplicates them when making the archive.
    fs' <- filterM doesFileExist fs
    -- For some reason base is in there as well and we don't need that
    return $ filter (not . (==) base) fs'

shouldInclude :: DockerIgnore -> FilterPredicate
shouldInclude d = check `liftM` fileName
    where check f = dockerIgnoreDecision (exclusionCheck f (exclusionPatterns d), inclusionCheck f (inclusionPatterns d))

shouldRecurse :: DockerIgnore -> RecursionPredicate
shouldRecurse d = check `liftM` fileName
    where check f = dockerIgnoreDecision (exclusionCheck f (exclusionPatterns d), inclusionCheck f (inclusionPatterns d))

-- TODO: We don't handle precedence rules. For instance a dockerignore file
-- like this:
--
-- *.md
-- !README*.md
-- README-secret.md
--
-- Should result in no markdown files being included in the context except README files other
-- than README-secret.md. FIXME: OUR implementation would in fact include
-- README-secret.md as well!!!
--
-- Whereas this:
--
-- *.md
-- README-secret.md
-- !README*.md
--
-- Should result in all of the README files being included. The middle line has no effect
-- because !README*.md matches README-secret.md and comes last. Our
-- implementation will result in the same thing.
dockerIgnoreDecision :: (Bool, Bool) -> Bool
dockerIgnoreDecision p = case p of
         -- If it's in any of the exclusion patterns but also
         -- in any of the inclusion patterns then laeave it
         (True, True)   -> True
         (True, False)  -> False
         (False, True)  -> True
         (False, False) -> True

exclusionCheck :: FilePath -> [ExclusionPattern] -> Bool
exclusionCheck f ps = any id (map (\(ExclusionPattern p) -> f ~~ T.unpack p) ps)

inclusionCheck :: FilePath -> [InclusionPattern] -> Bool
inclusionCheck f ps = any id (map (\(InclusionPattern p) -> f ~~ T.unpack p) ps)

