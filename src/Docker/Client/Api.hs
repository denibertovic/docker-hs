{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Client.Api (
    -- * Containers
      listContainers
    , createContainer
    , startContainer
    , stopContainer
    , waitContainer
    , killContainer
    , restartContainer
    , pauseContainer
    , unpauseContainer
    , deleteContainer
    , inspectContainer
    , getContainerLogs
    , getContainerLogsStream
    -- * Images
    , listImages
    , deleteImage
    , buildImageFromDockerfile
    , pullImage
    -- * Network
    , createNetwork
    , removeNetwork
    , listNetworks
    -- * Other
    , getDockerVersion
    ) where

import           Control.Monad.Catch    (MonadMask (..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (ask, lift)
import           Data.Aeson             (FromJSON, eitherDecode')
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Conduit           (Sink)
import qualified Data.Conduit.Binary    as Conduit
import qualified Data.Text              as T
import qualified Data.Text              as Text
import           Network.HTTP.Client    (responseStatus)
import           Network.HTTP.Types     (StdMethod (..))
import           System.Exit            (ExitCode (..))

import           Docker.Client.Http
import           Docker.Client.Types
import           Docker.Client.Utils

requestUnit :: (MonadIO m, MonadMask m) => HttpVerb -> Endpoint -> DockerT m (Either DockerError ())
requestUnit verb endpoint = fmap (const ()) <$> requestHelper verb endpoint

requestHelper :: (MonadIO m, MonadMask m) => HttpVerb -> Endpoint -> DockerT m (Either DockerError BSL.ByteString)
requestHelper verb endpoint = requestHelper' verb endpoint Conduit.sinkLbs

requestHelper' :: (MonadIO m, MonadMask m) => HttpVerb -> Endpoint -> Sink BS.ByteString m a -> DockerT m (Either DockerError a)
requestHelper' verb endpoint sink = do
    (opts, HttpHandler httpHandler) <- ask
    case mkHttpRequest verb endpoint opts of
        Nothing ->
            return $ Left $ DockerInvalidRequest endpoint
        Just request -> do
            -- JP: Do we need runResourceT?
            -- lift $ NHS.httpSink request $ \response ->
            lift $ httpHandler request $ \response ->
                -- Check status code.
                let status = responseStatus response in
                case statusCodeToError endpoint status of
                    Just err ->
                        return $ Left err
                    Nothing ->
                        fmap Right sink

parseResponse :: (FromJSON a, Monad m) => Either DockerError BSL.ByteString -> DockerT m (Either DockerError a)
parseResponse (Left err) =
    return $ Left err
parseResponse (Right response) =
    -- Parse request body.
    case eitherDecode' response of
        Left err ->
            return $ Left $ DockerClientDecodeError $ Text.pack err
        Right r ->
            return $ Right r

-- | Gets the version of the docker engine remote API.
getDockerVersion :: forall m. (MonadIO m, MonadMask m) => DockerT m (Either DockerError DockerVersion)
getDockerVersion = requestHelper GET VersionEndpoint >>= parseResponse

-- | Lists all running docker containers. Pass in @'defaultListOpts' {all
-- = True}@ to get a list of stopped containers as well.
listContainers :: forall m. (MonadIO m, MonadMask m) => ListOpts -> DockerT m (Either DockerError [Container])
listContainers opts = requestHelper GET (ListContainersEndpoint opts) >>= parseResponse

-- | Lists all docker images.
listImages :: forall m. (MonadIO m, MonadMask m) => ListOpts -> DockerT m (Either DockerError [Image])
listImages opts = requestHelper GET (ListImagesEndpoint opts) >>= parseResponse

-- | Deletes an image with the given 'ImageID'.
-- See "ImageDeleteOpts" for options and use 'defaultImageDeleteOpts' for sane
-- defaults.
deleteImage :: forall m. (MonadIO m, MonadMask m) => ImageDeleteOpts -> ImageID -> DockerT m (Either DockerError ())
deleteImage dopts iid = requestUnit DELETE $ DeleteImageEndpoint dopts iid

-- | Creates a docker container but does __not__ start it. See
-- 'CreateOpts' for a list of options and you can use 'defaultCreateOpts'
-- for some sane defaults.
createContainer :: forall m. (MonadIO m, MonadMask m) => CreateOpts -> Maybe ContainerName -> DockerT m (Either DockerError ContainerID)
createContainer opts cn = requestHelper POST (CreateContainerEndpoint opts cn) >>= parseResponse

-- | Start a container from a given 'ContainerID' that we get from
-- 'createContainer'. See 'StartOpts' for a list of configuration options
-- for starting a container. Use 'defaultStartOpts' for sane defaults.
startContainer :: forall m. (MonadIO m, MonadMask m) => StartOpts -> ContainerID -> DockerT m (Either DockerError ())
startContainer sopts cid = requestUnit POST $ StartContainerEndpoint sopts cid

-- | Attempts to stop a container with the given 'ContainerID' gracefully
-- (SIGTERM).
-- The docker daemon will wait for the given 'Timeout' and then send
-- a SIGKILL killing the container.
stopContainer :: forall m. (MonadIO m, MonadMask m) => Timeout -> ContainerID -> DockerT m (Either DockerError ())
stopContainer t cid = requestUnit POST $ StopContainerEndpoint t cid

-- | Blocks until a container with the given 'ContainerID' stops,
-- then returns the exit code
waitContainer :: forall m. (MonadIO m, MonadMask m) => ContainerID -> DockerT m (Either DockerError ExitCode)
waitContainer cid = fmap (fmap statusCodeToExitCode) (requestHelper POST (WaitContainerEndpoint cid) >>= parseResponse)
  where
    statusCodeToExitCode (StatusCode 0) = ExitSuccess
    statusCodeToExitCode (StatusCode x) = ExitFailure x

-- | Sends a 'Signal' to the container with the given 'ContainerID'. Same
-- as 'stopContainer' but you choose the signal directly.
killContainer :: forall m. (MonadIO m, MonadMask m) => Signal -> ContainerID -> DockerT m (Either DockerError ())
killContainer s cid = requestUnit POST $ KillContainerEndpoint s cid

-- | Restarts a container with the given 'ContainerID'.
restartContainer :: forall m. (MonadIO m, MonadMask m) => Timeout -> ContainerID -> DockerT m (Either DockerError ())
restartContainer t cid = requestUnit POST $ RestartContainerEndpoint t cid

-- | Pauses a container with the given 'ContainerID'.
pauseContainer :: forall m. (MonadIO m, MonadMask m) => ContainerID -> DockerT m (Either DockerError ())
pauseContainer cid = requestUnit POST $ PauseContainerEndpoint cid

-- | Unpauses a container with the given 'ContainerID'.
unpauseContainer :: forall m. (MonadIO m, MonadMask m) => ContainerID -> DockerT m (Either DockerError ())
unpauseContainer cid = requestUnit GET $ UnpauseContainerEndpoint cid

-- | Deletes a container with the given 'ContainerID'.
-- See "ContainerDeleteOpts" for options and use 'defaultContainerDeleteOpts' for sane
-- defaults.
deleteContainer :: forall m. (MonadIO m, MonadMask m) => ContainerDeleteOpts -> ContainerID -> DockerT m (Either DockerError ())
deleteContainer dopts cid = requestUnit DELETE $ DeleteContainerEndpoint dopts cid

-- | Gets 'ContainerDetails' for a given 'ContainerID'.
inspectContainer :: forall m . (MonadIO m, MonadMask m) => ContainerID -> DockerT m (Either DockerError ContainerDetails)
inspectContainer cid = requestHelper GET (InspectContainerEndpoint cid) >>= parseResponse

-- | Get's container's logs for a given 'ContainerID'.
-- This will only work with the 'JsonFile' 'LogDriverType' as the other driver types disable
-- this endpoint and it will return a 'DockerError'.
--
-- See 'LogOpts' for options that you can pass and
-- 'defaultLogOpts' for sane defaults.
--
-- __NOTE__: his function will fetch the entire log that the
-- container produced in the json-file on disk. Depending on the logging
-- setup of the process in your container this can be a significant amount
-- which might block your application...so use with caution.
--
-- If you want to stream the logs from the container continuosly then use
-- 'getContainerLogsStream'
--
-- __NOTE__: It's recommended to use one of the other 'LogDriverType's available (like
-- syslog) for creating your containers.
getContainerLogs ::  forall m. (MonadIO m, MonadMask m) => LogOpts -> ContainerID -> DockerT m (Either DockerError BSL.ByteString)
getContainerLogs logopts cid = requestHelper GET (ContainerLogsEndpoint logopts False cid)

{-| Continuously gets the container's logs as a stream. Uses conduit.

__Example__:

@
>>> import Docker.Client
>>> import Data.Maybe
>>> import Conduit
>>> h <- defaultHttpHanlder
>>> let cid = fromJust $ toContainerID "fee86e1d522b"
>>> runDockerT (defaultClientOpts, h) $ getContainerLogsStream defaultLogOpts cid stdoutC
@

-}
getContainerLogsStream :: forall m b . (MonadIO m, MonadMask m) => LogOpts -> ContainerID -> Sink BS.ByteString m b -> DockerT m (Either DockerError b)
getContainerLogsStream logopts cid = requestHelper' GET (ContainerLogsEndpoint logopts True cid)
-- JP: Should the second (follow) argument be True? XXX

-- | Build an Image from a Dockerfile
--
-- TODO: Add X-Registry-Config
--
-- TODO: Add support for remote URLs to a Dockerfile
--
-- TODO: Clean up temp tar.gz file after the image is built
buildImageFromDockerfile :: forall m. (MonadIO m, MonadMask m) => BuildOpts -> FilePath -> DockerT m (Either DockerError ())
buildImageFromDockerfile opts base = do
    ctx <- makeBuildContext $ BuildContextRootDir base
    case ctx of
        Left e  -> return $ Left e
        Right c -> requestUnit POST (BuildImageEndpoint opts c)

-- | Pulls an image from Docker Hub (by default).
--
-- TODO: Add support for X-Registry-Auth and pulling from private docker
-- registries.
--
-- TODO: Implement importImage function that uses he same
-- CreateImageEndpoint but rather than pulling from docker hub it imports
-- the image from a tarball or a URL.
pullImage :: forall m b . (MonadIO m, MonadMask m) => T.Text -> Tag -> Sink BS.ByteString m b -> DockerT m (Either DockerError b)
pullImage name tag = requestHelper' POST (CreateImageEndpoint name tag Nothing)

-- | Creates network
createNetwork :: forall m. (MonadIO m, MonadMask m) => CreateNetworkOpts -> DockerT m (Either DockerError NetworkID)
createNetwork opts = requestHelper POST (CreateNetworkEndpoint opts)  >>= parseResponse

-- | Removes a network
removeNetwork :: forall m. (MonadIO m, MonadMask m) => NetworkID -> DockerT m (Either DockerError ())
removeNetwork nid = requestUnit DELETE $ RemoveNetworkEndpoint nid

-- | Lists networks optionally matching a list of 'Filter's
listNetworks :: forall m . (MonadIO m, MonadMask m) => [NetworkFilter] -> DockerT m (Either DockerError [NetworkDetails])
listNetworks nfs = requestHelper GET (ListNetworksEndpoint nfs) >>= parseResponse
