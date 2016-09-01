{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Client.Api (
    -- * Containers
      listContainers
    , createContainer
    , startContainer
    , stopContainer
    , killContainer
    , restartContainer
    , pauseContainer
    , unpauseContainer
    , deleteContainer
    , inspectContainer
    , getContainerLogs
    -- * Images
    , listImages
    -- * Other
    , getDockerVersion
    ) where

import           Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import           Control.Monad.Reader (ask, lift)
import           Data.Aeson           (FromJSON, eitherDecode')
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as Text
import           Network.HTTP.Client  (responseBody, responseStatus)
import           Network.HTTP.Types   (StdMethod (..))

import           Docker.Client.Http
import           Docker.Client.Types

requestUnit :: (Monad m) => HttpVerb -> Endpoint -> DockerT m (Either DockerError ())
requestUnit verb endpoint = const (Right ()) <$> requestHelper verb endpoint

requestHelper :: (Monad m) => HttpVerb -> Endpoint -> DockerT m (Either DockerError Response)
requestHelper verb endpoint = runExceptT $ do
    (opts, httpHandler) <- lift ask
    case mkHttpRequest verb endpoint opts of
        Nothing ->
            throwError $ DockerInvalidRequest endpoint
        Just request -> do
            response <- ExceptT $ lift $ httpHandler request

            -- Check status code.
            let status = responseStatus response
            maybe (return ()) throwError $
                statusCodeToError endpoint status

            return response

parseResponse :: (FromJSON a, Monad m) => Either DockerError Response -> DockerT m (Either DockerError a)
parseResponse (Left err) =
    return $ Left err
parseResponse (Right response) =
    -- Parse request body.
    case eitherDecode' $ responseBody response of
        Left err ->
            return $ Left $ DockerClientDecodeError $ Text.pack err
        Right r ->
            return $ Right r

-- | Gets the version of the docker engine remote API.
getDockerVersion :: forall m. Monad m => DockerT m (Either DockerError DockerVersion)
getDockerVersion = requestHelper GET VersionEndpoint >>= parseResponse

-- | Lists all running docker containers. Pass in @'defaultListOpts' {all
-- = True}@ to get a list of stopped containers as well.
listContainers :: forall m. Monad m => ListOpts -> DockerT m (Either DockerError [Container])
listContainers opts = requestHelper GET (ListContainersEndpoint opts) >>= parseResponse

-- | Lists all docker images.
listImages :: forall m. Monad m => ListOpts -> DockerT m (Either DockerError [Image])
listImages opts = requestHelper GET (ListImagesEndpoint opts) >>= parseResponse

-- | Creates a docker container but does __not__ start it. See
-- 'CreateOpts' for a list of options and you can use 'defaultCreateOpts'
-- for some sane defaults.
createContainer :: forall m. Monad m => CreateOpts -> Maybe ContainerName -> DockerT m (Either DockerError ContainerID)
createContainer opts cn = requestHelper POST (CreateContainerEndpoint opts cn) >>= parseResponse

-- | Start a container from a given 'ContainerID' that we get from
-- 'createContainer'. See 'StartOpts' for a list of configuration options
-- for starting a container. Use 'defaultStartOpts' for sane defaults.
startContainer :: forall m. Monad m => StartOpts -> ContainerID -> DockerT m (Either DockerError ())
startContainer sopts cid = requestUnit POST $ StartContainerEndpoint sopts cid

-- | Attempts to stop a container with the given 'ContainerID' gracefully
-- (SIGTERM).
-- The docker daemon will wait for the given 'Timeout' and then send
-- a SIGKILL killing the container.
stopContainer :: forall m. Monad m => Timeout -> ContainerID -> DockerT m (Either DockerError ())
stopContainer t cid = requestUnit POST $ StopContainerEndpoint t cid

-- | Sends a 'Signal' to the container with the given 'ContainerID'. Same
-- as 'stopContainer' but you choose the signal directly.
killContainer :: forall m. Monad m => Signal -> ContainerID -> DockerT m (Either DockerError ())
killContainer s cid = requestUnit POST $ KillContainerEndpoint s cid

-- | Restarts a container with the given 'ContainerID'.
restartContainer :: forall m. Monad m => Timeout -> ContainerID -> DockerT m (Either DockerError ())
restartContainer t cid = requestUnit POST $ RestartContainerEndpoint t cid

-- | Pauses a container with the given 'ContainerID'.
pauseContainer :: forall m. Monad m => ContainerID -> DockerT m (Either DockerError ())
pauseContainer cid = requestUnit POST $ PauseContainerEndpoint cid

-- | Unpauses a container with the given 'ContainerID'.
unpauseContainer :: forall m. Monad m => ContainerID -> DockerT m (Either DockerError ())
unpauseContainer cid = requestUnit GET $ UnpauseContainerEndpoint cid

-- | Deletes a container with the given 'ContainerID'.
-- See "DeleteOpts" for options and use 'defaultDeleteOpts' for sane
-- defaults.
deleteContainer :: forall m. Monad m => DeleteOpts -> ContainerID -> DockerT m (Either DockerError ())
deleteContainer dopts cid = requestUnit DELETE $ DeleteContainerEndpoint dopts cid

-- | Gets 'ContainerDetails' for a given 'ContainerID'.
inspectContainer :: forall m . Monad m => ContainerID -> DockerT m (Either DockerError ContainerDetails)
inspectContainer cid = requestHelper GET (InspectContainerEndpoint cid) >>= parseResponse

-- | Get's container's logs for a given 'ContainerID'.
-- This will only work with the 'JsonFile' 'LogDriverType' as the other driver types disable
-- this endpoint and it will return a 'DockerError'.
--
-- See 'LogOpts' for options that you can pass and
-- 'defaultLogOpts' for sane defaults.
--
-- __NOTE__: Currently streaming logs is
-- not supported and this function will fetch the entire log that the
-- container produced in the json-file on disk. Depending on the logging
-- setup of the process in your container this can be a significant amount
-- which might block your application...so use with caution.
--
-- The recommended method is to use one of the other 'LogDriverType's available (like
-- syslog) for creating your containers.
getContainerLogs ::  forall m. Monad m => LogOpts -> ContainerID -> DockerT m (Either DockerError BSL.ByteString)
getContainerLogs logopts cid = fmap responseBody <$> requestHelper GET (ContainerLogsEndpoint logopts False cid)

-- TODO: Use http-conduit to output to a sink.
-- getContainerLogsStream :: forall m. Monad m => Sink BSL.ByteString m b -> LogOpts -> ContainerID -> DockerT m (Either DockerError b)
-- getContainerLogsStream sink logopts cid = runResourceT $ do
--  response <- http request manager
--  responseBody response C.$$+- sink

