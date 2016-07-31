{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Client (
      runDockerT
    , DockerT
    , DockerError(..)
    , DockerClientOpts(..)
    , defaultClientOpts
    , HttpHandler
    , defaultHttpHandler
    , httpHandler
    , unixHttpHandler
    , clientParamsWithClientAuthentication
    , clientParamsSetCA
    , getDockerVersion
    , listContainers
    , listImages
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

getDockerVersion :: forall m. Monad m => DockerT m (Either DockerError DockerVersion)
getDockerVersion = requestHelper GET VersionEndpoint >>= parseResponse

listContainers :: forall m. Monad m => ListOpts -> DockerT m (Either DockerError [Container])
listContainers opts = requestHelper GET (ListContainersEndpoint opts) >>= parseResponse

listImages :: forall m. Monad m => ListOpts -> DockerT m (Either DockerError [Image])
listImages opts = requestHelper GET (ListImagesEndpoint opts) >>= parseResponse

createContainer :: forall m. Monad m => CreateOpts -> DockerT m (Either DockerError ContainerID)
createContainer opts = requestHelper POST (CreateContainerEndpoint opts) >>= parseResponse

-- TODO: Is this the correct return type? XXX
--  convert status code to unit or error
startContainer :: forall m. Monad m => StartOpts -> ContainerID -> DockerT m (Either DockerError ())
startContainer sopts cid = requestUnit POST $ StartContainerEndpoint sopts cid

stopContainer :: forall m. Monad m => Timeout -> ContainerID -> DockerT m (Either DockerError ())
stopContainer t cid = requestUnit POST $ StopContainerEndpoint t cid

killContainer :: forall m. Monad m => Signal -> ContainerID -> DockerT m (Either DockerError ())
killContainer s cid = requestUnit POST $ KillContainerEndpoint s cid

restartContainer :: forall m. Monad m => Timeout -> ContainerID -> DockerT m (Either DockerError ())
restartContainer t cid = requestUnit POST $ RestartContainerEndpoint t cid

pauseContainer :: forall m. Monad m => ContainerID -> DockerT m (Either DockerError ())
pauseContainer cid = requestUnit POST $ PauseContainerEndpoint cid

unpauseContainer :: forall m. Monad m => ContainerID -> DockerT m (Either DockerError ())
unpauseContainer cid = requestUnit GET $ UnpauseContainerEndpoint cid

deleteContainer :: forall m. Monad m => DeleteOpts -> ContainerID -> DockerT m (Either DockerError ())
deleteContainer dopts cid = requestUnit DELETE $ DeleteContainerEndpoint dopts cid

inspectContainer :: forall m . Monad m => ContainerID -> DockerT m (Either DockerError ContainerDetails)
inspectContainer cid = requestHelper GET (InspectContainerEndpoint cid) >>= parseResponse

getContainerLogs ::  forall m. Monad m => LogOpts -> ContainerID -> DockerT m (Either DockerError BSL.ByteString)
getContainerLogs logopts cid = fmap responseBody <$> requestHelper GET (ContainerLogsEndpoint logopts False cid)

-- TODO: Use http-conduit to output to a sink.
-- getContainerLogsStream :: forall m. Monad m => Sink BSL.ByteString m b -> LogOpts -> ContainerID -> DockerT m (Either DockerError b)
-- getContainerLogsStream sink logopts cid = runResourceT $ do
--  response <- http request manager
--  responseBody response C.$$+- sink

