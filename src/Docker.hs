{-# LANGUAGE ExplicitForAll #-}

module Docker where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad.Catch
import           Control.Monad.Except   (catchError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ask)
import           Control.Monad.Reader   (lift)
import           Data.Aeson             (decode)
import           Data.Maybe             (fromJust)
import           Network.HTTP.Client    (responseBody)
import           Network.HTTP.Types     (StdMethod (..))

import           Docker.Internal
import           Docker.Types

getDockerVersion :: forall m. MonadThrow m => DockerT m DockerVersion
getDockerVersion = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET VersionEndpoint opts
    response <- lift $ ((httpHandler request) :: m Response)
    let body = responseBody response
    let res = decode body
    return $ fromJust res

listContainers :: forall m. MonadThrow m => ListOpts -> DockerT m [Container]
listContainers lopts = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (ListContainersEndpoint lopts) opts
    response <- lift $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res

listImages :: forall m. MonadThrow m => ListOpts -> DockerT m [Image]
listImages lopts = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (ListImagesEndpoint lopts) opts
    response <- lift $ ((httpHandler request) :: m Response)
    let body = responseBody response
    let res = decode body
    return $ fromJust res

createContainer :: forall m. MonadThrow m => CreateOpts -> DockerT m ContainerID
createContainer  = undefined

startContainer :: forall m. MonadThrow m => StartOpts -> ContainerID -> DockerT m ContainerID
startContainer sopts cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (StartContainerEndpoint sopts cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

stopContainer :: forall m. MonadThrow m => Timeout -> ContainerID ->DockerT m ContainerID
stopContainer t cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (StopContainerEndpoint t cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

killContainer :: forall m. MonadThrow m => Signal -> ContainerID -> DockerT m ContainerID
killContainer s cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (KillContainerEndpoint s cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

restartContainer :: forall m. MonadThrow m => Timeout -> ContainerID -> DockerT m ContainerID
restartContainer t cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (RestartContainerEndpoint t cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

pauseContainer :: forall m. MonadThrow m => ContainerID -> DockerT m ContainerID
pauseContainer cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (PauseContainerEndpoint cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

unpauseContainer :: forall m. MonadThrow m => ContainerID -> DockerT m ContainerID
unpauseContainer cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (UnpauseContainerEndpoint cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

deleteContainer :: forall m. MonadThrow m => DeleteOpts -> ContainerID -> DockerT m ContainerID
deleteContainer dopts cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest DELETE (DeleteContainerEndpoint dopts cid) opts
    _ <- lift $ ((httpHandler request) :: m Response)
    return cid

getContainerLogs ::  forall m. MonadThrow m => LogOpts -> ContainerID -> DockerT m ()
getContainerLogs logopts cid = undefined


