module Docker where

import           Control.Applicative    ((<$>), (<*>))
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

getDockerVersion :: DockerT IO DockerVersion
getDockerVersion = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET VersionEndpoint opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res

listContainers :: ListOpts -> DockerT IO [Container]
listContainers lopts = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (ListContainersEndpoint lopts) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res

listImages :: ListOpts -> DockerT IO [Image]
listImages lopts = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (ListImagesEndpoint lopts) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res

createContainer :: CreateOpts -> DockerT IO ContainerID
createContainer  = undefined

startContainer :: StartOpts -> ContainerID -> DockerT IO ContainerID
startContainer sopts cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (StartContainerEndpoint sopts cid) opts
    _ <- liftIO $ httpHandler request
    return cid

stopContainer :: Timeout -> ContainerID ->DockerT IO ContainerID
stopContainer t cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (StopContainerEndpoint t cid) opts
    _ <- liftIO $ httpHandler request
    return cid

killContainer :: Signal -> ContainerID -> DockerT IO ContainerID
killContainer s cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (KillContainerEndpoint s cid) opts
    _ <- liftIO $ httpHandler request
    return cid

restartContainer :: Timeout -> ContainerID -> DockerT IO ContainerID
restartContainer t cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (RestartContainerEndpoint t cid) opts
    _ <- liftIO $ httpHandler request
    return cid

pauseContainer :: ContainerID -> DockerT IO ContainerID
pauseContainer cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest POST (PauseContainerEndpoint cid) opts
    _ <- liftIO $ httpHandler request
    return cid

unpauseContainer :: ContainerID -> DockerT IO ContainerID
unpauseContainer cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (UnpauseContainerEndpoint cid) opts
    _ <- liftIO $ httpHandler request
    return cid

deleteContainer :: DeleteOpts -> ContainerID -> DockerT IO ContainerID
deleteContainer dopts cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest DELETE (DeleteContainerEndpoint dopts cid) opts
    _ <- liftIO $ httpHandler request
    return cid

getContainerLogs :: LogOpts -> ContainerID -> DockerT IO ()
getContainerLogs = undefined

