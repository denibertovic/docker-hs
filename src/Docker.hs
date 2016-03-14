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

getDockerVersion :: DockerT IO (DockerVersion)
getDockerVersion = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET VersionEndpoint opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res

listContainers :: ListOpts -> DockerT IO ([Container])
listContainers lopts = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (ListContainersEndpoint lopts) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


listImages :: ListOpts -> DockerT IO ([Image])
listImages lopts = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (ListImagesEndpoint lopts) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


createContainer :: CreateOpts -> DockerT IO (ContainerID)
createContainer  = undefined

startContainer :: StartOpts -> ContainerID -> DockerT IO (ContainerID)
startContainer sopts cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (StartContainerEndpoint sopts cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


stopContainer :: Timeout -> ContainerID ->DockerT IO ()
stopContainer t cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (StopContainerEndpoint t cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


killContainer :: Signal -> ContainerID -> DockerT IO ()
killContainer s cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (KillContainerEndpoint s cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


restartContainer :: Timeout -> ContainerID -> DockerT IO ()
restartContainer t cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (RestartContainerEndpoint t cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


pauseContainer :: ContainerID -> DockerT IO ()
pauseContainer cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (PauseContainerEndpoint cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


unpauseContainer :: ContainerID -> DockerT IO ()
unpauseContainer cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (UnpauseContainerEndpoint cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res


deleteContainer :: DeleteOpts -> ContainerID -> DockerT IO ()
deleteContainer dopts cid = do
    (opts, httpHandler) <- ask
    let request = fromJust $ mkHttpRequest GET (DeleteContainerEndpoint dopts cid) opts
    response <- liftIO $ httpHandler request
    let body = responseBody response
    let res = decode body
    return $ fromJust res

getContainerLogs :: LogOpts -> ContainerID -> DockerT IO ()
getContainerLogs = undefined

