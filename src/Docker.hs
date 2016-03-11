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
    let res = decode body :: Maybe DockerVersion
    return $ fromJust res

listContainers :: Monad m => ListOpts -> DockerT m ([Container])
listContainers = undefined

listImages :: Monad m => ListOpts -> DockerT m ([Image])
listImages = undefined

createContainer :: Monad m => CreateOpts -> DockerT m (ContainerID)
createContainer  = undefined

startContainer :: Monad m => StartOpts -> DockerT m (ContainerID)
startContainer = undefined

stopContainer :: Monad m => Timeout -> DockerT m ()
stopContainer = undefined

killContainer :: Monad m => Signal -> DockerT m ()
killContainer = undefined

restartContainer :: Monad m => Timeout -> DockerT m ()
restartContainer = undefined

pauseContainer :: Monad m => DockerT m ()
pauseContainer = undefined

unpauseContainer :: Monad m => DockerT m ()
unpauseContainer = undefined

deleteContainer :: Monad m => DeleteOpts -> DockerT m ()
deleteContainer = undefined

getContainerLogs :: Monad m => LogOpts -> DockerT m ()
getContainerLogs = undefined

