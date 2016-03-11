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

listContainers :: ListOpts -> DockerT IO ([Container])
listContainers = undefined

listImages :: ListOpts -> DockerT IO ([Image])
listImages = undefined

createContainer :: CreateOpts -> DockerT IO (ContainerID)
createContainer  = undefined

startContainer :: StartOpts -> ContainerID -> DockerT IO (ContainerID)
startContainer = undefined

stopContainer :: Timeout -> ContainerID ->DockerT IO ()
stopContainer = undefined

killContainer :: Signal -> ContainerID -> DockerT IO ()
killContainer = undefined

restartContainer :: Timeout -> ContainerID -> DockerT IO ()
restartContainer = undefined

pauseContainer :: ContainerID -> DockerT IO ()
pauseContainer = undefined

unpauseContainer :: ContainerID -> DockerT IO ()
unpauseContainer = undefined

deleteContainer :: DeleteOpts -> ContainerID -> DockerT IO ()
deleteContainer = undefined

getContainerLogs :: LogOpts -> ContainerID -> DockerT IO ()
getContainerLogs = undefined

