module Docker where

import           Docker.Types

getDockerVersion :: Monad m => DockerT m (DockerVersion)
getDockerVersion = undefined

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

