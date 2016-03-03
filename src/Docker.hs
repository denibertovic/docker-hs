module Docker where

import           Docker.Types

getDockerVersion :: Monad m => DockerT m (DockerVersion)
getDockerVersion = undefined

listContainers :: Monad m => ListOpts -> DockerT m ([Container])
listContainers = undefined

listImages :: Monad m => ListOpts -> DockerT m ([Image])
listImages = undefined
