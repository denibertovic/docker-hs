{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Docker.Internal where

import           Control.Lens
import           Control.Monad.Free
import           Data.Aeson           (FromJSON, ToJSON, Value, decode,
                                       eitherDecode, toJSON)
import           Data.Aeson.Lens      (key, _String)
import qualified Data.ByteString.Lazy as L
import           Network.Docker.Types
import           Network.Wreq
import           Text.Printf          (printf)

emptyPost = "" :: String


run :: HttpRequestM r -> IO (Response L.ByteString)
run (Free (Get url)) = get url
run (Free (Post url body)) = post url body

constructUrl :: URL -> ApiVersion -> String -> URL
constructUrl url apiVersion endpoint = printf "%s%s%s" url apiVersion endpoint

-- decodeResponse :: Response L.ByteString -> a
decodeResponse r = decode (r ^. responseBody)

getElemFromResponse k r = (^? responseBody . key k . _String) r

getResponseStatusCode r = (^. responseStatus) r

getEndpoint :: SEndpoint a -> String
getEndpoint SVersionEndpoint = "/version"
getEndpoint SListContainersEndpoint = "/containers/json"
getEndpoint SListImagesEndpoint = "/images/json"
getEndpoint SCreateContainerEndpoint = "/containers/create"
getEndpoint (SStartContainerEndpoint cid) = printf "/containers/%s/start" cid
getEndpoint (SStopContainerEndpoint cid) = printf "/containers/%s/stop" cid
getEndpoint (SKillContainerEndpoint cid) = printf "/containers/%s/kill" cid
getEndpoint (SRestartContainerEndpoint cid) = printf "/containers/%s/restart" cid
getEndpoint (SPauseContainerEndpoint cid) = printf "/containers/%s/pause" cid
getEndpoint (SUnpauseContainerEndpoint cid) = printf "/containers/%s/unpause" cid
getEndpoint (SContainerLogsEndpoint cid) = printf "/containers/%s/logs" cid

fullUrl :: DockerClientOpts -> SEndpoint a -> URL
fullUrl clientOpts endpoint = constructUrl (baseUrl clientOpts) (apiVersion clientOpts) (getEndpoint endpoint)

_dockerGetQuery :: DockerClientOpts -> SEndpoint a -> HttpRequestM (SEndpoint a)
_dockerGetQuery clientOpts endpoint = Free (Get (fullUrl clientOpts endpoint))

_dockerPostQuery :: ToJSON b => DockerClientOpts -> SEndpoint a -> b -> HttpRequestM (SEndpoint a)
_dockerPostQuery clientOpts endpoint postObject = Free (Post (fullUrl clientOpts endpoint) (toJSON postObject))

_dockerEmptyPostQuery :: DockerClientOpts -> SEndpoint a -> HttpRequestM (SEndpoint a)
_dockerEmptyPostQuery clientOpts endpoint = Free (Post (fullUrl clientOpts endpoint) (toJSON emptyPost))

getDockerVersionM :: DockerClientOpts -> SEndpoint VersionEndpoint -> HttpRequestM (SEndpoint VersionEndpoint)
getDockerVersionM clientOpts e = _dockerGetQuery clientOpts e

stopContainerM :: DockerClientOpts -> SEndpoint StopContainerEndpoint -> HttpRequestM (SEndpoint StopContainerEndpoint)
stopContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

listContainersM :: DockerClientOpts -> SEndpoint ListContainersEndpoint -> HttpRequestM (SEndpoint ListContainersEndpoint)
listContainersM clientOpts e = _dockerGetQuery clientOpts e

listImagesM :: DockerClientOpts -> SEndpoint ListImagesEndpoint -> HttpRequestM (SEndpoint ListImagesEndpoint)
listImagesM clientOpts e = _dockerGetQuery clientOpts e

killContainerM :: DockerClientOpts -> SEndpoint KillContainerEndpoint -> HttpRequestM (SEndpoint KillContainerEndpoint)
killContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

restartContainerM :: DockerClientOpts -> SEndpoint RestartContainerEndpoint -> HttpRequestM (SEndpoint RestartContainerEndpoint)
restartContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

pauseContainerM :: DockerClientOpts -> SEndpoint PauseContainerEndpoint -> HttpRequestM (SEndpoint PauseContainerEndpoint)
pauseContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

unpauseContainerM :: DockerClientOpts -> SEndpoint UnpauseContainerEndpoint -> HttpRequestM (SEndpoint UnpauseContainerEndpoint)
unpauseContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

createContainerM :: DockerClientOpts -> SEndpoint CreateContainerEndpoint -> CreateContainerOpts -> HttpRequestM (SEndpoint CreateContainerEndpoint)
createContainerM clientOpts e c = _dockerPostQuery clientOpts e c

startContainerM :: DockerClientOpts -> SEndpoint StartContainerEndpoint -> StartContainerOpts -> HttpRequestM (SEndpoint StartContainerEndpoint)
startContainerM clientOpts e c = _dockerPostQuery clientOpts e c


