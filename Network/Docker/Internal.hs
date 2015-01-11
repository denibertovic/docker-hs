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

getEndpoint :: Endpoint -> String
getEndpoint VersionEndpoint = "/version"
getEndpoint ListContainersEndpoint = "/containers/json"
getEndpoint ListImagesEndpoint = "/images/json"
getEndpoint CreateContainerEndpoint = "/containers/create"
getEndpoint (StartContainerEndpoint cid) = printf "/containers/%s/start" cid
getEndpoint (StopContainerEndpoint cid) = printf "/containers/%s/stop" cid
getEndpoint (KillContainerEndpoint cid) = printf "/containers/%s/kill" cid
getEndpoint (RestartContainerEndpoint cid) = printf "/containers/%s/restart" cid
getEndpoint (PauseContainerEndpoint cid) = printf "/containers/%s/pause" cid
getEndpoint (UnpauseContainerEndpoint cid) = printf "/containers/%s/unpause" cid
getEndpoint (ContainerLogsEndpoint cid) = printf "/containers/%s/logs" cid

fullUrl :: DockerClientOpts -> Endpoint -> URL
fullUrl clientOpts endpoint = constructUrl (baseUrl clientOpts) (apiVersion clientOpts) (getEndpoint endpoint)

_dockerGetQuery :: DockerClientOpts -> Endpoint -> HttpRequestM Endpoint
_dockerGetQuery clientOpts endpoint = Free (Get (fullUrl clientOpts endpoint))

_dockerPostQuery :: ToJSON a => DockerClientOpts -> Endpoint -> a -> HttpRequestM Endpoint
_dockerPostQuery clientOpts endpoint postObject = Free (Post (fullUrl clientOpts endpoint) (toJSON postObject))

_dockerEmptyPostQuery :: DockerClientOpts -> Endpoint -> HttpRequestM Endpoint
_dockerEmptyPostQuery clientOpts endpoint = Free (Post (fullUrl clientOpts endpoint) (toJSON emptyPost))

getDockerVersionM :: DockerClientOpts -> HttpRequestM Endpoint
getDockerVersionM clientOpts = _dockerGetQuery clientOpts VersionEndpoint

stopContainerM :: DockerClientOpts -> Endpoint -> HttpRequestM Endpoint
stopContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e


