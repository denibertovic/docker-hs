{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Docker.Internal where

import           Control.Lens
import           Control.Monad.Free
import           Data.Aeson                  (FromJSON, ToJSON, Value, decode,
                                              eitherDecode, toJSON)
import           Data.Aeson.Lens             (key, _String)
import qualified Data.ByteString.Lazy        as L
import           Network.HTTP.Client.OpenSSL
import           Network.Wreq
import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext, context)
import qualified OpenSSL.Session             as SSL
import           Text.Printf                 (printf)

import           Network.Docker.Types

emptyPost = "" :: String

runDocker :: HttpRequestM r -> IO (Response L.ByteString)
runDocker (Free (Get url)) = get url
runDocker (Free (GetSSL sslOpts url)) = getSSL sslOpts url
runDocker (Free (Post url body)) = post url body
runDocker (Free (PostSSL sslOpts url body)) = postSSL sslOpts url body
runDocker (Free (Delete url)) = delete url
runDocker (Free (DeleteSSL sslOpts url)) = deleteSSL sslOpts url

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
getEndpoint (SContainerLogsEndpoint cid (LogOpts stdout stderr follow )) =
        printf "/containers/%s/logs?stdout=%s&stderr=%s&follow=%s" cid (show stdout) (show stderr) (show follow)
getEndpoint (SDeleteContainerEndpoint cid (DeleteOpts removeVolumes force)) =
        printf "/containers/%s?v=%s;force=%s" cid (show removeVolumes) (show force)

setupSSLCtx :: SSLOptions -> IO SSLContext
setupSSLCtx (SSLOptions key cert) = do
    ctx <- SSL.context
    SSL.contextSetPrivateKeyFile  ctx key
    SSL.contextSetCertificateFile ctx cert
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
    return ctx

mkOpts c = defaults & manager .~ Left (opensslManagerSettings c)

getSSL
    :: SSLOptions
    -> String
    -> IO (Response L.ByteString)
getSSL sopts url = withOpenSSL $ getWith (mkOpts $ setupSSLCtx sopts) url

postSSL
    :: ToJSON a
    => SSLOptions
    -> String
    -> a
    -> IO (Response L.ByteString)
postSSL sopts url = withOpenSSL . postWith (mkOpts $ setupSSLCtx sopts) url . toJSON

deleteSSL
    :: SSLOptions
    -> String
    -> IO (Response L.ByteString)
deleteSSL sopts url = withOpenSSL $ deleteWith (mkOpts $ setupSSLCtx sopts) url

fullUrl :: DockerClientOpts -> SEndpoint a -> URL
fullUrl clientOpts endpoint =
    constructUrl (baseUrl clientOpts) (apiVersion clientOpts) (getEndpoint endpoint)

_dockerGetQuery :: DockerClientOpts -> SEndpoint a -> HttpRequestM (SEndpoint a)
_dockerGetQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint =
    Free (Get (fullUrl clientOpts endpoint))
_dockerGetQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint =
    Free (GetSSL sslOpts (fullUrl clientOpts endpoint))

_dockerPostQuery :: ToJSON b => DockerClientOpts -> SEndpoint a -> b -> HttpRequestM (SEndpoint a)
_dockerPostQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint postObject =
    Free (Post (fullUrl clientOpts endpoint) (toJSON postObject))
_dockerPostQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint postObject =
    Free (PostSSL sslOpts (fullUrl clientOpts endpoint) (toJSON postObject))

_dockerEmptyPostQuery :: DockerClientOpts -> SEndpoint a -> HttpRequestM (SEndpoint a)
_dockerEmptyPostQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint =
    Free (Post (fullUrl clientOpts endpoint) (toJSON emptyPost))
_dockerEmptyPostQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint =
    Free (PostSSL sslOpts (fullUrl clientOpts endpoint) (toJSON emptyPost))

_dockerDeleteQuery :: DockerClientOpts -> SEndpoint a -> HttpRequestM (SEndpoint a)
_dockerDeleteQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint =
    Free (Delete (fullUrl clientOpts endpoint))
_dockerDeleteQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint =
    Free (DeleteSSL sslOpts (fullUrl clientOpts endpoint))

getDockerVersionM
    :: DockerClientOpts
    -> SEndpoint VersionEndpoint
    -> HttpRequestM (SEndpoint VersionEndpoint)
getDockerVersionM clientOpts e = _dockerGetQuery clientOpts e

stopContainerM
    :: DockerClientOpts
    -> SEndpoint StopContainerEndpoint
    -> HttpRequestM (SEndpoint StopContainerEndpoint)
stopContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

listContainersM
    :: DockerClientOpts
    -> SEndpoint ListContainersEndpoint
    -> HttpRequestM (SEndpoint ListContainersEndpoint)
listContainersM clientOpts e = _dockerGetQuery clientOpts e

listImagesM
    :: DockerClientOpts
    -> SEndpoint ListImagesEndpoint
    -> HttpRequestM (SEndpoint ListImagesEndpoint)
listImagesM clientOpts e = _dockerGetQuery clientOpts e

killContainerM
    :: DockerClientOpts
    -> SEndpoint KillContainerEndpoint
    -> HttpRequestM (SEndpoint KillContainerEndpoint)
killContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

restartContainerM
    :: DockerClientOpts
    -> SEndpoint RestartContainerEndpoint
    -> HttpRequestM (SEndpoint RestartContainerEndpoint)
restartContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

pauseContainerM
    :: DockerClientOpts
    -> SEndpoint PauseContainerEndpoint
    -> HttpRequestM (SEndpoint PauseContainerEndpoint)
pauseContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

unpauseContainerM
    :: DockerClientOpts
    -> SEndpoint UnpauseContainerEndpoint
    -> HttpRequestM (SEndpoint UnpauseContainerEndpoint)
unpauseContainerM clientOpts e = _dockerEmptyPostQuery clientOpts e

createContainerM
    :: DockerClientOpts
    -> SEndpoint CreateContainerEndpoint
    -> CreateContainerOpts
    -> HttpRequestM (SEndpoint CreateContainerEndpoint)
createContainerM clientOpts e c = _dockerPostQuery clientOpts e c

startContainerM
    :: DockerClientOpts
    -> SEndpoint StartContainerEndpoint
    -> StartContainerOpts
    -> HttpRequestM (SEndpoint StartContainerEndpoint)
startContainerM clientOpts e c = _dockerPostQuery clientOpts e c

deleteContainerM
    :: DockerClientOpts
    -> SEndpoint DeleteContainerEndpoint
    -> HttpRequestM (SEndpoint DeleteContainerEndpoint)
deleteContainerM clientOpts e = _dockerDeleteQuery clientOpts e

getContainerLogsM
    :: DockerClientOpts
    -> SEndpoint ContainerLogsEndpoint
    -> HttpRequestM (SEndpoint ContainerLogsEndpoint)
getContainerLogsM clientOpts e = _dockerGetQuery clientOpts e

