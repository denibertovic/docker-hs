{-# LANGUAGE OverloadedStrings #-}

module Network.Docker where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Lens
import           Data.Aeson                  (FromJSON, ToJSON, decode,
                                              eitherDecode, toJSON)
import           Data.Aeson.Lens             (key, _String)
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy        as L
import           Data.Char
import qualified Data.Text                   as T
import           Network.Docker.Options
import           Network.Docker.Types
import           Network.HTTP.Client.OpenSSL
import           Network.Wreq
import           OpenSSL                     (withOpenSSL)
import           OpenSSL.Session             (SSLContext, context)
import qualified OpenSSL.Session             as SSL
import           Pipes
import qualified Pipes.ByteString            as PB
import qualified Pipes.HTTP                  as PH
import           Text.Printf                 (printf)


defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts
                { apiVersion = "v1.12"
                , baseUrl = "http://127.0.0.1:3128/"
                , ssl = NoSSL
                }

constructUrl :: URL -> ApiVersion -> Endpoint -> URL
constructUrl url apiVer endpoint = printf "%s%s%s" url apiVer endpoint

constructRelativeUrl url = url :: String

decodeResponse r = decode . (^. responseBody) <$> r

getOutOfResponse k = (^? responseBody . key k . _String)

getResponseStatusCode = (^. responseStatus)

fullUrl :: DockerClientOpts -> Endpoint -> URL
fullUrl clientOpts endpoint = constructUrl (baseUrl clientOpts) (apiVersion clientOpts) endpoint

setupSSLCtx :: SSLOptions -> IO SSLContext
setupSSLCtx (SSLOptions key cert) = do
  ctx <- SSL.context
  SSL.contextSetPrivateKeyFile  ctx key
  SSL.contextSetCertificateFile ctx cert
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
  SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
  return ctx


mkOpts c = defaults & manager .~ Left (opensslManagerSettings c)

getSSL :: SSLOptions -> String -> IO (Response L.ByteString)
getSSL sopts url = withOpenSSL $ getWith (mkOpts $ setupSSLCtx sopts) url

postSSL :: ToJSON a =>
  SSLOptions ->
  String ->
  a ->
  IO (Response L.ByteString)
postSSL sopts url = withOpenSSL . postWith (mkOpts $ setupSSLCtx sopts) url . toJSON

_dockerGetQuery :: Endpoint -> DockerClientOpts -> IO(Response L.ByteString)
_dockerGetQuery endpoint clientOpts@DockerClientOpts{ssl = NoSSL} =
  get (fullUrl clientOpts endpoint)
_dockerGetQuery endpoint clientOpts@DockerClientOpts{ssl = SSL sslOpts} =
  getSSL sslOpts (fullUrl clientOpts endpoint)

_dockerPostQuery :: ToJSON a => Endpoint -> DockerClientOpts -> a -> IO (Response L.ByteString)
_dockerPostQuery endpoint clientOpts@DockerClientOpts{ssl = NoSSL} postObject =
  post (fullUrl clientOpts endpoint) (toJSON postObject)
_dockerPostQuery endpoint clientOpts@DockerClientOpts{ssl = SSL sslOpts} postObject =
  postSSL sslOpts (fullUrl clientOpts endpoint) postObject

emptyPost = "" :: String

_dockerEmptyPostQuery endpoint clientOpts =
  post (fullUrl clientOpts endpoint) (toJSON emptyPost)

_dockerEmptyDeleteQuery endpoint clientOpts = delete (fullUrl clientOpts endpoint)

getDockerVersion :: DockerClientOpts -> IO (Maybe DockerVersion)
getDockerVersion = decodeResponse . _dockerGetQuery "/version"

getDockerContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
getDockerContainers = decodeResponse . _dockerGetQuery "/containers/json"

getAllDockerContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
getAllDockerContainers = decodeResponse . _dockerGetQuery "/containers/json?all=true"

getDockerImages :: DockerClientOpts -> IO (Maybe [DockerImage])
getDockerImages = decodeResponse . _dockerGetQuery "/images/json"

getAllDockerImages :: DockerClientOpts -> IO (Maybe [DockerImage])
getAllDockerImages = decodeResponse . _dockerGetQuery "/images/json?all=true"

createContainer :: DockerClientOpts -> CreateContainerOpts -> IO(Maybe T.Text)
createContainer clientOpts createOpts = getOutOfResponse "Id" <$> _dockerPostQuery "/containers/create" clientOpts createOpts

startContainer :: DockerClientOpts -> String -> StartContainerOpts -> IO(Status)
startContainer clientOpts containerId startOpts = (^. responseStatus) <$> _dockerPostQuery (printf "/containers/%s/start" containerId) clientOpts startOpts

stopContainer :: DockerClientOpts -> String -> IO Status
stopContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/stop" containerId) clientOpts

killContainer :: DockerClientOpts -> String -> IO Status
killContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/kill" containerId) clientOpts

restartContainer :: DockerClientOpts -> String -> IO Status
restartContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/restart" containerId) clientOpts

pauseContainer :: DockerClientOpts -> String -> IO Status
pauseContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/pause" containerId) clientOpts

unpauseContainer :: DockerClientOpts -> String -> IO Status
unpauseContainer  clientOpts containerId = (^. responseStatus) <$> _dockerEmptyPostQuery (printf "/containers/%s/unpause" containerId) clientOpts

deleteContainer :: DockerClientOpts -> String -> IO Status
deleteContainer = deleteContainerWithOpts defaultDeleteOpts

deleteContainerWithOpts :: DeleteOpts -> DockerClientOpts -> String -> IO Status
deleteContainerWithOpts (DeleteOpts removeVolumes force) clientOpts contId = (^. responseStatus) <$> _dockerEmptyDeleteQuery req clientOpts
  where req = printf "/containers/%s?v=%s;force=%s" contId (show removeVolumes) (show force)

getContainerLogsStream :: DockerClientOpts -> String -> IO ()
getContainerLogsStream  clientOpts contId = do
                req <- PH.parseUrl (fullUrl clientOpts url)
                let req' =  req {PH.method = "GET"}
                PH.withManager PH.defaultManagerSettings $ \m  -> PH.withHTTP req' m  $ \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
        where url = printf "/containers/%s/logs?stdout=1&stderr=1&follow=1" contId

getContainerLogs :: DockerClientOpts -> String -> IO L.ByteString
getContainerLogs  clientOpts contId = (^. responseBody) <$> _dockerGetQuery url clientOpts
        where url = printf "/containers/%s/logs?stdout=1&stderr=1" contId

