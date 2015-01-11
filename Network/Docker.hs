{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Docker where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Lens
import           Control.Monad.Free
import           Data.Aeson                  (FromJSON, ToJSON, Value, decode,
                                              eitherDecode, toJSON)
import           Data.Aeson.Lens             (key, _String)
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy        as L
import           Data.Char
import qualified Data.Text                   as T
import qualified Data.Text                   as T
import           Network.Docker.Options
import           Network.Wreq
-- import           Pipes
-- import qualified Pipes.ByteString        as PB
-- import qualified Pipes.HTTP              as PH


import           Network.Docker.Internal
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

emptyPost = "" :: String

defaultClientOpts :: DockerClientOpts
defaultClientOpts = DockerClientOpts
                { apiVersion = "v1.12"
                , baseUrl = "http://127.0.0.1:3128/"
                , ssl = NoSSL
                }
run :: HttpRequestM r -> IO (Response L.ByteString)
run (Free (Get url)) = get url
run (Free (GetSSL sslOpts url)) = getSSL sslOpts url
run (Free (Post url body)) = post url body
run (Free (PostSSL sslOpts url body)) = postSSL sslOpts url body

constructUrl :: URL -> ApiVersion -> String -> URL
constructUrl url apiVersion endpoint = printf "%s%s%s" url apiVersion endpoint

-- decodeResponse :: Response L.ByteString -> a
decodeResponse r = decode (r ^. responseBody)

getElemFromResponse k r = (^? responseBody . key k . _String) r

getResponseStatusCode r = (^. responseStatus) r


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

_dockerGetQuery :: DockerClientOpts -> Endpoint -> HttpRequestM Endpoint
_dockerGetQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint = Free (Get (fullUrl clientOpts endpoint))
_dockerGetQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint = Free (GetSSL sslOpts (fullUrl clientOpts endpoint))

_dockerPostQuery :: ToJSON a => DockerClientOpts -> Endpoint -> a -> HttpRequestM Endpoint
_dockerPostQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint postObject = Free (Post (fullUrl clientOpts endpoint) (toJSON postObject))
_dockerPostQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint postObject = Free (PostSSL sslOpts (fullUrl clientOpts endpoint) (toJSON postObject))

_dockerEmptyPostQuery :: DockerClientOpts -> Endpoint -> HttpRequestM Endpoint
_dockerEmptyPostQuery clientOpts@DockerClientOpts{ssl = NoSSL} endpoint = Free (Post (fullUrl clientOpts endpoint) (toJSON emptyPost))
_dockerEmptyPostQuery clientOpts@DockerClientOpts{ssl = SSL sslOpts} endpoint = Free (PostSSL sslOpts (fullUrl clientOpts endpoint) (toJSON emptyPost))


_dockerEmptyDeleteQuery endpoint clientOpts = delete (fullUrl clientOpts endpoint)

getDockerVersion :: DockerClientOpts -> IO (Maybe DockerVersion)
getDockerVersion clientOpts = decodeResponse <$> run (getDockerVersionM clientOpts SVersionEndpoint)

listContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
listContainers clientOpts = decodeResponse <$> run (listContainersM clientOpts SListContainersEndpoint)

listImages :: DockerClientOpts -> IO (Maybe [DockerImage])
listImages clientOpts = decodeResponse <$> run (listImagesM clientOpts SListImagesEndpoint)

createContainer :: DockerClientOpts -> CreateContainerOpts -> IO(Maybe T.Text)
createContainer clientOpts createOpts = getElemFromResponse "Id" <$> run (createContainerM clientOpts (SCreateContainerEndpoint) createOpts)

startContainer :: DockerClientOpts -> String -> StartContainerOpts -> IO(Status)
startContainer clientOpts cid startOpts = (^. responseStatus) <$> run (startContainerM clientOpts (SStartContainerEndpoint cid) startOpts)

stopContainer :: DockerClientOpts -> String -> IO (Status)
stopContainer clientOpts cid = (^. responseStatus) <$> run (stopContainerM clientOpts (SStopContainerEndpoint cid))

killContainer :: DockerClientOpts -> String -> IO (Status)
killContainer clientOpts cid = (^. responseStatus) <$>  run (killContainerM clientOpts (SKillContainerEndpoint cid))

restartContainer :: DockerClientOpts -> String -> IO (Status)
restartContainer clientOpts cid = (^. responseStatus) <$> run (restartContainerM clientOpts (SRestartContainerEndpoint cid))

pauseContainer :: DockerClientOpts -> String -> IO (Status)
pauseContainer clientOpts cid = (^. responseStatus) <$> run (pauseContainerM clientOpts (SPauseContainerEndpoint cid))

unpauseContainer :: DockerClientOpts -> String -> IO (Status)
unpauseContainer clientOpts cid = (^. responseStatus) <$> run (unpauseContainerM clientOpts (SUnpauseContainerEndpoint cid))


-- getContainerLogsStream :: DockerClientOpts -> String -> IO ()
-- getContainerLogsStream  clientOpts containerId = do
--                 req <- PH.parseUrl (fullUrl clientOpts url)
--                 let req' =  req {PH.method = "GET"}
--                 PH.withManager PH.defaultManagerSettings $ \m  -> PH.withHTTP req' m  $ \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
--         where url = (printf "/containers/%s/logs?stdout=1&stderr=1&follow=1" containerId)

-- getContainerLogs :: DockerClientOpts -> String -> IO (L.ByteString)
-- getContainerLogs  clientOpts containerId = (^. responseBody) <$> _dockerGetQuery url clientOpts
--         where url = (printf "/containers/%s/logs?stdout=1&stderr=1" containerId)

