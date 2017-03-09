{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Client.Http where

-- import           Control.Monad.Base           (MonadBase(..), liftBaseDefault)
import           Control.Monad.Catch          (MonadMask(..))
import           Control.Monad.Reader         (ReaderT(..), runReaderT)
import qualified Data.ByteString.Char8        as BSC
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 (Sink)
import           Data.Default.Class           (def)
import           Data.Monoid                  ((<>))
import           Data.Text.Encoding           (encodeUtf8)
import           Data.X509                    (CertificateChain (..))
import           Data.X509.CertificateStore   (makeCertificateStore)
import           Data.X509.File               (readKeyFile, readSignedObject)
import           Network.HTTP.Client          (defaultManagerSettings,
                                               managerRawConnection, method,
                                               newManager, parseUrl,
                                               requestBody, requestHeaders)
import qualified Network.HTTP.Client          as HTTP
import           Network.HTTP.Client.Internal (makeConnection)
import           Network.HTTP.Types           (StdMethod, status101, status200,
                                               status201, status204)
import qualified Network.HTTP.Simple          as NHS
import           Network.TLS                  (ClientHooks (..),
                                               ClientParams (..), Shared (..),
                                               Supported (..),
                                               defaultParamsClient)
import           Network.TLS.Extra            (ciphersuite_strong)
import           System.X509                  (getSystemCertificateStore)

import           Control.Monad.Catch          (try)
import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Data.Text                    as T
import           Data.Typeable                (Typeable)
import qualified Network.HTTP.Types           as HTTP
import qualified Network.Socket               as S
import qualified Network.Socket.ByteString    as SBS


import           Docker.Client.Internal       (getEndpoint,
                                               getEndpointContentType,
                                               getEndpointRequestBody)
import           Docker.Client.Types          (DockerClientOpts, Endpoint (..),
                                               baseUrl)

type Request = HTTP.Request
type Response = HTTP.Response BL.ByteString
type HttpVerb = StdMethod
newtype HttpHandler m = HttpHandler (forall a . Request -> (HTTP.Response () -> Sink BSC.ByteString m (Either DockerError a)) -> m (Either DockerError a))

data DockerError = DockerConnectionError
                 | DockerInvalidRequest Endpoint
                 | DockerClientError Text
                 | DockerClientDecodeError Text -- ^ Could not parse the response from the Docker endpoint.
                 | DockerInvalidStatusCode HTTP.Status -- ^ Invalid exit code received from Docker endpoint.
                 | GenericDockerError Text deriving (Eq, Show, Typeable)

newtype DockerT m a = DockerT {
        unDockerT :: Monad m => ReaderT (DockerClientOpts, HttpHandler m) m a
    } deriving (Functor) -- Applicative, Monad, MonadReader, MonadError, MonadTrans

instance Applicative m => Applicative (DockerT m) where
    pure a = DockerT $ pure a
    (<*>) (DockerT f) (DockerT v) =  DockerT $ f <*> v

instance Monad m => Monad (DockerT m) where
    (DockerT m) >>= f = DockerT $ m >>= unDockerT . f
    return = pure

instance Monad m => MonadReader (DockerClientOpts, HttpHandler m) (DockerT m) where
    ask = DockerT ask
    local f (DockerT m) = DockerT $ local f m

instance MonadTrans DockerT where
    lift m = DockerT $ lift m

instance MonadIO m => MonadIO (DockerT m) where
    liftIO = lift . liftIO

-- instance MonadBase IO m => MonadBase IO (DockerT m) where
--     liftBase = liftBaseDefault

runDockerT :: Monad m => (DockerClientOpts, HttpHandler m) -> DockerT m a -> m a
runDockerT (opts, h) r = runReaderT (unDockerT r) (opts, h)

-- The reason we return Maybe Request is because the parseURL function
-- might find out parameters are invalid and will fail to build a Request
-- Since we are the ones building the Requests this shouldn't happen, but would
-- benefit from testing that on all of our Endpoints
mkHttpRequest :: HttpVerb -> Endpoint -> DockerClientOpts -> Maybe Request
mkHttpRequest verb e opts = request
        where fullE = T.unpack (baseUrl opts) ++ T.unpack (getEndpoint e)
              initialR = parseUrl fullE
              request' = case  initialR of
                            Just ir ->
                                return $ ir {method = (encodeUtf8 . T.pack $ show verb),
                                              requestHeaders = [("Content-Type", (getEndpointContentType e))]}
                            Nothing -> Nothing
              request = (\r -> maybe r (\body -> r {requestBody = body,  -- This will either be a HTTP.RequestBodyLBS  or HTTP.RequestBodySourceChunked for the build endpoint
                                                    requestHeaders = [("Content-Type", "application/json; charset=utf-8")]}) $ getEndpointRequestBody e) <$> request'
              -- Note: Do we need to set length header?

defaultHttpHandler :: (MonadIO m, MonadMask m) => m (HttpHandler m)
defaultHttpHandler = do
    manager <- liftIO $ newManager defaultManagerSettings
    return $ httpHandler manager

httpHandler :: (MonadIO m, MonadMask m) => HTTP.Manager -> HttpHandler m
httpHandler manager = HttpHandler $ \request' sink -> do -- runResourceT .. 
    let request = NHS.setRequestManager manager request'
    try (NHS.httpSink request sink) >>= \res -> case res of
        Right res                              -> return res
        Left HTTP.FailedConnectionException{}  -> return $ Left DockerConnectionError
        Left HTTP.FailedConnectionException2{} -> return $ Left DockerConnectionError
        Left e                                 -> return $ Left $ GenericDockerError (T.pack $ show e)

-- | Connect to a unix domain socket (the default docker socket is
--   at \/var\/run\/docker.sock)
--
--   Docker seems to ignore the hostname in requests sent over unix domain
--   sockets (and the port obviously doesn't matter either)
unixHttpHandler :: (MonadIO m, MonadMask m) => FilePath -- ^ The socket to connect to
                -> m (HttpHandler m)
unixHttpHandler fp = do
  let mSettings = defaultManagerSettings
                    { managerRawConnection = return $ openUnixSocket fp}
  manager <- liftIO $ newManager mSettings
  return $ httpHandler manager

  where
    openUnixSocket filePath _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix filePath)
      makeConnection (SBS.recv s 8096)
                     (SBS.sendAll s)
                     (S.sClose s)

-- TODO:
--  Move this to http-client-tls or network?
--  Add CA.
--  Maybe change this to: HostName -> PortNumber -> ClientParams -> IO (Either String TLSSettings)
clientParamsWithClientAuthentication :: S.HostName -> S.PortNumber -> FilePath -> FilePath -> IO (Either String ClientParams)
clientParamsWithClientAuthentication host port keyFile certificateFile = do
    keys <- readKeyFile keyFile
    cert <- readSignedObject certificateFile
    case keys of
        [key] ->
            -- TODO: load keys/path from file
            let params = (defaultParamsClient host $ BSC.pack $ show port) {
                    clientHooks = def
                        { onCertificateRequest = \_ -> return (Just (CertificateChain cert, key))}
                  , clientSupported = def
                        { supportedCiphers = ciphersuite_strong}
                  }
            in
            return $ Right params
        _ ->
            return $ Left $ "Could not read key file: " ++ keyFile

clientParamsSetCA :: ClientParams -> FilePath -> IO ClientParams
clientParamsSetCA params path = do
    userStore <- makeCertificateStore <$> readSignedObject path
    systemStore <- getSystemCertificateStore
    let store = userStore <> systemStore
    let oldShared = clientShared params
    return $ params { clientShared = oldShared
            { sharedCAStore = store }
        }


-- If the status is an error, returns a Just DockerError. Otherwise, returns Nothing.
statusCodeToError :: Endpoint -> HTTP.Status -> Maybe DockerError
statusCodeToError VersionEndpoint st =
    if st == status200 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (ListContainersEndpoint _) st =
    if st == status200 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (ListImagesEndpoint _) st =
    if st == status200 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (CreateContainerEndpoint _ _) st =
    if st == status201 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (StartContainerEndpoint _ _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (StopContainerEndpoint _ _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (WaitContainerEndpoint _) st =
    if st == status200 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (KillContainerEndpoint _ _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (RestartContainerEndpoint _ _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (PauseContainerEndpoint _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (UnpauseContainerEndpoint _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (ContainerLogsEndpoint _ _ _) st =
    if st == status200 || st == status101 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (DeleteContainerEndpoint _ _) st =
    if st == status204 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (InspectContainerEndpoint _) st =
    if st == status200 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st
statusCodeToError (BuildImageEndpoint _ _) st =
    if st == status200 then
        Nothing
    else
        Just $ DockerInvalidStatusCode st

