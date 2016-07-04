{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docker.Http where

import           Control.Applicative
import           Control.Exception            (catch, try)
import           Control.Monad.Except
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy         as BL
import           Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Typeable                (Typeable)
import           Network.HTTP.Client          (defaultManagerSettings, httpLbs,
                                               managerRawConnection, method,
                                               newManager, parseUrl)
import qualified Network.HTTP.Client          as HTTP
import           Network.HTTP.Client.Internal (makeConnection)
import           Network.HTTP.Types           (StdMethod)
import qualified Network.Socket               as S
import qualified Network.Socket.ByteString    as SBS
import           System.IO.Error              (catchIOError, ioError)

import           Docker.Internal              (getEndpoint)
import           Docker.Types                 (DockerClientOpts, Endpoint,
                                               baseUrl)

type Request = HTTP.Request
type Response = HTTP.Response BL.ByteString
type HttpVerb = StdMethod
type HttpHandler m = Request -> m (Either DockerError Response)

data DockerError = DockerConnectionError
                 | GenericDockerError Text deriving (Eq, Show, Typeable)

newtype DockerT m a = DockerT {
        unDockerT :: Monad m => ReaderT (DockerClientOpts, HttpHandler m) m a
    } deriving (Functor) -- Applicative, Monad, MonadReader, MonadError, MonadTrans


-- instance Applicative m => Applicative (DockerT m) where
--     pure a = DockerT $ pure a
--     (<*>) (DockerT f) (DockerT v) =  DockerT $ f <*> v


-- type DockerT m a = ReaderT (DockerClientOpts, HttpHandler m) (ExceptT DockerError m) a

-- runDockerT :: forall m a. Monad m => (DockerClientOpts, HttpHandler m) -> DockerT m a -> m (Either DockerError a)
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
              request = case  initialR of
                            Just ir -> return $ ir { method = (encodeUtf8 . T.pack $ show verb) } -- , requestBody = RequestBodyLBS $ encode requestObject  }
                            Nothing -> Nothing
              -- TODO: manager = newManager defaultManagerSettings -- We likely need
              -- this for TLS.

-- mapHttpToDocker :: HTTP.HttpException -> DockerError
-- mapHttpToDocker e = case e of
--     HTTP.FailedConnectionException{}  -> DockerConnectionError
--     HTTP.FailedConnectionException2{} -> DockerConnectionError
--     otherwise                         -> GenericDockerError "dinamo"

defaultHttpHandler :: HttpHandler IO
defaultHttpHandler request = do
    manager <- newManager defaultManagerSettings
    -- either mapHttpToDocker id <$> try httpLbs request manager
    try (httpLbs request manager) >>= \res -> case res of
        Right res                              -> return $ Right res
        Left HTTP.FailedConnectionException{}  -> return $ Left DockerConnectionError
        Left HTTP.FailedConnectionException2{} -> return $ Left DockerConnectionError
        Left e                                 -> return $ Left $ GenericDockerError (T.pack $ show e)

-- | Connect to a unix domain socket (the default docker socket is
--   at \/var\/run\/docker.sock)
--
--   Docker seems to ignore the hostname in requests sent over unix domain
--   sockets (and the port obviously doesn't matter either)
unixHttpHandler :: FilePath -- ^ The socket to connect to
                -> HttpHandler IO
unixHttpHandler fp request= do
  let mSettings = defaultManagerSettings
                    { managerRawConnection = return $ openUnixSocket fp}
  manager <- newManager mSettings
  httpLbs request manager
  where
    openUnixSocket filePath _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix filePath)
      makeConnection (SBS.recv s 8096)
                     (SBS.sendAll s)
                     (S.sClose s)


