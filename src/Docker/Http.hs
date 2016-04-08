module Docker.Http where

import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy as BL
import           Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import           Network.HTTP.Client  (defaultManagerSettings, httpLbs,
                                       managerRawConnection, method, newManager,
                                       parseUrl)
import qualified Network.HTTP.Client  as HTTP
import           Network.HTTP.Types   (StdMethod)

import           Docker.Internal      (getEndpoint)
import           Docker.Types         (DockerClientOpts, Endpoint, baseUrl)

type Request = HTTP.Request
type Response = HTTP.Response BL.ByteString
type HttpVerb = StdMethod
type HttpHandler m = Request -> m Response

type DockerT m a = ReaderT (DockerClientOpts, HttpHandler m) (ExceptT String m) a

runDockerT :: Monad m => (DockerClientOpts, HttpHandler m) -> DockerT m a -> m (Either String a)
runDockerT (opts, h) r = runExceptT $ runReaderT r (opts, h)


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

defaultHttpHandler :: HttpHandler IO
defaultHttpHandler request = do
        manager <- newManager defaultManagerSettings
        response <- httpLbs request manager
        return response

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


