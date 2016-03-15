module Docker.Internal where


import           Blaze.ByteString.Builder (toByteString)
import           Data.ByteString          as BS
import           Data.ByteString          (ByteString)
import           Data.Maybe
import           Data.Text                as T
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Network.HTTP.Client      (defaultManagerSettings, httpLbs,
                                           method, newManager, parseUrl)
import           Network.HTTP.Types       (Query, encodePath,
                                           encodePathSegments)

import           Docker.Types

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

encodeURL :: [Text] -> Text
encodeURL ps = decodeUtf8 $ toByteString $ encodePathSegments ps

encodeURLWithQuery :: [Text] -> Query -> Text
encodeURLWithQuery ps q = decodeUtf8 $ toByteString $ encodePath ps q

encodeQ = encodeUtf8 . T.pack

getEndpoint :: Endpoint -> Text
getEndpoint VersionEndpoint = encodeURL ["version"]
getEndpoint (ListContainersEndpoint lsOpts) = encodeURL ["containers", "json"]
getEndpoint (ListImagesEndpoint lsOpts) = encodeURL ["images", "json"]
getEndpoint CreateContainerEndpoint = encodeURL ["containers", "create"]
getEndpoint (StartContainerEndpoint startOpts cid) = encodeURLWithQuery ["containers", cid, "start"] query
        where query = case (detachKeys startOpts) of
                WithCtrl c -> [("detachKeys", Just (encodeQ $ ctrl ++ [c]))]
                WithoutCtrl c -> [("detachKeys", Just (encodeQ [c]))]
                DefaultDetachKey -> []
              ctrl = ['c', 't', 'r', 'l', '-']
getEndpoint (StopContainerEndpoint t cid) = encodeURLWithQuery ["containers", cid, "stop"] query
        where query = case t of
                Timeout x -> [("t", Just (encodeQ $ show x))]
                DefaultTimeout -> []
getEndpoint (KillContainerEndpoint s cid) = encodeURLWithQuery ["containers", cid, "kill"] query
        where query = case s of
                SIG x -> [("signal", Just (encodeQ $ show x))]
                otherwise -> [("signal", Just (encodeQ $ show s))]
getEndpoint (RestartContainerEndpoint t cid) = encodeURLWithQuery ["containers", cid, "restart"] query
        where query = case t of
                Timeout x -> [("t", Just (encodeQ $ show x))]
                DefaultTimeout -> []
getEndpoint (PauseContainerEndpoint cid) = encodeURL ["containers", cid, "pause"]
getEndpoint (UnpauseContainerEndpoint cid) = encodeURL ["containers", cid, "unpause"]
getEndpoint (ContainerLogsEndpoint cid (LogOpts follow stdout stderr since timestamps tail)) =
            encodeURLWithQuery    ["containers", cid, "logs"] query
        where query = [("stdout", Just (encodeQ $ show stdout)), ("stderr", Just (encodeQ $ show stderr)), ("follow", Just (encodeQ $ show follow))]
getEndpoint (DeleteContainerEndpoint (DeleteOpts removeVolumes force) cid) =
            encodeURLWithQuery ["containers", cid] query
        where query = [("v", Just (encodeQ $ show removeVolumes)), ("force", Just (encodeQ $ show force))]

