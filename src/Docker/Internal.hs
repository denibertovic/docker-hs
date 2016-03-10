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
-- Since we're building the Requests this shouldn't happen, but would
-- benefit from testing that for instance all of our Endpoints are solid
-- and so on.
mkHttpRequest :: HttpVerb -> Endpoint -> DockerClientOpts -> Maybe Request
mkHttpRequest verb e opts = request
        where fullE = T.unpack (baseUrl opts) ++ T.unpack (getEndpoint e)
              initialR = parseUrl fullE
              request = case  initialR of
                            Just ir -> return $ ir { method = (encodeUtf8 . T.pack $ show verb) } -- , requestBody = RequestBodyLBS $ encode requestObject  }
                            Nothing -> Nothing
              -- TODO: manager = newManager defaultManagerSettings -- We likely need
              -- this for TLS.

defaultHandler :: Request -> IO Response
defaultHandler = undefined

encodeURL :: [Text] -> Text
encodeURL ps = decodeUtf8 $ toByteString $ encodePathSegments ps

encodeURLWithQuery :: [Text] -> Query -> Text
encodeURLWithQuery ps q = decodeUtf8 $ toByteString $ encodePath ps q

encodeQ = encodeUtf8 . T.pack . show

getEndpoint :: Endpoint -> Text
getEndpoint VersionEndpoint = encodeURL ["version"]
getEndpoint (ListContainersEndpoint lsOpts) = encodeURL ["containers", "json"]
getEndpoint (ListImagesEndpoint lsOpts) = encodeURL ["images", "json"]
getEndpoint CreateContainerEndpoint = encodeURL ["containers", "create"]
getEndpoint (StartContainerEndpoint cid) = encodeURL ["containers", cid, "start"]
getEndpoint (StopContainerEndpoint cid) = encodeURL ["containers", cid, "stop"]
getEndpoint (KillContainerEndpoint cid) = encodeURL ["containers", cid, "kill"]
getEndpoint (RestartContainerEndpoint cid) = encodeURL ["containers", cid, "restart"]
getEndpoint (PauseContainerEndpoint cid) = encodeURL ["containers", cid, "pause"]
getEndpoint (UnpauseContainerEndpoint cid) = encodeURL ["containers", cid, "unpause"]
getEndpoint (ContainerLogsEndpoint cid (LogOpts follow stdout stderr since timestamps tail)) =
            encodeURLWithQuery    ["containers", cid, "logs"] query
        where query = [("stdout", Just (encodeQ stdout)), ("stderr", Just (encodeQ stderr)), ("follow", Just (encodeQ follow))]
getEndpoint (DeleteContainerEndpoint cid (DeleteOpts removeVolumes force)) =
            encodeURLWithQuery ["containers", cid] query
        where query = [("v", Just (encodeQ removeVolumes)), ("force", Just (encodeQ force))]

