module Docker.Internal where


import           Blaze.ByteString.Builder (toByteString)
import           Data.ByteString          as BS
import           Data.ByteString          (ByteString)
import           Data.Text                as T
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Network.HTTP.Types       (Query, encodePath,
                                           encodePathSegments)


import           Docker.Types

mkHttpRequest :: DockerClientOpts -> Request
mkHttpRequest = undefined

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

