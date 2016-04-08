module Docker.Internal where


import           Blaze.ByteString.Builder     (toByteString)
import           Data.ByteString              (ByteString)
import           Data.Maybe
import           Data.Text                    as T
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Docker.Types
import           Network.HTTP.Client.Internal (makeConnection)
import           Network.HTTP.Types           (Query, encodePath,
                                               encodePathSegments)
import qualified Network.Socket               as S
import qualified Network.Socket.ByteString    as SBS

encodeURL :: [Text] -> Text
encodeURL ps = decodeUtf8 $ toByteString $ encodePathSegments ps

encodeURLWithQuery :: [Text] -> Query -> Text
encodeURLWithQuery ps q = decodeUtf8 $ toByteString $ encodePath ps q

encodeQ :: String -> ByteString
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
getEndpoint (ContainerLogsEndpoint (LogOpts follow stdout stderr since timestamps tail) cid) =
            encodeURLWithQuery    ["containers", cid, "logs"] query
        where query = [("stdout", Just (encodeQ $ show stdout)), ("stderr", Just (encodeQ $ show stderr)), ("follow", Just (encodeQ $ show follow))]
getEndpoint (DeleteContainerEndpoint (DeleteOpts removeVolumes force) cid) =
            encodeURLWithQuery ["containers", cid] query
        where query = [("v", Just (encodeQ $ show removeVolumes)), ("force", Just (encodeQ $ show force))]
