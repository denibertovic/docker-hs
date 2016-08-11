module Docker.Client.Internal where


import           Blaze.ByteString.Builder (toByteString)
import qualified Data.Aeson               as JSON
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Network.HTTP.Types       (Query, encodePath,
                                           encodePathSegments)

import           Docker.Client.Types


encodeURL :: [T.Text] -> T.Text
encodeURL ps = decodeUtf8 $ toByteString $ encodePathSegments ps

encodeURLWithQuery :: [T.Text] -> Query -> T.Text
encodeURLWithQuery ps q = decodeUtf8 $ toByteString $ encodePath ps q

encodeQ :: String -> ByteString
encodeQ = encodeUtf8 . T.pack

getEndpoint :: Endpoint -> T.Text
getEndpoint VersionEndpoint = encodeURL ["version"]
getEndpoint (ListContainersEndpoint _) = encodeURL ["containers", "json"] -- Make use of lsOpts here
getEndpoint (ListImagesEndpoint _) = encodeURL ["images", "json"] -- Make use of lsOpts here
getEndpoint (CreateContainerEndpoint _) = encodeURL ["containers", "create"]
getEndpoint (StartContainerEndpoint startOpts cid) = encodeURLWithQuery ["containers", fromContainerID cid, "start"] query
        where query = case (detachKeys startOpts) of
                WithCtrl c -> [("detachKeys", Just (encodeQ $ ctrl ++ [c]))]
                WithoutCtrl c -> [("detachKeys", Just (encodeQ [c]))]
                DefaultDetachKey -> []
              ctrl = ['c', 't', 'r', 'l', '-']
getEndpoint (StopContainerEndpoint t cid) = encodeURLWithQuery ["containers", fromContainerID cid, "stop"] query
        where query = case t of
                Timeout x -> [("t", Just (encodeQ $ show x))]
                DefaultTimeout -> []
getEndpoint (KillContainerEndpoint s cid) = encodeURLWithQuery ["containers", fromContainerID cid, "kill"] query
        where query = case s of
                SIG x -> [("signal", Just (encodeQ $ show x))]
                _ -> [("signal", Just (encodeQ $ show s))]
getEndpoint (RestartContainerEndpoint t cid) = encodeURLWithQuery ["containers", fromContainerID cid, "restart"] query
        where query = case t of
                Timeout x -> [("t", Just (encodeQ $ show x))]
                DefaultTimeout -> []
getEndpoint (PauseContainerEndpoint cid) = encodeURL ["containers", fromContainerID cid, "pause"]
getEndpoint (UnpauseContainerEndpoint cid) = encodeURL ["containers", fromContainerID cid, "unpause"]
-- Make use of since/timestamps/tail logopts here instead of ignoreing them
getEndpoint (ContainerLogsEndpoint (LogOpts stdout stderr _ _ _) follow cid) =
            encodeURLWithQuery    ["containers", fromContainerID cid, "logs"] query
        where query = [("stdout", Just (encodeQ $ show stdout)), ("stderr", Just (encodeQ $ show stderr)), ("follow", Just (encodeQ $ show follow))]
getEndpoint (DeleteContainerEndpoint (DeleteOpts removeVolumes force) cid) =
            encodeURLWithQuery ["containers", fromContainerID cid] query
        where query = [("v", Just (encodeQ $ show removeVolumes)), ("force", Just (encodeQ $ show force))]
getEndpoint (InspectContainerEndpoint cid) =
            encodeURLWithQuery ["containers", fromContainerID cid, "json"] []

getEndpointRequestBody :: Endpoint -> Maybe BSL.ByteString
getEndpointRequestBody VersionEndpoint = Nothing
getEndpointRequestBody (ListContainersEndpoint _) = Nothing
getEndpointRequestBody (ListImagesEndpoint _) = Nothing
getEndpointRequestBody (CreateContainerEndpoint opts) = Just $ JSON.encode opts
getEndpointRequestBody (StartContainerEndpoint _ _) = Nothing
getEndpointRequestBody (StopContainerEndpoint _ _) = Nothing
getEndpointRequestBody (KillContainerEndpoint _ _) = Nothing
getEndpointRequestBody (RestartContainerEndpoint _ _) = Nothing
getEndpointRequestBody (PauseContainerEndpoint _) = Nothing
getEndpointRequestBody (UnpauseContainerEndpoint _) = Nothing
getEndpointRequestBody (ContainerLogsEndpoint _ _ _) = Nothing
getEndpointRequestBody (DeleteContainerEndpoint _ _) = Nothing
getEndpointRequestBody (InspectContainerEndpoint _) = Nothing
