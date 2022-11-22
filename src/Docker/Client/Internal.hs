module Docker.Client.Internal where

import           Blaze.ByteString.Builder (toByteString)
import qualified Data.Aeson               as JSON
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as BSC
import qualified Data.Conduit.Binary      as CB
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Client      as HTTP
import           Network.HTTP.Conduit     (requestBodySourceChunked, RequestBody(RequestBodyBS))
import           Network.HTTP.Types       (Query, encodePath,
                                           encodePathSegments)
import           Prelude                  hiding (all)

import           Docker.Client.Types


encodeURL :: [T.Text] -> T.Text
encodeURL ps = decodeUtf8 $ toByteString $ encodePathSegments ps

encodeURLWithQuery :: [T.Text] -> Query -> T.Text
encodeURLWithQuery ps q = decodeUtf8 $ toByteString $ encodePath ps q

encodeQ :: String -> ByteString
encodeQ = encodeUtf8 . T.pack

getEndpoint :: ApiVersion -> Endpoint -> T.Text
getEndpoint v VersionEndpoint = encodeURL [v, "version"]
getEndpoint v (ListContainersEndpoint (ListOpts all)) = encodeURLWithQuery [v, "containers", "json"] [("all", Just (encodeQ $ show all))]
getEndpoint v (ListImagesEndpoint _) = encodeURL [v, "images", "json"] -- Make use of lsOpts here
getEndpoint v (CreateContainerEndpoint _ cn) = case cn of
        Just cn -> encodeURLWithQuery [v, "containers", "create"] [("name", Just (encodeQ $ T.unpack cn))]
        Nothing -> encodeURL [v, "containers", "create"]
getEndpoint v (StartContainerEndpoint startOpts cid) = encodeURLWithQuery [v, "containers", fromContainerID cid, "start"] query
        where query = case (detachKeys startOpts) of
                WithCtrl c -> [("detachKeys", Just (encodeQ $ ctrl ++ [c]))]
                WithoutCtrl c -> [("detachKeys", Just (encodeQ [c]))]
                DefaultDetachKey -> []
              ctrl = ['c', 't', 'r', 'l', '-']
getEndpoint v (StopContainerEndpoint t cid) = encodeURLWithQuery [v, "containers", fromContainerID cid, "stop"] query
        where query = case t of
                Timeout x      -> [("t", Just (encodeQ $ show x))]
                DefaultTimeout -> []
getEndpoint v (WaitContainerEndpoint cid) = encodeURL [v, "containers", fromContainerID cid, "wait"]
getEndpoint v (KillContainerEndpoint s cid) = encodeURLWithQuery [v, "containers", fromContainerID cid, "kill"] query
        where query = case s of
                SIG x -> [("signal", Just (encodeQ $ show x))]
                _     -> [("signal", Just (encodeQ $ show s))]
getEndpoint v (RestartContainerEndpoint t cid) = encodeURLWithQuery [v, "containers", fromContainerID cid, "restart"] query
        where query = case t of
                Timeout x      -> [("t", Just (encodeQ $ show x))]
                DefaultTimeout -> []
getEndpoint v (PauseContainerEndpoint cid) = encodeURL [v, "containers", fromContainerID cid, "pause"]
getEndpoint v (UnpauseContainerEndpoint cid) = encodeURL [v, "containers", fromContainerID cid, "unpause"]
-- Make use of since/timestamps/tail logopts here instead of ignoreing them
getEndpoint v (ContainerLogsEndpoint (LogOpts stdout stderr _ _ _) follow cid) =
            encodeURLWithQuery    [v, "containers", fromContainerID cid, "logs"] query
        where query = [("stdout", Just (encodeQ $ show stdout)), ("stderr", Just (encodeQ $ show stderr)), ("follow", Just (encodeQ $ show follow))]
getEndpoint v (DeleteContainerEndpoint (ContainerDeleteOpts removeVolumes force) cid) =
            encodeURLWithQuery [v, "containers", fromContainerID cid] query
        where query = [("v", Just (encodeQ $ show removeVolumes)), ("force", Just (encodeQ $ show force))]
getEndpoint v (InspectContainerEndpoint cid) =
            encodeURLWithQuery [v, "containers", fromContainerID cid, "json"] []
getEndpoint v (BuildImageEndpoint o _) = encodeURLWithQuery [v, "build"] query
        where query = [("t", Just t), ("dockerfile", Just dockerfile), ("q", Just q), ("nocache", Just nocache), ("rm", Just rm), ("forcerm", Just forcerm), ("pull", Just pull)]
              t = encodeQ $ T.unpack $ buildImageName o
              dockerfile = encodeQ $ T.unpack $ buildDockerfileName o
              q = encodeQ $ show $ buildQuiet o
              nocache = encodeQ $ show $ buildNoCache o
              rm = encodeQ $ show $ buildRemoveItermediate o
              forcerm = encodeQ $ show $ buildForceRemoveIntermediate o
              pull = encodeQ $ show $ buildPullParent o
getEndpoint v (CreateImageEndpoint name tag _) = encodeURLWithQuery [v, "images", "create"] query
        where query = [("fromImage", Just n), ("tag", Just t)]
              n = encodeQ $ T.unpack name
              t = encodeQ $ T.unpack tag
getEndpoint v (LoadImageEndpoint quiet _) = encodeURLWithQuery [v, "images", "load"] query
        where query = [("quiet", Just $ encodeQ $ show quiet)]
getEndpoint v (DeleteImageEndpoint _ cid) = encodeURL [v, "images", fromImageID cid]
getEndpoint v (CreateNetworkEndpoint _) = encodeURL [v, "networks", "create"]
getEndpoint v (RemoveNetworkEndpoint nid) = encodeURL [v, "networks", fromNetworkID nid]

getEndpointRequestBody :: Endpoint -> Maybe HTTP.RequestBody
getEndpointRequestBody VersionEndpoint = Nothing
getEndpointRequestBody (ListContainersEndpoint _) = Nothing
getEndpointRequestBody (ListImagesEndpoint _) = Nothing
getEndpointRequestBody (CreateContainerEndpoint opts _) = Just $ HTTP.RequestBodyLBS (JSON.encode opts)
getEndpointRequestBody (StartContainerEndpoint _ _) = Nothing
getEndpointRequestBody (StopContainerEndpoint _ _) = Nothing
getEndpointRequestBody (WaitContainerEndpoint _) = Nothing
getEndpointRequestBody (KillContainerEndpoint _ _) = Nothing
getEndpointRequestBody (RestartContainerEndpoint _ _) = Nothing
getEndpointRequestBody (PauseContainerEndpoint _) = Nothing
getEndpointRequestBody (UnpauseContainerEndpoint _) = Nothing
getEndpointRequestBody (ContainerLogsEndpoint _ _ _) = Nothing
getEndpointRequestBody (DeleteContainerEndpoint _ _) = Nothing
getEndpointRequestBody (InspectContainerEndpoint _) = Nothing

getEndpointRequestBody (BuildImageEndpoint _ fp) = Just $ requestBodySourceChunked $ CB.sourceFile fp
getEndpointRequestBody (CreateImageEndpoint _ _ _) = Nothing
getEndpointRequestBody (LoadImageEndpoint _ fp) = Just $ requestBodySourceChunked $ CB.sourceFile fp
getEndpointRequestBody (DeleteImageEndpoint _ _) = Nothing

getEndpointRequestBody (CreateNetworkEndpoint opts) = Just $ HTTP.RequestBodyLBS (JSON.encode opts)
getEndpointRequestBody (RemoveNetworkEndpoint _) = Nothing

getEndpointContentType :: Endpoint -> BSC.ByteString
getEndpointContentType (BuildImageEndpoint _ _) = BSC.pack "application/tar"
getEndpointContentType (LoadImageEndpoint _ _) = BSC.pack "application/tar"
getEndpointContentType _ = BSC.pack "application/json; charset=utf-8"

#if MIN_VERSION_http_client(0,5,0)
getEndpointTimeout :: Endpoint -> HTTP.ResponseTimeout
getEndpointTimeout (WaitContainerEndpoint _) = HTTP.responseTimeoutNone
getEndpointTimeout _ = HTTP.responseTimeoutDefault
#else
-- Prior to version 0.5.0 of `http-client`, `ResponseTimeout` does not exist
-- and we can't easily say "use the manager setting" here. So this is a bit
-- ugly and only exists for the sake of backwards compatibility.
getEndpointTimeout :: Endpoint -> Maybe Int
getEndpointTimeout (WaitContainerEndpoint _) = Nothing
getEndpointTimeout _ = Just 30000000
#endif
