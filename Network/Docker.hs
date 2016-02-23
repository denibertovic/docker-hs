{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Docker where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Lens
import           Control.Monad.Free
import           Data.Aeson              (FromJSON, ToJSON, Value, decode,
                                          eitherDecode, toJSON)
import           Data.Aeson.Lens         (key, _String)
import qualified Data.ByteString.Lazy    as L
import           Data.Char
import qualified Data.Text               as T
import           Network.Wreq
import           Pipes
import           Pipes
import qualified Pipes.ByteString        as PB
import qualified Pipes.HTTP              as PH
import           Text.Printf             (printf)

import           Network.Docker.Internal
import           Network.Docker.Types

getDockerVersion :: DockerClientOpts -> IO (Maybe DockerVersion)
getDockerVersion clientOpts = decodeResponse <$>
    runDocker (getDockerVersionM clientOpts SVersionEndpoint)

listContainers :: DockerClientOpts -> IO (Maybe [DockerContainer])
listContainers clientOpts = decodeResponse <$>
    runDocker (listContainersM clientOpts SListContainersEndpoint)

listImages :: DockerClientOpts -> IO (Maybe [DockerImage])
listImages clientOpts = decodeResponse <$>
    runDocker (listImagesM clientOpts SListImagesEndpoint)

createContainer :: DockerClientOpts -> CreateContainerOpts -> IO(Maybe T.Text)
createContainer clientOpts createOpts = getElemFromResponse "Id" <$>
    runDocker (createContainerM clientOpts (SCreateContainerEndpoint) createOpts)

startContainer :: DockerClientOpts -> String -> StartContainerOpts -> IO(Status)
startContainer clientOpts cid startOpts = (^. responseStatus) <$>
    runDocker (startContainerM clientOpts (SStartContainerEndpoint cid) startOpts)

stopContainer :: DockerClientOpts -> String -> IO (Status)
stopContainer clientOpts cid = (^. responseStatus) <$>
    runDocker (stopContainerM clientOpts (SStopContainerEndpoint cid))

killContainer :: DockerClientOpts -> String -> IO (Status)
killContainer clientOpts cid = (^. responseStatus) <$>
    runDocker (killContainerM clientOpts (SKillContainerEndpoint cid))

restartContainer :: DockerClientOpts -> String -> IO (Status)
restartContainer clientOpts cid = (^. responseStatus) <$>
    runDocker (restartContainerM clientOpts (SRestartContainerEndpoint cid))

pauseContainer :: DockerClientOpts -> String -> IO (Status)
pauseContainer clientOpts cid = (^. responseStatus) <$>
    runDocker (pauseContainerM clientOpts (SPauseContainerEndpoint cid))

unpauseContainer :: DockerClientOpts -> String -> IO (Status)
unpauseContainer clientOpts cid = (^. responseStatus) <$>
    runDocker (unpauseContainerM clientOpts (SUnpauseContainerEndpoint cid))

deleteContainer :: DockerClientOpts -> String -> IO (Status)
deleteContainer c cid = deleteContainerWithOpts c defaultDeleteOpts cid

deleteContainerWithOpts :: DockerClientOpts -> DeleteOpts -> String -> IO (Status)
deleteContainerWithOpts clientOpts deleteOpts cid = (^. responseStatus) <$>
    runDocker (deleteContainerM clientOpts (SDeleteContainerEndpoint cid deleteOpts))

getContainerLogsWithOpts :: DockerClientOpts ->  LogOpts -> String -> IO (L.ByteString)
getContainerLogsWithOpts  clientOpts l cid = (^. responseBody) <$>
    runDocker (getContainerLogsM clientOpts (SContainerLogsEndpoint cid l))

getContainerLogs :: DockerClientOpts -> String -> IO (L.ByteString)
getContainerLogs clientOpts cid = (^. responseBody) <$>
    runDocker (getContainerLogsM clientOpts (SContainerLogsEndpoint cid defaultLogOpts))

getContainerLogsStream :: DockerClientOpts -> String -> IO ()
getContainerLogsStream clientOpts cid = do
                req <- PH.parseUrl (fullUrl clientOpts url)
                let req' =  req {PH.method = "GET"}
                PH.withManager PH.defaultManagerSettings $ \m  -> PH.withHTTP req' m  $ \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
        where url = SContainerLogsEndpoint cid defaultLogOpts{follow=True}

