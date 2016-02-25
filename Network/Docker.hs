{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Docker where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.Reader    (ask)
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

getDockerVersion :: DockerM IO (Maybe DockerVersion)
getDockerVersion = do
        clientOpts <- ask
        decodeResponse <$> runDocker (getDockerVersionM clientOpts SVersionEndpoint)

listContainers :: DockerM IO (Maybe [DockerContainer])
listContainers = do
    clientOpts <- ask
    decodeResponse <$> runDocker (listContainersM clientOpts SListContainersEndpoint)

listImages :: DockerM IO (Maybe [DockerImage])
listImages = do
    clientOpts <- ask
    decodeResponse <$> runDocker (listImagesM clientOpts SListImagesEndpoint)

createContainer :: CreateContainerOpts -> DockerM IO (Maybe T.Text)
createContainer createOpts = do
    clientOpts <- ask
    getElemFromResponse "Id" <$> runDocker (createContainerM clientOpts (SCreateContainerEndpoint) createOpts)

startContainer :: String -> StartContainerOpts -> DockerM IO (Status)
startContainer cid startOpts = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (startContainerM clientOpts (SStartContainerEndpoint cid) startOpts)

stopContainer :: String -> DockerM IO (Status)
stopContainer cid = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (stopContainerM clientOpts (SStopContainerEndpoint cid))

killContainer :: String -> DockerM IO (Status)
killContainer cid = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (killContainerM clientOpts (SKillContainerEndpoint cid))

restartContainer :: String -> DockerM IO (Status)
restartContainer cid = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (restartContainerM clientOpts (SRestartContainerEndpoint cid))

pauseContainer :: String -> DockerM IO (Status)
pauseContainer cid = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (pauseContainerM clientOpts (SPauseContainerEndpoint cid))

unpauseContainer :: String -> DockerM IO (Status)
unpauseContainer cid = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (unpauseContainerM clientOpts (SUnpauseContainerEndpoint cid))

deleteContainer :: String -> DockerM IO (Status)
deleteContainer cid = do
        c <- ask
        deleteContainerWithOpts defaultDeleteOpts cid

deleteContainerWithOpts :: DeleteOpts -> String -> DockerM IO (Status)
deleteContainerWithOpts deleteOpts cid = do
    clientOpts <- ask
    (^. responseStatus) <$> runDocker (deleteContainerM clientOpts (SDeleteContainerEndpoint cid deleteOpts))

getContainerLogsWithOpts :: LogOpts -> String -> DockerM IO (L.ByteString)
getContainerLogsWithOpts  l cid = do
    clientOpts <- ask
    (^. responseBody) <$> runDocker (getContainerLogsM clientOpts (SContainerLogsEndpoint cid l))

getContainerLogs :: String -> DockerM IO (L.ByteString)
getContainerLogs cid = do
    clientOpts <- ask
    (^. responseBody) <$> runDocker (getContainerLogsM clientOpts (SContainerLogsEndpoint cid defaultLogOpts))

getContainerLogsStream :: String -> DockerM IO ()
getContainerLogsStream cid = do
        clientOpts <- ask
        req <- PH.parseUrl (fullUrl clientOpts url)
        let req' =  req {PH.method = "GET"}
        liftIO $ PH.withManager PH.defaultManagerSettings $ \m  -> PH.withHTTP req' m  $ \resp -> runEffect $ PH.responseBody resp >-> PB.stdout
    where url = SContainerLogsEndpoint cid defaultLogOpts{follow=True}

