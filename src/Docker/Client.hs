{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Docker.Client
Copyright   : (c) Deni Bertovic, 2016
License     : BSD3
Maintainer  : deni@denibertovic.com
Stability   : experimental
Portability : POSIX

= Getting started

Creating a container:

We're going to create a nginx container and we're going to map port @80@ from within the container to port @8000@ on
the host.

@
import           Docker.Client

runNginxContainer :: IO ContainerID
runNginxContainer = runDockerT (defaultClientOpts, defaultHttpHandler) $ do
    let pb = PortBinding 80 TCP [HostPort "0.0.0.0" 8000]
    let myCreateOpts = addPortBinding pb $ defaultCreateOpts "nginx:latest"
    cid <- createContainer myCreateOpts
    case cid of
        Left err -> error $ show err
        Right i -> do
            _ <- startContainer defaultStartOpts i
            return i
@

Let's start out nginx container:

>>> cid <- runNginxContainer


Visit http://localhost:8000 and verify that nginx is running.

Let's stop the container now:

@
stopNginxContainer :: ContainerID -> IO ()
stopNginxContainer cid = runDockerT (defaultClientOpts, defaultHttpHandler) $ do
    r <- stopContainer DefaultTimeout cid
    case r of
        Left err -> error "I failed to stop the container"
        Right _ -> return ()
@

>>> stopNginxContainer cid

Let's start a Postgres container by mapping the \/tmp directory from within the container to the
\/tmp directory on the host. That way we make sure that the data we write to \/tmp in the container will
persist on the host file system.

@
runPostgresContainer :: IO ContainerID
runPostgresContainer = runDockerT (defaultClientOpts, defaultHttpHandler) $ do
    let pb = PortBinding 5432 TCP [HostPort "0.0.0.0" 5432]
    let myCreateOpts = addBinds [Bind "\/tmp" "\/tmp" Nothing] $ addPortBinding pb $ defaultCreateOpts "postgres:9.5"
    cid <- createContainer myCreateOpts
    case cid of
        Left err -> error $ show err
        Right i -> do
            _ <- startContainer defaultStartOpts i
            return i
@

= Get Docker API Version

>>> runDockerT (defaultClientOpts, defaultHttpHandler) $ getDockerVersion
Right (DockerVersion {version = "1.12.0", apiVersion = "1.24", gitCommit = "8eab29e", goVersion = "go1.6.3", os = "linux", arch = "amd64", kernelVersion = "4.6.0-1-amd64", buildTime = "2016-07-28T21:46:40.664812891+00:00"})

= Setup SSL Authentication

Let's create a custom 'HttpHandler' that uses a client's certificate and private key for SSL authentication.
It also accepts a self-signed CA certificate which is specified via 'clientParamsSetCA'.
This handler can replace 'defaultHttpHandler' in the arguments to 'runDockerT'.

@
let host = "domain.name"
let port = fromInteger 4000
let privKey = "path\/to\/private\/key"
let cert = "path\/to\/certificate"
let ca = "path\/to\/CA"
paramsE <- clientParamsWithClientAuthentication host port privKey cert
case paramsE of
    Left err ->
        error err
    Right params' -> do
        params <- clientParamsSetCA params' ca
        settings <- HTTP.mkManagerSettings (TLSSettings params) Nothing
        manager <- newManager settings
        return $ httpHandler manager
@


-}
module Docker.Client (
    -- * Client functions
      module Docker.Client.Api
    -- * Types
    , module Docker.Client.Types
    -- * Http
    , module Docker.Client.Http
    ) where

import           Docker.Client.Api
import           Docker.Client.Http
import           Docker.Client.Types
