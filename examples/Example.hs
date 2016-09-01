import           Docker.Client

runNginxContainer :: IO ContainerID
runNginxContainer = runDockerT (defaultClientOpts, defaultHttpHandler) $ do
    let pb = PortBinding 80 TCP [HostPort "0.0.0.0" 8000]
    let myCreateOpts = addPortBinding pb $ defaultCreateOpts "nginx:latest"
    cid <- createContainer myCreateOpts (Just "myNginxContainer")
    case cid of
        Left err -> error $ show err
        Right i -> do
            _ <- startContainer defaultStartOpts i
            return i

stopNginxContainer :: ContainerID -> IO ()
stopNginxContainer cid = runDockerT (defaultClientOpts, defaultHttpHandler) $ do
    r <- stopContainer DefaultTimeout cid
    case r of
        Left err -> error "I failed to stop the container"
        Right _ -> return ()

runPostgresContainer :: IO ContainerID
runPostgresContainer = runDockerT (defaultClientOpts, defaultHttpHandler) $ do
    let pb = PortBinding 5432 TCP [HostPort "0.0.0.0" 5432]
    let b = Bind "/tmp" "/tmp" Nothing
    let myCreateOpts = addBind b $ addPortBinding pb $ defaultCreateOpts "postgres:9.5"
    cid <- createContainer myCreateOpts Nothing
    case cid of
        Left err -> error $ show err
        Right i -> do
            _ <- startContainer defaultStartOpts i
            return i

