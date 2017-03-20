import           Docker.Client

runNginxContainer :: IO ContainerID
runNginxContainer = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) $
    do let pb = PortBinding 80 TCP [HostPort "0.0.0.0" 8000]
       let myCreateOpts = addPortBinding pb $ defaultCreateOpts "nginx:latest"
       cid <- createContainer myCreateOpts (Just "myNginxContainer")
       case cid of
         Left err -> error $ show err
         Right i -> do
           _ <- startContainer defaultStartOpts i
           return i

stopNginxContainer :: ContainerID -> IO ()
stopNginxContainer cid = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) $
    do r <- stopContainer DefaultTimeout cid
       case r of
         Left _  -> error "I failed to stop the container"
         Right _ -> return ()

runPostgresContainer :: IO ContainerID
runPostgresContainer = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) $
    do let pb = PortBinding 5432 TCP [HostPort "0.0.0.0" 5432]
       let b = Bind "/tmp" "/tmp" Nothing
       let myCreateOpts = addBind b $ addPortBinding pb $ defaultCreateOpts "postgres:9.5"
       cid <- createContainer myCreateOpts Nothing
       case cid of
         Left err -> error $ show err
         Right i -> do
           _ <- startContainer defaultStartOpts i
           return i

-- | Example of how we can start a database container while hosting it's
-- data in a data volume living inside another container.
runPostgresWithDataContainer :: IO ContainerID
runPostgresWithDataContainer = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) $
  -- We use a dummy command like /bin/true because we don't want to start
  -- the postgres database in the data container we just want it to
  -- create a container filesystem for us.
    do let dataOpts = setCmd "/bin/true" $ defaultCreateOpts "postgres:9.5"
       cid <- createContainer dataOpts (Just "myDataContainer")
       _ <-
         case cid of
           Left err -> error $ show err
           Right i  -> startContainer defaultStartOpts i
       let pb = PortBinding 5432 TCP [HostPort "0.0.0.0" 5432]
       -- Default permission is read-write if we don't specify anything
       let vf = VolumeFrom "myDataContainer" Nothing
       let myCreateOpts = addVolumeFrom vf $ addPortBinding pb $ defaultCreateOpts "postgres:9.5"
       ccid <- createContainer myCreateOpts (Just "pgContainer")
       case ccid of
         Left err -> error $ show err
         Right i -> do
           _ <- startContainer defaultStartOpts i
           return i

