{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Test.QuickCheck.Monadic   as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck     as QC

import           Control.Concurrent        (threadDelay)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as BL
import           Data.Maybe                (fromJust, isJust)
import           Data.Monoid
import           Data.Text                 (unpack)
import           Network.Connection        (TLSSettings (..))
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
import           System.Process            (system)

import           Docker
import           Docker.Types


-- opts = defaultClientOpts

testImageName = "docker-hs-test"

toStrict1 = B.concat . BL.toChunks

runDocker f = do
    -- let opts = defaultClientOpts {baseUrl = "https://127.0.0.1:2376/"}
    -- params' <- clientParamsWithClientAuthentication "127.0.0.1" 2376 "~/.docker/test-client-key.pem" "~/.docker/test-client-cert.pem" >>= fromRight
    -- params <- clientParamsSetCA params' "~/.docker/test-ca.pem"
    -- let settings = mkManagerSettings (TLSSettings params) Nothing
    -- mgr <- newManager settings
    runDockerT (defaultClientOpts, defaultHttpHandler) f

checkDockerVersion :: IO ()
checkDockerVersion = runDocker $ do
    v <- getDockerVersion
    lift $ assert $ isRight v

findTestImage :: IO ()
findTestImage = runDocker $ do
    images <- listImages defaultListOpts >>= fromRight
    let x = filter ((== [testImageName<>":latest"]) . imageRepoTags) images
    lift $ assert $ length x == 1

runAndReadLog :: IO ()
runAndReadLog = runDocker $ do
    containerId <- createContainer (defaultCreateOpts (testImageName <> ":latest"))
    c <- fromRight containerId
    status1 <- startContainer defaultStartOpts c
    _ <- inspectContainer c >>= fromRight
    lift $ threadDelay 300000 -- give 300ms for the application to finish
    lift $ assert $ status1 == Right ()
    status2 <- killContainer SIGTERM c
    logs <- getContainerLogs defaultLogOpts c >>= fromRight
    lift $ assert $ status2 == Right ()
    lift $ assert $ (C.pack "123") `C.isInfixOf` (toStrict1 logs)
    status3 <- deleteContainer (DeleteOpts True True) c
    lift $ assert $ status3 == Right ()


tests :: TestTree
tests = testGroup "Metrics tests" [
    testCase "Get docker version" checkDockerVersion,
    testCase "Find image by name" findTestImage,
    testCase "Run a dummy container and read its log" runAndReadLog]

setup :: IO ()
setup =  system ("docker build -t "++unpack testImageName++" tests") >> return ()

main :: IO ()
main = do
  setup
  defaultMain tests

isRight (Left _) = False
isRight (Right _) = True

fromRight :: (MonadIO m, Show l) => Either l r -> m r
fromRight (Left l) = do
    liftIO $ assertFailure $ "Left: " ++ show l
    undefined
fromRight (Right r) = return r
