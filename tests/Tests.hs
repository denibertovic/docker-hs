{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import qualified Tests.Integration as Integration
import qualified Tests.JSON as JSON

main :: IO ()
main = do
  useUnixSocket <- lookupEnv "DOCKER_UNIX_SOCKET"
  let runDocker = if isJust useUnixSocket then Integration.runDockerUnix else Integration.runDockerHTTP

  maybeRunIntegrationTests <- lookupEnv "RUN_INTEGRATION_TESTS"
  tests <- case maybeRunIntegrationTests of
             Nothing -> return [JSON.tests]
             Just _ -> do
               integrationTests <- testSpecs $ Integration.tests runDocker
               return $ JSON.tests : integrationTests

  defaultMain $ testGroup "Tests" tests
