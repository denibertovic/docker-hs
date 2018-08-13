{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

import qualified Tests.Integration as Integration
import qualified Tests.JSON as JSON

main :: IO ()
main = do
  -- useUnixSocket <- lookupEnv "DOCKER_UNIX_SOCKET"
  let useUnixSocket = Just True
  let runDocker = if isJust useUnixSocket then Integration.runDockerUnix else Integration.runDockerHTTP

  maybeRunIntegrationTests <- lookupEnv "RUN_INTEGRATION_TESTS"
  let tests = case maybeRunIntegrationTests of
        Nothing -> [JSON.tests]
        Just _ -> [JSON.tests, Integration.tests runDocker]

  defaultMain $ testGroup "Tests" tests
