module Main
  ( main
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Spec
import System.Process
import Test.Hspec
import qualified TestConfig as C

main :: IO ()
main = bracket startDocker stopDocker $ const $ hspec spec

startDocker :: IO String
startDocker = do
  output <- readProcess "/usr/bin/env" ["docker", "run", "-p", show C.scheduleStorePort ++ ":27017", "-d", "mongo"] ""
  let containerID = head $ lines output
  putStrLn $ "start container " ++ containerID
  threadDelay $ 1000 * 1000
  return containerID

stopDocker :: String -> IO ()
stopDocker containerID = do
  callProcess "/usr/bin/env" ["docker", "container", "rm", "-f", containerID]
  putStrLn $ "stop container " ++ containerID
