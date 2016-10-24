module Main where

import           Propellor          hiding (Result)
import           Propellor.Config
import           Propellor.Engine
import           System.Environment

main :: IO ()
main = do
  (myIp:allIps) <- getArgs   -- assume first IP given is own ip
  let toConfigure = host myIp $ props & multiNetworkDockerHost allIps myIp
  mainProperties toConfigure
