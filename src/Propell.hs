module Main where

import           Propellor          hiding (Result)
import           Propellor.Config
import           Propellor.Engine
import           System.Environment

main :: IO ()
main = do
  ips <- getArgs   -- assume first IP given is own ip
  let toConfigure = host (head ips) & multiNetworkDockerHost ips
  mainProperties toConfigure
