module Main where

import           Propellor          hiding (Result)
import           Propellor.Config
import           Propellor.Engine
import           System.Environment

main :: IO ()
main = do
  [ ip ] <- getArgs
  let toConfigure = host ip & multiNetworkDockerHost ip
  mainProperties toConfigure
