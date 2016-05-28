{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Either
import           Data.Functor.Coproduct
import           Data.IP
import           Data.Maybe
import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Utils
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.REST
import           Propellor                    hiding (Result)
import           Propellor.Config
import qualified Propellor.Docker             as Docker
import qualified Propellor.Locale             as Locale
import qualified Propellor.Property.Cmd       as Cmd
import           Propellor.Spin
import           System.Environment
import           System.Exit
import           System.IO.Error              (isDoesNotExistError)
import           System.Process               (callCommand)

main :: IO ()
main = do
  [ numberOfHosts, userKey] <- getArgs
  hosts <- createHostsOnDO (read userKey) (read numberOfHosts)
  if not $ null $ lefts hosts
    then putStrLn ("Hosts creation failed: " ++ show hosts ) >> exitWith (ExitFailure 1)
    else configureHosts (rights hosts)  >>= mapM_ print

createHostsOnDO :: Int -> Int -> IO [ Result Droplet ]
createHostsOnDO userKey n = putStrLn ("Creating " ++ show n ++ " hosts") >> mapConcurrently (createHostOnDO userKey) [ 1 .. n ]
  where
    createHostOnDO userKey num = do
      authToken <- getAuthFromEnv
      putStrLn $ "creating host " ++ show num ++ " with AUTH_KEY "++ show authToken
      let droplet = BoxConfiguration ("hosts" ++ show num) (RegionSlug "ams2") G1 defaultImage [userKey] False
      runWreq $ pairEffectM (\ _ b -> return b) (mkDOClient $ Tool Nothing authToken False) (injr (createDroplet droplet) :: FreeT (Coproduct DO DropletCommands) (RESTT IO) (Result Droplet))

    getAuthFromEnv :: IO (Maybe AuthToken)
    getAuthFromEnv = (Just `fmap` getEnv "AUTH_TOKEN") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

configureHosts :: [Droplet] -> IO [Droplet]
configureHosts droplets = do
  mapM (acceptHostsKey . publicIP) droplets
  runPropellor $ configured droplets
  return droplets
  where
    configured  = map (toConfigure . show) . catMaybes . map publicIP
    runPropellor = mapM (uncurry $ spin Nothing)
    toConfigure ip = (ip, host ip & multiNetworkDockerHost ip)


-- | The host should have been created
acceptHostsKey :: Maybe IP -> IO ()
acceptHostsKey (Just ip) =
  mapM_ callCommand [ "ssh-keygen -R " ++ show ip
                    , "cp ~/.ssh/known_hosts ~/.ssh/known_hosts.old"
                    , "ssh-keyscan " ++  show ip ++ " > ~/.ssh/known_hosts"
                    ]
acceptHostsKey (Nothing) = return ()

