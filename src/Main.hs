{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Either
import           Data.Functor.Coproduct
import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.REST
import           System.Environment
import           System.IO.Error              (isDoesNotExistError)

main :: IO ()
main = do
  [ numberOfHosts, userKey] <- getArgs
  rights <$> createHostsOnDO (read userKey) (read numberOfHosts) >>=
    mapM configureHost                                           >>=
    mapM_ print


createHostsOnDO :: Int -> Int -> IO [ Result Droplet ]
createHostsOnDO userKey n = mapConcurrently (createHostOnDO userKey) [ 1 .. n ]
  where
    createHostOnDO userKey num = do
      authToken <- getAuthFromEnv
      let droplet = BoxConfiguration ("hosts" ++ show num) (RegionSlug "ams2") G1 defaultImage [userKey] False
      runWreq $ pairEffectM (\ _ b -> return b) (mkDOClient $ Tool authToken Nothing False) (injr (createDroplet droplet) :: FreeT (Coproduct DO DropletCommands) (RESTT IO) (Result Droplet))

    getAuthFromEnv :: IO (Maybe AuthToken)
    getAuthFromEnv = (Just `fmap` getEnv "AUTH_TOKEN") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

configureHost :: Droplet -> IO Droplet
configureHost = undefined
