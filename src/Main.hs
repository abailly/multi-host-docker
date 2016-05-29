{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import qualified Data.ByteString              as BS
import           Data.Either
import           Data.Functor.Coproduct
import           Data.IP
import           Data.List                    (intercalate)
import           Data.Maybe
import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Utils
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.REST
import           Propellor                    hiding (Result, createProcess)
import           Propellor.Config
import qualified Propellor.Docker             as Docker
import qualified Propellor.Locale             as Locale
import qualified Propellor.Property.Cmd       as Cmd
import           Propellor.Spin
import           Propellor.Utilities          (shellWrap)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error              (isDoesNotExistError)
import           System.Process               (CreateProcess (..),
                                               StdStream (..), callCommand,
                                               createProcess, proc, readProcess)

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
  mapM (runPropellor "propell") $ configured droplets
  return droplets
  where
    configured  = map show . catMaybes . map publicIP
    toConfigure ip = (ip, host ip & multiNetworkDockerHost ip)

copy :: Handle -> Handle -> IO ()
copy hIn hOut = do
  bs <- BS.hGet hIn 4096
  if not (BS.null bs)
    then BS.hPut hOut bs >> copy hIn hOut
    else return ()

buildInDocker :: FilePath -> String -> IO FilePath
buildInDocker srcDir targetName = do
  absSrcDir <- canonicalizePath srcDir
  removeFileIfExists ".cidfile"
  (_,_,_,hdl) <- createProcess $ proc "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build", "-w", "/build" , "haskell:7.10.3","stack", "build","--allow-different-user", ":" ++ targetName ]
  exitCode <- waitForProcess hdl
  case exitCode of
    ExitSuccess      -> exportBinary targetName
    ExitFailure code -> fail $ "failed to build correctly " ++ targetName ++ " in directory " ++ srcDir ++ ": " ++ show code

    where
      removeFileIfExists fp = do
        exist <- doesFileExist fp
        when exist $ removeFile fp

exportBinary :: String -> IO FilePath
exportBinary targetName = do
  cid <- readFile ".cidfile"
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--volumes-from=" ++ cid,  "-w", "/build", "haskell:7.10.3", "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--volumes-from=" ++ cid, "busybox","dd", "if=" ++ stackRoot ++ "/bin/" ++ targetName ]) { std_out = CreatePipe }
  withBinaryFile targetName WriteMode $ \ hDst -> copy hout hDst
  void $ waitForProcess phdl
  return targetName

runPropellor :: String -> HostName -> IO Bool
runPropellor configExe h = do
  copied <- boolSystem "scp" (map Param $ [ "-o","StrictHostKeyChecking=no", configExe, "root@" ++ h ++ ":" ])
  if copied
    then boolSystem "ssh" (map Param $ [ "-o","StrictHostKeyChecking=no", "root@" ++ h, runRemotePropellCmd h ])
    else fail $ "failed to copy " ++ configExe ++ " to remote host " ++ h
    where
      runRemotePropellCmd h = shellWrap $ intercalate " && " [ "chmod +x " ++ configExe
                                                             , "./" ++ configExe ++ " " ++ h
                                                             ]

