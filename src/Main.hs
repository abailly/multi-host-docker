{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import qualified Data.ByteString              as BS
import           Data.Either
import           Data.Functor.Coproduct
import           Data.IP
import           Data.List                    (intercalate, intersperse,
                                               isSuffixOf, (\\))
import           Data.Maybe
import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Utils
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.REST
import           Options.Generic
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
                                               callProcess, createProcess, proc,
                                               readProcess)

data Actions = CreateDroplets { numberOfDroplets :: Int
                              , userKey          :: Int
                              , compilePropellor :: Bool
                              , deployPropellor  :: Bool
                              , executable       :: Maybe String   -- default is 'propell'
                              , sourceDir        :: Maybe FilePath -- default is '.'
                              }
             | RunPropellor { allHosts   :: [ HostName ]
                            , executable :: Maybe String  -- default is 'propell'
                            , hostname   ::  HostName
                            }
             | BuildPropellor { sourceDir  :: Maybe FilePath -- default is '.'
                              , targetName :: Maybe String   -- default is 'propell'
                              }
             | BuildOpenVSwitch
             deriving (Show, Generic)

instance ParseRecord Actions

main :: IO ()
main = do
  action <- getRecord "Multi-host docker networking"
  go action

  where
    go (CreateDroplets num userKey compile deploy exe srcDir) = do
      when compile $ void $ buildInDocker srcDir exe
      hosts <- createHostsOnDO userKey num
      when (not $ null $ lefts hosts) $ putStrLn ("Hosts creation failed: " ++ show hosts ) >> exitWith (ExitFailure 1)
      when deploy $ void $ configureHosts (rights hosts)

      print hosts
    go (RunPropellor allHosts exe h) = void $ runPropellor allHosts exe h
    go (BuildPropellor src tgt) = void $ buildInDocker src tgt
    go BuildOpenVSwitch = void $ buildOpenVSwitch

createHostsOnDO :: Int -> Int -> IO [ Result Droplet ]
createHostsOnDO userKey n = do
  putStrLn ("Creating " ++ show n ++ " hosts")
  mapConcurrently (createHostOnDO userKey) [ 1 .. n ]
  where
    createHostOnDO userKey num = do
      authToken <- getAuthFromEnv
      putStrLn $ "creating host " ++ show num ++ " with AUTH_KEY "++ show authToken
      let droplet = BoxConfiguration ("host" ++ show num) (RegionSlug "ams2") G1 defaultImage [userKey] False
      runWreq $ pairEffectM (\ _ b -> return b) (mkDOClient $ Tool Nothing authToken False) (injr (createDroplet droplet) :: FreeT (Coproduct DO DropletCommands) (RESTT IO) (Result Droplet))

    getAuthFromEnv :: IO (Maybe AuthToken)
    getAuthFromEnv = (Just `fmap` getEnv "AUTH_TOKEN") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

configureHosts :: [Droplet] -> IO [Droplet]
configureHosts droplets = do
  mapM (runPropellor hosts $ Just "propell") hosts
  return droplets
  where
    hosts      = configured droplets
    configured = map show . catMaybes . map publicIP

runPropellor :: [ HostName ] -> Maybe String -> HostName -> IO ()
runPropellor allHosts Nothing          h = runPropellor allHosts (Just "propell") h
runPropellor allHosts (Just configExe) h = do
  unlessM (trySsh h 10) $ fail $ "cannot ssh into host " ++ h ++ " after 10s"
  uploadOpenVSwitch ["openvswitch-common_2.3.1-1_amd64.deb",  "openvswitch-switch_2.3.1-1_amd64.deb"] h
  callProcess "scp" [ "-o","StrictHostKeyChecking=no", configExe, "root@" ++ h ++ ":" ]
  callProcess "ssh" [ "-o","StrictHostKeyChecking=no", "root@" ++ h, runRemotePropellCmd h ]
    where
      runRemotePropellCmd h = shellWrap $ intercalate " && " [ "chmod +x " ++ configExe
                                                             , "./" ++ configExe ++ " " ++ h ++ " " ++ otherHosts
                                                             ]
      otherHosts = concat $ intersperse " " (allHosts \\ [h])

      trySsh :: HostName -> Int -> IO Bool
      trySsh h n = do
        res <- boolSystem "ssh" (map Param $ [ "-o","StrictHostKeyChecking=no", "root@" ++ h, "echo hello" ])
        if (not res && n > 0)
          then do
            threadDelay 1000000
            trySsh h (n - 1)
          else return res

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM test ifFail = do
  result <- test
  when (not result) $ ifFail

copy :: Handle -> Handle -> IO ()
copy hIn hOut = do
  bs <- BS.hGet hIn 4096
  if not (BS.null bs)
    then BS.hPut hOut bs >> copy hIn hOut
    else return ()

buildInDocker :: Maybe FilePath -> Maybe String -> IO FilePath
buildInDocker Nothing        Nothing          = buildInDocker (Just ".") (Just "propell")
buildInDocker Nothing       (Just exe)        = buildInDocker (Just ".") (Just exe)
buildInDocker (Just src)     Nothing          = buildInDocker (Just src) (Just "propell")
buildInDocker (Just srcDir) (Just targetName) = do
  absSrcDir <- canonicalizePath srcDir
  buildAlreadyRun <- doesFileExist ".cidfile"
  if buildAlreadyRun
    then do
    cid <- readFile ".cidfile"
    removeFile ".cidfile"
    callProcess "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build", "--volumes-from=" ++ cid,
                          "-v", "/root/.stack", "-w", "/build" , "haskell:7.10.3","stack", "build","--allow-different-user", ":" ++ targetName ]
    else callProcess "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build",
                               "-v", "/root/.stack", "-w", "/build" , "haskell:7.10.3","stack", "build","--allow-different-user", ":" ++ targetName ]

  exportBinary targetName

buildOpenVSwitch :: IO [ FilePath ]
buildOpenVSwitch = do
  callProcess "docker" ["build", "-t", imageName, "openvswitch" ]
  debs <- filter (".deb" `isSuffixOf`) . lines <$> readProcess "docker" [ "run", "--rm", imageName, "ls",  "-1", "/" ] ""
  localDebs <- forM debs extractPackage
  return localDebs
    where
      imageName = "openvswitch:2.3.1"

      extractPackage deb = do
        (_, Just hout, _, phdl) <- createProcess  (proc "docker" [ "run", "--rm", imageName, "dd", "if=/" ++ deb ]) { std_out = CreatePipe }
        withBinaryFile deb WriteMode $ \ hDst -> copy hout hDst
        void $ waitForProcess phdl
        return deb

uploadOpenVSwitch :: [ FilePath ] -> String -> IO ()
uploadOpenVSwitch debs host = forM_ debs $ \ deb -> callProcess "scp" [ "-o","StrictHostKeyChecking=no", deb, "root@" ++ host ++ ":" ]

exportBinary :: String -> IO FilePath
exportBinary targetName = do
  cid <- readFile ".cidfile"
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--rm", "--volumes-from=" ++ cid,  "-w", "/build", "haskell:7.10.3", "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--rm", "--volumes-from=" ++ cid, "busybox","dd", "if=" ++ stackRoot ++ "/bin/" ++ targetName ]) { std_out = CreatePipe }
  withBinaryFile targetName WriteMode $ \ hDst -> copy hout hDst
  void $ waitForProcess phdl
  return targetName

