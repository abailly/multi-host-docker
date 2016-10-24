{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}

-- | To install latest docker from docker.com
-- Ubuntu is quite far behind, so we don't want to use Propellors' default docker support which will
-- replace a recent docker with ubuntu's one.
module Propellor.Docker (-- * Docker properties
  installLatestDocker
  ,dockerEnabledFor
  ,dockerAuthTokenFor
  ,hasDataContainer
  ,dockerComposeInstalled
   -- ** Docker Containers description
  ,PropDocker.ContainerName
  ,PropDocker.Image
  ,createImage
  ,getContainerIp
  ,run,container,udp,port,volume,name,pull,detach,link,ip,restart,logConfig
  ,LogDriver(..)
  ,RestartPolicy(..)
  ,restartPolicy
  ,tag
   -- * Docker-compose
  ,composeUp, composeDown, composeRm
  ) where

import           Data.Char                 (toLower)
import           Data.List                 as L
import           Propellor.Base            hiding (Port)
import           Propellor.FileMode
import qualified Propellor.Property.Apt    as Apt
import qualified Propellor.Property.Cmd    as C
import qualified Propellor.Property.Docker as PropDocker
import qualified Propellor.Property.File   as File
import qualified Propellor.Property.User   as User
import qualified Propellor.Sudo            as Sudo
import           System.Docker
import           System.Network.Extra      hiding (ip, port)
import           System.Posix.Files

dockerEnabledFor :: User -> Property DebianLike
dockerEnabledFor =  Sudo.binaryEnabledFor "/usr/bin/docker"

-- configure docker authent to pull images from dockerhub
dockerAuthTokenFor :: User -> Property HasInfo
dockerAuthTokenFor (User userName) =
  withPrivData (PrivFile "docker-auth-token") (Context "dev")
  (\ getdata -> property "docker auth configured"
                $ getdata $ \ (PrivData tok) -> liftIO $ do
                  home <- User.homedir $ User userName
                  (writeFile (home </> ".dockercfg")
                   (unlines
                    [ "{"
                    , "\"https://index.docker.io/v1/\":"
                    , "   {\"auth\":\""  ++ tok ++ "\""
                    , ", \"email\":\"dev@capital-match.com\"}"
                    , "}"
                    ]) >> return MadeChange) `catchIO` const (return FailedChange))

installLatestDocker :: Property DebianLike
installLatestDocker = propertyList "install latest docker from official repositories" $ props 
                      & cmdProperty "apt-key" [ "adv"
                                              , "--keyserver"
                                              , "hkp://keyserver.ubuntu.com:80"
                                              , "--recv-keys"
                                              , "36A1D7869245C8950F966E92D8576A8BA88D21E9"
                                              ] `assume` MadeChange
                      & File.containsLines "/etc/apt/sources.list.d/docker.list"
                        ["deb https://get.docker.com/ubuntu docker main"]
                      & Apt.update
                      & Apt.installed [ "lxc-docker" ]


dockerComposeInstalled :: Property UnixLike
dockerComposeInstalled = check (not <$> doesFileExist "/usr/local/bin/docker-compose") $
                         propertyList "docker-compose installed" $ props
                         & scriptProperty [ "curl -L https://github.com/docker/compose/releases/download/1.5.2/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose" ] `assume` MadeChange
                         & File.mode "/usr/local/bin/docker-compose" (combineModes  (ownerWriteMode:readModes ++ executeModes))


-- |Create an image for a data volume container
-- We use an image, so that we can create a user and group inside of it, and ensure the permissions for the data volume inside the container match
-- TODO make user and volume configurable
createImage :: ImageName -> FilePath -> Property DebianLike
createImage (ImageName image) from = property "docker creates image from dockerfile" $ liftIO $ do
  toResult <$> C.boolSystem "docker" (map C.Param ["build","-t",image,from])

-- |Create a data container only if it does not yet exist on the host
-- base image configurable because we might want to change users and what path is in the container
-- might change it to a list of paths later.
-- example: hasDataContainer "cm-data" "capital/app"
hasDataContainer :: PropDocker.ContainerName -> ImageName -> Property DebianLike
hasDataContainer cname (ImageName image) = property "docker creates data-only container" $ liftIO $ do
  (containers,res) <- processTranscript "docker" ["ps", "-a"] Nothing
  if not res
    then  return FailedChange
    else if any (cname `L.isInfixOf`) (lines containers)
            then return NoChange
            else toResult <$> C.boolSystem "docker" (map C.Param [ "create","--name=" <> cname, image
                                                                  , "echo", "'data container for capitalmatch app'"])

-- | Ensures some `docker-compose` configuration is up and running
-- Starts and optionally creates containers defined in given docker-compose file. Containers
-- are started in detached mode with default timeout.
composeUp :: Bool -> FilePath -> Maybe [(String,String)] -> Property UnixLike
composeUp ifPull composeFile envs = check (doesFileExist composeFile) $
                            dockerComposeInstalled `before`
                            if ifPull
                            then (dockerPull  `before` runDockerComposeUp)
                            else runDockerComposeUp
  where
    dockerPull :: Property UnixLike
    dockerPull = property ("docker-compose pull configuration " ++ composeFile) $
                 liftIO $ toResult <$> C.boolSystemEnv "docker-compose" (map C.Param [ "-f", composeFile, "pull"  ] ) envs

    runDockerComposeUp :: Property UnixLike
    runDockerComposeUp = property ("docker-compose up configuration " ++ composeFile) $
                         liftIO $ toResult <$> C.boolSystemEnv "docker-compose" (map C.Param [ "-f", composeFile, "up", "-d"  ] ) envs

composeDown :: FilePath  -> Property UnixLike
composeDown composeFile = check (doesFileExist composeFile) $
                          dockerComposeInstalled `before` runDockerComposeDown
  where
    runDockerComposeDown :: Property UnixLike
    runDockerComposeDown = property ("docker-compose stop configuration " ++ composeFile) $
                           liftIO $ toResult <$> C.boolSystem "docker-compose" (map C.Param [ "-f", composeFile, "stop"  ] )

composeRm :: FilePath  -> Property UnixLike
composeRm composeFile = check (doesFileExist composeFile) $
                          dockerComposeInstalled `before` runDockerComposeRm
  where
    runDockerComposeRm :: Property UnixLike
    runDockerComposeRm = property ("docker-compose remove configuration " ++ composeFile) $
                           liftIO $ toResult <$> C.boolSystem "docker-compose" (map C.Param [ "-f", composeFile, "rm", "-f", "-v" ] )

tag :: ImageName -> ImageName -> Property DebianLike
tag (ImageName from) (ImageName to) = property ("tagging image: " ++ from ++ " with:" ++ to) $
  liftIO $ toResult <$> C.boolSystem "docker" (map C.Param [ "tag", "-f", from, to ] )

pull :: ImageName -> Property DebianLike
pull (ImageName imgName) = property ("docker pulling image " ++ imgName) $ liftIO $ toResult <$> C.boolSystem "docker" (map C.Param [ "pull", imgName ] )

run :: RunParam -> PropDocker.ContainerName -> ImageName -> Property DebianLike
run p cname img = property ("run docker image " ++ show img ++ " as " ++ cname ++ " with " ++ cmdline) $ liftIO $ toResult <$> runDocker p cname img
  where
    cmdline = "running docker container with command-line: " ++ unwords (dockerParams p)

runDocker :: RunParam -> PropDocker.ContainerName -> ImageName -> IO Bool
runDocker p cname img = do
  (containers,res) <- processTranscript "docker" ["ps", "-a"] Nothing
  when (res && any (cname `L.isInfixOf`) (lines containers)) $ do
           -- We stop existing container with same name then starts it...
           void $ C.boolSystem "docker" (map C.Param ["stop", cname])
           void $ C.boolSystem "docker" (map C.Param ["rm", cname])
  C.boolSystem "docker" (map C.Param ("run" : dockerParams (name cname <> p <> Image img)))

getContainerIp :: PropDocker.ContainerName -> IO (Maybe IPInterface)
getContainerIp cname = do
  (contip, res) <- processTranscript "docker" ["inspect","--format","{{ .NetworkSettings.IPAddress }}", cname] Nothing
  return (if not res then Nothing else Just (filter (/= '\n') contip))
