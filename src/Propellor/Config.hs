module Propellor.Config where


import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Either
import           Data.Functor.Coproduct
import           Data.IP
import           Data.List                    ((\\))
import           Data.Maybe
import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Utils
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.REST
import           Propellor                    hiding (Result)
import           Propellor.Base
import qualified Propellor.Docker             as Docker
import qualified Propellor.Locale             as Locale
import qualified Propellor.Property.Apt       as Apt
import qualified Propellor.Property.Cmd       as Cmd
import qualified Propellor.Property.Network   as Net
import           Propellor.Spin
import           System.Directory             (doesFileExist)
import           System.Environment
import           System.Exit
import           System.IO.Error              (isDoesNotExistError)
import           System.Process               (callCommand)

-- | Configure a single host to run docker with a custom network configuration using GRE tunnels
-- and openvswitch to route packets across nodes
multiNetworkDockerHost :: [String] -> String -> Property HasInfo
multiNetworkDockerHost hosts ip = propertyList "configuring host for multi-network docker" $ props
  & Locale.setDefaultLocale Locale.en_us_UTF_8
  & Docker.installLatestDocker
  & Apt.installed [ "bridge-utils" ]
  -- Assumes .deb are available in current directory
  & installOpenVSwitch
  -- Creates network interfaces
  & createInterfaces hosts ip

installOpenVSwitch :: Property NoInfo
installOpenVSwitch =
  check (and <$> mapM doesFileExist debs)
  (runDpkg debs)
  `describe` "installing openvswitch"
  where
    debs = [ "openvswitch-common_2.3.1-1_amd64.deb",  "openvswitch-switch_2.3.1-1_amd64.deb" ]
    runDpkg debs = cmdPropertyEnv "dpkg -i" debs noninteractiveEnv

createInterfaces :: [String] -> String -> Property NoInfo
createInterfaces allIps myIp = propertyList "configuring network interfaces"
  [ createOVSBridgeInterface "br0" (length allIps - 1)
  , createGREInterfaces (allIps \\ [ myIp])
  , createDockerInterface "docker0" allIps myIp
  ]

createOVSBridgeInterface :: String -> Int -> Property NoInfo
createOVSBridgeInterface ifaceName numberOfGREs = undefined

createGREInterfaces :: [ String ] -> Property NoInfo
createGREInterfaces otherIps = undefined

createDockerInterface :: String -> [ String ] -> String -> Property NoInfo
createDockerInterface ifaceName allIps myIp = undefined

noninteractiveEnv :: [(String, String)]
noninteractiveEnv =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

root :: User
root = User "root"
