module Propellor.Config where


import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Either
import           Data.Functor.Coproduct
import           Data.IP
import           Data.List                    (intersperse, (\\))
import           Data.Maybe
import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Utils
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.REST
import           Propellor                    hiding (Result)
import           Propellor.Base               hiding (head)
import qualified Propellor.Docker             as Docker
import qualified Propellor.Locale             as Locale
import qualified Propellor.Property.Apt       as Apt
import qualified Propellor.Property.Cmd       as Cmd
import qualified Propellor.Property.File      as File
import qualified Propellor.Property.Network   as Net
import           Propellor.Spin
import           System.Directory             (doesFileExist)
import           System.Environment
import           System.Exit
import           System.IO.Error              (isDoesNotExistError)
import           System.Process               (callCommand)

-- | Configure a single host to run docker with a custom network configuration using GRE tunnels
-- and openvswitch to route packets across nodes
multiNetworkDockerHost :: [String] -> Property HasInfo
multiNetworkDockerHost allIps = propertyList ("configuring host " ++ myIp ++ " for multi-network docker") $ props
  & Locale.setDefaultLocale Locale.en_us_UTF_8
  & Docker.installLatestDocker
  & Apt.installed [ "bridge-utils" ]
  -- Assumes .deb are available in current directory
  & installOpenVSwitch
  -- Creates network interfaces
  & createInterfaces allIps myIp
  where
    myIp = head allIps

installOpenVSwitch :: Property NoInfo
installOpenVSwitch =
  check (and <$> mapM doesFileExist debs)
  (runDpkg debs)
  `describe` "installing openvswitch"
  where
    debs = [ "-i", "openvswitch-common_2.3.1-1_amd64.deb",  "openvswitch-switch_2.3.1-1_amd64.deb" ]
    runDpkg debs = cmdPropertyEnv "dpkg" debs noninteractiveEnv

createInterfaces :: [String] -> String -> Property NoInfo
createInterfaces allIps myIp = propertyList "configuring network interfaces"
  [ createOVSBridgeInterface "br0" (length allIps - 1)
  , createGREInterfaces "br0" (allIps \\ [ myIp])
  , createDockerInterface "docker0" allIps myIp
  ]

createOVSBridgeInterface :: String -> Int -> Property NoInfo
createOVSBridgeInterface ifaceName numberOfGREs = hasInterfaceFile `describe` description `requires` Net.interfacesDEnabled
  where
    description = "setup interface " ++ ifaceName ++ " with " ++ show numberOfGREs ++ " tunnels"

    interfaceFile = Net.interfaceDFile ifaceName

    listOfGREs = concat $ intersperse " " $ map (\ i -> "gre" ++ show i) [1 .. numberOfGREs ]

    hasInterfaceFile = interfaceFile `File.hasContent` [ "auto " ++ ifaceName ++  "=" ++ ifaceName
                                                       , "allow-ovs "++ ifaceName
                                                       , "iface " ++ ifaceName ++ " inet manual"
                                                       , "    ovs_type OVSBridge"
                                                       , "    ovs_ports " ++ listOfGREs
                                                       , "    ovs_extra set bridge ${IFACE} stp_enable=true"
                                                       , "    mtu 1462"
                                                       ]

createGREInterfaces :: String -> [ String ] -> Property NoInfo
createGREInterfaces bridgeIfaceName otherIps = propertyList ("Configuring " ++ show numberOfIfaces ++ " GRE interfaces") $
  map (uncurry createGREInterface) (zip otherIps [1 .. numberOfIfaces ])
  where
    numberOfIfaces = length otherIps
    createGREInterface ip num = interfaceFile `File.hasContent` [ "allow-"++ bridgeIfaceName ++ " " ++ greName
                                                                , "iface " ++ greName ++ " inet manual"
                                                                , "    ovs_type OVSPort"
                                                                , "    ovs_bridge " ++ bridgeIfaceName
                                                                , "    ovs_extra set interface ${IFACE} type=gre options:remote_ip=" ++ ip
                                                                ]
      where
        greName = "gre" ++ show num
        interfaceFile = Net.interfaceDFile greName


createDockerInterface :: String -> [ String ] -> String -> Property NoInfo
createDockerInterface ifaceName allIps myIp = undefined

noninteractiveEnv :: [(String, String)]
noninteractiveEnv = [ ("DEBIAN_FRONTEND", "noninteractive")
                    , ("APT_LISTCHANGES_FRONTEND", "none")
                    ]

root :: User
root = User "root"
