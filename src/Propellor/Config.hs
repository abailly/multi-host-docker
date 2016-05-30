module Propellor.Config where


import           Control.Concurrent.Async
import           Control.Exception            (catch, throw)
import           Control.Monad
import           Control.Monad.Trans.Free
import           Data.Either
import           Data.Functor.Coproduct
import           Data.IP
import           Data.List                    (elemIndex, intersperse, (\\))
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
multiNetworkDockerHost :: [String] -> String -> Property HasInfo
multiNetworkDockerHost allIps myIp = propertyList ("configuring host " ++ myIp ++ " for multi-network docker") $ props
  & Locale.setDefaultLocale Locale.en_us_UTF_8
  & Docker.installLatestDocker
  & Apt.installed [ "bridge-utils" ]
  -- Assumes .deb are available in current directory
  & installOpenVSwitch
  -- Creates network interfaces
  & createInterfaces allIps myIp
  & configureDockerDefaults allIps myIp "docker0"

installOpenVSwitch :: Property NoInfo
installOpenVSwitch =
  check (and <$> mapM doesFileExist debs)
  (runDpkg debs)
  `describe` "installing openvswitch from local packages"
  where
    debs = [ "openvswitch-common_2.3.1-1_amd64.deb",  "openvswitch-switch_2.3.1-1_amd64.deb" ]
    runDpkg debs = cmdPropertyEnv "dpkg" ("-i" : debs) noninteractiveEnv

createInterfaces :: [String] -> String -> Property NoInfo
createInterfaces allIps myIp = propertyList "configuring network interfaces"
  [ createOVSBridgeInterface "br0" allIps myIp
  , createGREInterfaces "br0" allIps myIp
  , createDockerInterface "br0" "docker0" allIps myIp
  ]

createOVSBridgeInterface :: String -> [ String ] -> String -> Property NoInfo
createOVSBridgeInterface ifaceName allIps myIp = hasInterfaceFile `describe` description `requires` Net.interfacesDEnabled
  where
    description = "setup interface " ++ ifaceName ++ " with " ++ show (length $ listOfGREs) ++ " tunnels"

    interfaceFile = Net.interfaceDFile ifaceName

    listOfGREs = map snd $ greInterfaces allIps myIp

    hasInterfaceFile = interfaceFile `File.hasContent` [ "auto " ++ ifaceName ++  "=" ++ ifaceName
                                                       , "allow-ovs "++ ifaceName
                                                       , "iface " ++ ifaceName ++ " inet manual"
                                                       , "    ovs_type OVSBridge"
                                                       , "    ovs_ports " ++ concat (intersperse " " listOfGREs)
                                                       , "    ovs_extra set bridge ${IFACE} stp_enable=true"
                                                       , "    mtu 1462"
                                                       ]

createGREInterfaces :: String -> [ String ] -> String -> Property NoInfo
createGREInterfaces bridgeIfaceName allIps myIp = propertyList ("Configuring " ++ show (length allIps) ++ " GRE interfaces") $
  map (uncurry createGREInterface) (greInterfaces allIps myIp)
  where
    createGREInterface ip greName = interfaceFile `File.hasContent` [ "allow-"++ bridgeIfaceName ++ " " ++ greName
                                                                , "iface " ++ greName ++ " inet manual"
                                                                , "    ovs_type OVSPort"
                                                                , "    ovs_bridge " ++ bridgeIfaceName
                                                                , "    ovs_extra set interface ${IFACE} type=gre options:remote_ip=" ++ ip
                                                                ]
      where
        interfaceFile = Net.interfaceDFile greName


-- | Compute the list of interfaces to setup on `myIp` host given all other hosts' IPs
--
-- GRE interfaces name should be consistent between pair of hosts in order to be routed
-- properly, hence assignment should be consistent. We assign a number to all pairs of hosts and
-- filter on those whose source is `myIp`.
greInterfaces :: [ String ] -> String -> [ (String, String) ]
greInterfaces allIps myIp = map keepToAndNumber $ filter (linksToMyIp . snd) allPossibleInterfaces
  where
    greName = ("gre" ++) . show

    linksToMyIp (f,t) | f == myIp && t == myIp = False
                      | f == myIp || t == myIp = True
                      | otherwise           = False
    keepToAndNumber (num, (f,t)) | f == myIp = (t, greName num)
                                 | t == myIp = (f, greName num)
                                 | otherwise = Prelude.error $ "incorrect value " ++ show (f,t) ++ ": expected pair of IPs with at least one equal to " ++ myIp
    allPossibleInterfaces = zip [ 1 .. ] allPairs
    allPairs = [ (from, to) | from <- allIps, to <- Prelude.tail $ dropWhile (/= from) allIps ]

createDockerInterface :: String -> String -> [ String ] -> String -> Property NoInfo
createDockerInterface bridgeIfaceName ifaceName allIps myIp =
  interfaceFile `File.hasContent` [ "auto " ++ ifaceName ++ "=" ++ ifaceName
                                  , "iface " ++ ifaceName ++ " inet static"
                                  , "    address " ++ dockerAddress
                                  , "    network 172.17.0.0"
                                  , "    netmask 255.255.0.0"
                                  , "    bridge_ports " ++ bridgeIfaceName
                                  , "    mtu 1462"
                                  ]
  where
    interfaceFile = Net.interfaceDFile ifaceName
    Just myIndex  = myIp `elemIndex` allIps
    dockerAddress = "172.17.0." ++ show (myIndex + 1)

configureDockerDefaults :: [ String ] -> String -> String -> Property NoInfo
configureDockerDefaults allIps myIp dockerIfaceName  =
  dockerDefaultFile `File.hasContent` [ "BRIDGE=" ++ dockerIfaceName
                                      , "CIDR=" ++ dockerNetworkRange
                                      , "wait_ip() {"
                                      , "  address=$(ip add show $BRIDGE | grep 'inet ' | awk '{print $2}')"
                                      , "  [ -z \"$address\" ] && sleep $1 || :"
                                      , "}"
                                      , "wait_ip 5"
                                      , "wait_ip 15"
                                      , "DOCKER_OPTS=\""
                                      , "    -H unix:///var/run/docker.sock"
                                      , "    -H tcp://0.0.0.0:2375"
                                      , "    --fixed-cidr=$CIDR"
                                      , "    --bridge $BRIDGE"
                                      , "    --mtu 1462"
                                      , "\""
                                      ]
  where
    dockerDefaultFile = "/etc/default/docker"
    Just myIndex  = myIp `elemIndex` allIps
    dockerNetworkRange = "172.17." ++ show (myIndex + 2) ++ ".0/24"

noninteractiveEnv :: [(String, String)]
noninteractiveEnv = [ ("DEBIAN_FRONTEND", "noninteractive")
                    , ("APT_LISTCHANGES_FRONTEND", "none")
                    ]

root :: User
root = User "root"
