module Propellor.Config where


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
import           Propellor.Base
import qualified Propellor.Docker             as Docker
import qualified Propellor.Locale             as Locale
import qualified Propellor.Property.Cmd       as Cmd
import           Propellor.Spin
import           System.Directory             (doesFileExist)
import           System.Environment
import           System.Exit
import           System.IO.Error              (isDoesNotExistError)
import           System.Process               (callCommand)

multiNetworkDockerHost :: String -> Property HasInfo
multiNetworkDockerHost ip = propertyList "configuring host for multi-network docker" $ props
  & Locale.setDefaultLocale Locale.en_us_UTF_8
  & Docker.installLatestDocker
  -- Assumes .deb are available in current directory
  & installOpenVSwitch

installOpenVSwitch :: Property NoInfo
installOpenVSwitch =
  check (and <$> mapM doesFileExist debs)
  (runDpkg debs)
  `describe` "installing openvswitch"
  where
    debs = [ "openvswitch-common_2.3.1-1_amd64.deb",  "openvswitch-switch_2.3.1-1_amd64.deb" ]
    runDpkg debs = cmdPropertyEnv "dpgk -i" debs noninteractiveEnv

noninteractiveEnv :: [(String, String)]
noninteractiveEnv =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

root :: User
root = User "root"
