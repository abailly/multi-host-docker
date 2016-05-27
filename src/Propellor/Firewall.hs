module Propellor.Firewall (firewallPreamble,firewallHttpsDockerSsh,openDevHttpPorts
                         ,flush,dropEverything) where

import           Control.Applicative         ((<$>))
import           Control.Monad.Trans         (liftIO)
import           Propellor
import           Propellor.Property.Firewall as Firewall

-- |A basic rule to drop every input packet
--
-- This should be used as last clause for a bunch of rules, like:
dropEverything :: Property NoInfo
dropEverything =  rule INPUT DROP  Everything

-- |Drop all rules for given chain.
--
-- Useful at start of configuration of firewall rules
flush :: Chain -> Property NoInfo
flush chain = property ( "flushing all rules for chain " <> show chain) $ liftIO $
  toResult <$> boolSystem "iptables" (map Param ["-F", show chain])


firewallPreamble :: Property HasInfo
firewallPreamble = propertyList "standard firewall preamble rules (opens lo and docker0)" $ props
                   & Firewall.installed
                   & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
                   & Firewall.rule INPUT ACCEPT (InIFace "lo")
                   & Firewall.rule INPUT ACCEPT (InIFace "docker0")

openCommonPorts :: Property HasInfo
openCommonPorts =  propertyList "open common operating ports for web" $ props
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- DPort (Port 22))
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- DPort (Port 80))
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- DPort (Port 443))

firewallHttpsDockerSsh :: Property HasInfo
firewallHttpsDockerSsh = propertyList "firewall accepts ssh, http(s) and docker" $ props
                         & flush INPUT
                         & firewallPreamble
                         & openCommonPorts
                         & dropEverything

openDevHttpPorts :: Property HasInfo
openDevHttpPorts = propertyList "firewall accepts standard ports and a dev range for ports" $ props
                   & firewallPreamble
                   & openCommonPorts
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- DPortRange (Port 8080,Port 8099))
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- DPortRange (Port 5900,Port 5903))
                   & dropEverything
