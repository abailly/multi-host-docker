{-# LANGUAGE DeriveGeneric #-}
module Types(Actions(..), options) where

import           Options.Applicative

type HostName = String

data Actions = CreateDroplets { numberOfDroplets :: Int
                              , userKey          :: Int
                              , compilePropellor :: Bool
                              , deployPropellor  :: Bool
                              , executable       :: String   -- default is 'propell'
                              , sourceDir        :: FilePath -- default is '.'
                              , imageName        :: String    -- default is 'fpco/stack-build'
                              }
             | RunPropellor { allHosts   :: [ HostName ]
                            , executable :: String  -- default is 'propell'
                            , hostname   :: HostName
                            }
             | BuildPropellor { sourceDir  :: FilePath -- default is '.'
                              , targetName :: String   -- default is 'propell'
                              , imageName  :: String    -- default is 'fpco/stack-build'
                              }
             | BuildOpenVSwitch
             deriving (Show)


options = execParser $ info (helper <*> configOptions)
          ( fullDesc
            <> progDesc "An experiment in automated creation and configuration of droplets to host a docker network"
            <> header "multi-host-docker-network" )

configOptions :: Parser Actions
configOptions = subparser
                ( command "create-droplets"
                  (info createDropletsOptions
                    (progDesc "Create (and configure) droplets to be part of a multi-host docker network"))
                  <> command "build-propellor"
                  (info buildPropellorOptions
                    (progDesc "Build propellor program to configure droplets remotely (see http://propellor.branchable.com"))
                  <> command "run-propellor"
                  (info runPropellorOptions
                    (progDesc "Configure a single droplet with built propellor executable"))
                  <> command "build-openvswitch"
                  (info (pure BuildOpenVSwitch)
                    (progDesc "Build openvswitch packages to be deployed remotely"))
                  )

intOption :: Mod OptionFields String -> Parser Int
intOption = (read <$>) . strOption

createDropletsOptions :: Parser Actions
createDropletsOptions =
  CreateDroplets 
  <$> intOption ( long "number-of-droplets"
                  <> short 'n'
                  <> value "3"
                  <> metavar "INTEGER"
                  <> help "Number of droplets to create that will be part of the network" )
  <*> intOption ( long "user-key"
                  <> short 'k'
                  <> metavar "INTEGER"
                  <> help "User key to add to DO droplets configuration" )
  <*> switch     ( long "compile"
                 <> short 'C'
                 <> help "Compile propellor using stack + docker?")
  <*> switch     ( long "deploy"
                 <> short 'D'
                 <> help "Run propellor to configure remote hosts")
  <*> strOption ( long "executable"
                  <> short 'x'
                  <> value "propell"
                  <> metavar "FILE"
                  <> help "Name of executable file built and extracted for configuration (default: propell)")
  <*> strOption ( long "source-directory"
                  <> short 's'
                  <> value "."
                  <> metavar "DIRECTORY"
                  <> help "Directory to use for building configuration executable (default: .)")
  <*> strOption ( long "image-name"
                  <> short 'i'
                  <> value "fpco/stack-build"
                  <> metavar "REPO"
                  <> help "Stack repository name for building configuration executable (default: fpco/stack-build)")

runPropellorOptions :: Parser Actions
runPropellorOptions =
  RunPropellor
  <$> option auto ( long "all-hosts"
                    <> short 'a'
                    <> metavar "LIST OF HOSTS"
                    <> help "List of IPs/Names of all hosts part of the network" )
  <*> strOption ( long "executable"
                  <> short 'x'
                  <> value "propell"
                  <> metavar "FILE"
                  <> help "Name of executable file built and extracted for configuration (default: propell)")
  <*> strOption ( long "hostname"
                  <> short 'h'
                  <> metavar "HOSTNAME"
                  <> help "Name of host to configure")

buildPropellorOptions :: Parser Actions
buildPropellorOptions =
  BuildPropellor
  <$> strOption ( long "source-directory"
                  <> short 's'
                  <> value "."
                  <> metavar "DIRECTORY"
                  <> help "Directory to use for building configuration executable (default: .)")
  <*> strOption ( long "target-name"
                  <> short 't'
                  <> value "propell"
                  <> metavar "FILE"
                  <> help "Name of executable file built and extracted for configuration (default: propell)")
  <*> strOption ( long "image-name"
                  <> short 'i'
                  <> value "fpco/stack-build"
                  <> metavar "REPO"
                  <> help "Stack repository name for building configuration executable (default: fpco/stack-build)")
