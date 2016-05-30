{-# LANGUAGE DeriveGeneric #-}
module Types(Actions(..)) where

import           Options.Generic

type HostName = String

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

