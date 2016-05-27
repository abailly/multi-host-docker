{-# LANGUAGE RecordWildCards #-}
-- | Common utilities for networking, mostly types.
module Propellor.Network where

import           Data.Monoid ((<>))
-- | Name of interface or IP address, maybe 0.0.0.0 to bind to all available interfaces
type IPInterface = String

allInterfaces :: IPInterface
allInterfaces = "0.0.0.0"

localhost :: IPInterface
localhost = "127.0.0.1"

type Port = Int

data IPProto = UDP | TCP deriving (Show)

data Endpoint = Endpoint { ip   :: IPInterface
                         , port :: Port
                         }
              deriving (Eq, Show, Read)

hostport :: Endpoint -> String
hostport Endpoint{..} = ip <> ":" <> show port
