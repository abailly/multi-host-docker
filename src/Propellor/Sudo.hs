module Propellor.Sudo where

import           Propellor
import qualified Propellor.Property.Apt  as Apt
import           Propellor.Property.File

-- | Allows a user to sudo without password for some restricted binary
-- Put a file into /etc/sudoers.d/ directory instead of modifying /etc/sudoers and check
-- the latter contains include directives
binaryEnabledFor :: FilePath -> User -> Property DebianLike
binaryEnabledFor bin (User user) = prop `requires` Apt.installed ["sudo"]
  where
    prop = propertyList "sudoers includes /etc/sudoers.d directory" $ toProps
           [ containsLine "/etc/sudoers" "#includedir /etc/sudoers.d"
           , dirExists "/etc/sudoers.d"
           , containsLine ("/etc/sudoers.d/" ++ user)
             (user ++ " ALL= NOPASSWD: " ++ bin)
           ]
