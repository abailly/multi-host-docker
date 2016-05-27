module Propellor.FileMode where

import           System.Posix.Files
import           System.PosixCompat.Types

combineModes :: [FileMode] -> FileMode
combineModes [] = undefined
combineModes [m] = m
combineModes (m:ms) = foldl unionFileModes m ms

writeModes :: [FileMode]
writeModes = [ownerWriteMode, groupWriteMode, otherWriteMode]

readModes :: [FileMode]
readModes = [ownerReadMode, groupReadMode, otherReadMode]

executeModes :: [FileMode]
executeModes = [ownerExecuteMode, groupExecuteMode, otherExecuteMode]

ownerReadWriteMode :: FileMode
ownerReadWriteMode = combineModes [ownerReadMode,ownerWriteMode]
