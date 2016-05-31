-- | Provide high-level functions to build Haskell-project using some docker image
module System.Build(stackInDocker) where

import           Data.Functor
import           System.Directory
import           System.Docker
import           System.IO
import           System.IO.Extra
import           System.Process

-- | Build a Haskell project using some docker image
stackInDocker :: ImageName -> FilePath -> String -> IO FilePath
stackInDocker img@(ImageName imgName) srcDir targetName = do
  absSrcDir <- canonicalizePath srcDir
  buildAlreadyRun <- doesFileExist ".cidfile"
  if buildAlreadyRun
    then do
    cid <- readFile ".cidfile"
    removeFile ".cidfile"
    callProcess "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build", "--volumes-from=" ++ cid,
                          "-v", "/root/.stack", "-w", "/build" , imgName, "stack", "build","--allow-different-user", ":" ++ targetName ]
    else callProcess "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build",
                               "-v", "/root/.stack", "-w", "/build" , imgName, "stack", "build","--allow-different-user", ":" ++ targetName ]

  exportBinary img targetName


exportBinary :: ImageName -> String -> IO FilePath
exportBinary (ImageName imgName) targetName = do
  cid <- readFile ".cidfile"
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--rm", "--volumes-from=" ++ cid,  "-w", "/build", imgName, "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--rm", "--volumes-from=" ++ cid, "busybox","dd", "if=" ++ stackRoot ++ "/bin/" ++ targetName ]) { std_out = CreatePipe }
  withBinaryFile targetName WriteMode $ \ hDst -> copy hout hDst
  void $ waitForProcess phdl
  return targetName
