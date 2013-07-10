module Paths_compilation (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\bin"
libdir     = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\compilation-0.0.0.1\\ghc-7.4.2"
datadir    = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\compilation-0.0.0.1"
libexecdir = "C:\\Documents and Settings\\Administrator\\Application Data\\cabal\\compilation-0.0.0.1"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "compilation_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "compilation_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "compilation_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "compilation_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)