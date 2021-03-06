module Paths_Akkerman_func (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/anastasia/Haskell/Akkerman-func/.cabal-sandbox/bin"
libdir     = "/home/anastasia/Haskell/Akkerman-func/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/Akkerman-func-0.1.0.0"
datadir    = "/home/anastasia/Haskell/Akkerman-func/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/Akkerman-func-0.1.0.0"
libexecdir = "/home/anastasia/Haskell/Akkerman-func/.cabal-sandbox/libexec"
sysconfdir = "/home/anastasia/Haskell/Akkerman-func/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Akkerman_func_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Akkerman_func_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Akkerman_func_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Akkerman_func_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Akkerman_func_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
