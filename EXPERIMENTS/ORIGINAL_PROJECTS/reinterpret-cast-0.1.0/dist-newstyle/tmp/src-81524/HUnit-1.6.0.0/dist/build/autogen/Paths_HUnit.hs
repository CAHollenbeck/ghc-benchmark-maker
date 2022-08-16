{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HUnit (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,6,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chollenbeck/.cabal/store/ghc-8.10.3/HUnit-1.6.0.0-b0a7e51f2ed69e89ce3f5c6dd94777db3504afd98d1998dc348e0ab35cf9795b/bin"
libdir     = "/home/chollenbeck/.cabal/store/ghc-8.10.3/HUnit-1.6.0.0-b0a7e51f2ed69e89ce3f5c6dd94777db3504afd98d1998dc348e0ab35cf9795b/lib"
dynlibdir  = "/home/chollenbeck/.cabal/store/ghc-8.10.3/HUnit-1.6.0.0-b0a7e51f2ed69e89ce3f5c6dd94777db3504afd98d1998dc348e0ab35cf9795b/lib"
datadir    = "/home/chollenbeck/.cabal/store/ghc-8.10.3/HUnit-1.6.0.0-b0a7e51f2ed69e89ce3f5c6dd94777db3504afd98d1998dc348e0ab35cf9795b/share"
libexecdir = "/home/chollenbeck/.cabal/store/ghc-8.10.3/HUnit-1.6.0.0-b0a7e51f2ed69e89ce3f5c6dd94777db3504afd98d1998dc348e0ab35cf9795b/libexec"
sysconfdir = "/home/chollenbeck/.cabal/store/ghc-8.10.3/HUnit-1.6.0.0-b0a7e51f2ed69e89ce3f5c6dd94777db3504afd98d1998dc348e0ab35cf9795b/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HUnit_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HUnit_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HUnit_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HUnit_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HUnit_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HUnit_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
