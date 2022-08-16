{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_AC_Angle (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chollenbeck/.cabal/bin"
libdir     = "/home/chollenbeck/.cabal/lib/x86_64-linux-ghc-8.10.1/AC-Angle-1.0-inplace"
dynlibdir  = "/home/chollenbeck/.cabal/lib/ghc-8.10.1/AC-Angle-1.0-inplace"
datadir    = "/home/chollenbeck/.cabal/share/x86_64-linux-ghc-8.10.1/AC-Angle-1.0"
libexecdir = "/home/chollenbeck/.cabal/libexec/x86_64-linux-ghc-8.10.1/AC-Angle-1.0"
sysconfdir = "/home/chollenbeck/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AC_Angle_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AC_Angle_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AC_Angle_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AC_Angle_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AC_Angle_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AC_Angle_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
