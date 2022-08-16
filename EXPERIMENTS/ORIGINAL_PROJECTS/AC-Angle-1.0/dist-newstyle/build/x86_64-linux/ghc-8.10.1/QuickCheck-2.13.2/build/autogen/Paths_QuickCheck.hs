{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_QuickCheck (
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
version = Version [2,13,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chollenbeck/.cabal/bin"
libdir     = "/home/chollenbeck/.cabal/lib/x86_64-linux-ghc-8.10.1/QuickCheck-2.13.2-inplace"
dynlibdir  = "/home/chollenbeck/.cabal/lib/ghc-8.10.1/QuickCheck-2.13.2-inplace"
datadir    = "/home/chollenbeck/.cabal/share/x86_64-linux-ghc-8.10.1/QuickCheck-2.13.2"
libexecdir = "/home/chollenbeck/.cabal/libexec/x86_64-linux-ghc-8.10.1/QuickCheck-2.13.2"
sysconfdir = "/home/chollenbeck/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "QuickCheck_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "QuickCheck_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "QuickCheck_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "QuickCheck_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "QuickCheck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "QuickCheck_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
