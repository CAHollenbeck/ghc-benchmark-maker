{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_primitive (
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
version = Version [0,7,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chollenbeck/.cabal/store/ghc-8.10.3/primitive-0.7.0.1-6300fae0755ff29bfe0cd7ed87dc5fa52a0f0fc7735c8a3710c8443f93719f3d/bin"
libdir     = "/home/chollenbeck/.cabal/store/ghc-8.10.3/primitive-0.7.0.1-6300fae0755ff29bfe0cd7ed87dc5fa52a0f0fc7735c8a3710c8443f93719f3d/lib"
dynlibdir  = "/home/chollenbeck/.cabal/store/ghc-8.10.3/primitive-0.7.0.1-6300fae0755ff29bfe0cd7ed87dc5fa52a0f0fc7735c8a3710c8443f93719f3d/lib"
datadir    = "/home/chollenbeck/.cabal/store/ghc-8.10.3/primitive-0.7.0.1-6300fae0755ff29bfe0cd7ed87dc5fa52a0f0fc7735c8a3710c8443f93719f3d/share"
libexecdir = "/home/chollenbeck/.cabal/store/ghc-8.10.3/primitive-0.7.0.1-6300fae0755ff29bfe0cd7ed87dc5fa52a0f0fc7735c8a3710c8443f93719f3d/libexec"
sysconfdir = "/home/chollenbeck/.cabal/store/ghc-8.10.3/primitive-0.7.0.1-6300fae0755ff29bfe0cd7ed87dc5fa52a0f0fc7735c8a3710c8443f93719f3d/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "primitive_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "primitive_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "primitive_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "primitive_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "primitive_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "primitive_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
