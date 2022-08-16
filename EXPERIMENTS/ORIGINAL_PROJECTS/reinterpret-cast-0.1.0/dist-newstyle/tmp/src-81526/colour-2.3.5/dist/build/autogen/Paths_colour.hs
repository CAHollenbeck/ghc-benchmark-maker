{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_colour (
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
version = Version [2,3,5] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chollenbeck/.cabal/store/ghc-8.10.3/colour-2.3.5-c56ce39b38faa6fe72c6319beba2c385749271fb3dfc9ea807b0bee4d5880bd3/bin"
libdir     = "/home/chollenbeck/.cabal/store/ghc-8.10.3/colour-2.3.5-c56ce39b38faa6fe72c6319beba2c385749271fb3dfc9ea807b0bee4d5880bd3/lib"
dynlibdir  = "/home/chollenbeck/.cabal/store/ghc-8.10.3/colour-2.3.5-c56ce39b38faa6fe72c6319beba2c385749271fb3dfc9ea807b0bee4d5880bd3/lib"
datadir    = "/home/chollenbeck/.cabal/store/ghc-8.10.3/colour-2.3.5-c56ce39b38faa6fe72c6319beba2c385749271fb3dfc9ea807b0bee4d5880bd3/share"
libexecdir = "/home/chollenbeck/.cabal/store/ghc-8.10.3/colour-2.3.5-c56ce39b38faa6fe72c6319beba2c385749271fb3dfc9ea807b0bee4d5880bd3/libexec"
sysconfdir = "/home/chollenbeck/.cabal/store/ghc-8.10.3/colour-2.3.5-c56ce39b38faa6fe72c6319beba2c385749271fb3dfc9ea807b0bee4d5880bd3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "colour_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "colour_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "colour_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "colour_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "colour_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "colour_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
