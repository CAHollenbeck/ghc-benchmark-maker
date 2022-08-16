{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_quickcheck_io (
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
version = Version [0,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chollenbeck/.cabal/bin"
libdir     = "/home/chollenbeck/.cabal/lib/x86_64-linux-ghc-8.10.1/quickcheck-io-0.2.0-inplace"
dynlibdir  = "/home/chollenbeck/.cabal/lib/ghc-8.10.1/quickcheck-io-0.2.0-inplace"
datadir    = "/home/chollenbeck/.cabal/share/x86_64-linux-ghc-8.10.1/quickcheck-io-0.2.0"
libexecdir = "/home/chollenbeck/.cabal/libexec/x86_64-linux-ghc-8.10.1/quickcheck-io-0.2.0"
sysconfdir = "/home/chollenbeck/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quickcheck_io_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quickcheck_io_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quickcheck_io_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quickcheck_io_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quickcheck_io_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quickcheck_io_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
