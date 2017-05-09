{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_akrantiain2 (
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
version = Version [0,6,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/admin/Library/Haskell/bin"
libdir     = "/Users/admin/Library/Haskell/ghc-8.0.2-x86_64/lib/akrantiain2-0.6.2"
dynlibdir  = "/Users/admin/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/admin/Library/Haskell/share/ghc-8.0.2-x86_64/akrantiain2-0.6.2"
libexecdir = "/Users/admin/Library/Haskell/libexec"
sysconfdir = "/Users/admin/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "akrantiain2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "akrantiain2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "akrantiain2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "akrantiain2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "akrantiain2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "akrantiain2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
