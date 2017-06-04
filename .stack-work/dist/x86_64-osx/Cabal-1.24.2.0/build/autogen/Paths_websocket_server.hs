{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_websocket_server (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/reo/hsproject/BlockChain/websocket-server/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/bin"
libdir     = "/Users/reo/hsproject/BlockChain/websocket-server/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/lib/x86_64-osx-ghc-8.0.2/websocket-server-0.1.0.0-I7u4whFyaF5DcxUIKC3QwR"
dynlibdir  = "/Users/reo/hsproject/BlockChain/websocket-server/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/reo/hsproject/BlockChain/websocket-server/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/share/x86_64-osx-ghc-8.0.2/websocket-server-0.1.0.0"
libexecdir = "/Users/reo/hsproject/BlockChain/websocket-server/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/libexec"
sysconfdir = "/Users/reo/hsproject/BlockChain/websocket-server/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "websocket_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "websocket_server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "websocket_server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "websocket_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "websocket_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "websocket_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
