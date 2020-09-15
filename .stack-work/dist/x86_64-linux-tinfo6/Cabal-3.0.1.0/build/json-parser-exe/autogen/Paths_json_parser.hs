{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_json_parser (
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

bindir     = "/home/biel/fga/pp/2020.1_G4_Funcional_AnalisadorJson/.stack-work/install/x86_64-linux-tinfo6/7d8497f5182d0bfb7aa0999d58917696f438968e634c67851422283769d9e589/8.8.4/bin"
libdir     = "/home/biel/fga/pp/2020.1_G4_Funcional_AnalisadorJson/.stack-work/install/x86_64-linux-tinfo6/7d8497f5182d0bfb7aa0999d58917696f438968e634c67851422283769d9e589/8.8.4/lib/x86_64-linux-ghc-8.8.4/json-parser-0.1.0.0-AlQGP8smMdk7PjHIknuFe7-json-parser-exe"
dynlibdir  = "/home/biel/fga/pp/2020.1_G4_Funcional_AnalisadorJson/.stack-work/install/x86_64-linux-tinfo6/7d8497f5182d0bfb7aa0999d58917696f438968e634c67851422283769d9e589/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/biel/fga/pp/2020.1_G4_Funcional_AnalisadorJson/.stack-work/install/x86_64-linux-tinfo6/7d8497f5182d0bfb7aa0999d58917696f438968e634c67851422283769d9e589/8.8.4/share/x86_64-linux-ghc-8.8.4/json-parser-0.1.0.0"
libexecdir = "/home/biel/fga/pp/2020.1_G4_Funcional_AnalisadorJson/.stack-work/install/x86_64-linux-tinfo6/7d8497f5182d0bfb7aa0999d58917696f438968e634c67851422283769d9e589/8.8.4/libexec/x86_64-linux-ghc-8.8.4/json-parser-0.1.0.0"
sysconfdir = "/home/biel/fga/pp/2020.1_G4_Funcional_AnalisadorJson/.stack-work/install/x86_64-linux-tinfo6/7d8497f5182d0bfb7aa0999d58917696f438968e634c67851422283769d9e589/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "json_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "json_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "json_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "json_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "json_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "json_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
