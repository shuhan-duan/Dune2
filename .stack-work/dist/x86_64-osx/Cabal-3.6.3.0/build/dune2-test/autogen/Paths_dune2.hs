{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_dune2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shuhan/Sorbonne/S2/PAF/dune2/.stack-work/install/x86_64-osx/456e21daf7a449f0a32c69758262dbfffe2a468f863622ff95d3d03d53ff45ce/9.2.5/bin"
libdir     = "/Users/shuhan/Sorbonne/S2/PAF/dune2/.stack-work/install/x86_64-osx/456e21daf7a449f0a32c69758262dbfffe2a468f863622ff95d3d03d53ff45ce/9.2.5/lib/x86_64-osx-ghc-9.2.5/dune2-0.1.0.0-43ElVAEVo9Y2lI7HEep5wp-dune2-test"
dynlibdir  = "/Users/shuhan/Sorbonne/S2/PAF/dune2/.stack-work/install/x86_64-osx/456e21daf7a449f0a32c69758262dbfffe2a468f863622ff95d3d03d53ff45ce/9.2.5/lib/x86_64-osx-ghc-9.2.5"
datadir    = "/Users/shuhan/Sorbonne/S2/PAF/dune2/.stack-work/install/x86_64-osx/456e21daf7a449f0a32c69758262dbfffe2a468f863622ff95d3d03d53ff45ce/9.2.5/share/x86_64-osx-ghc-9.2.5/dune2-0.1.0.0"
libexecdir = "/Users/shuhan/Sorbonne/S2/PAF/dune2/.stack-work/install/x86_64-osx/456e21daf7a449f0a32c69758262dbfffe2a468f863622ff95d3d03d53ff45ce/9.2.5/libexec/x86_64-osx-ghc-9.2.5/dune2-0.1.0.0"
sysconfdir = "/Users/shuhan/Sorbonne/S2/PAF/dune2/.stack-work/install/x86_64-osx/456e21daf7a449f0a32c69758262dbfffe2a468f863622ff95d3d03d53ff45ce/9.2.5/etc"

getBinDir     = catchIO (getEnv "dune2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "dune2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "dune2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "dune2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dune2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dune2_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
