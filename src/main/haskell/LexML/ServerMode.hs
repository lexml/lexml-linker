{-# LANGUAGE DeriveDataTypeable #-}
module LexML.ServerMode where

import System.Console.CmdArgs
import Data.Data

data Server = Server {
      pidfile :: String
    , port :: Integer
    , syslog :: Bool
    , logfile :: String
    , cacheDir :: String
    , uncacheableDomainsFile :: String
  } deriving (Eq,Show,Read,Typeable,Data)

serverMode = Server { 
      pidfile = "" &= help "pid file" &= typFile
    , port = 8909 &= help "listening port" &= typ "INTEGER"
    , syslog = False &= help "send log to syslog"
    , logfile = "" &= help "send log to file"
    , cacheDir = "" &= help "cache directory" &= typ "PATH"
    , uncacheableDomainsFile = "" &= help "uncacheable domain list file" &= typFile
  }  

parseServerCommandLine = cmdArgs serverMode

