{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs 
import Data.Data
import Control.Monad
import LexML.URN.Types hiding (Data)
import LexML.URN.Parser
import LexML.Linker
import LexML.Linker(linker)
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
-- import qualified System.IO.UTF8 as IOUTF
import Control.Monad.Except
import Prelude hiding (log)
import LexML.Version
import qualified Data.ByteString as BIO
import Data.ByteString.UTF8 (toString, fromString)
import System.IO

main = do
  updateGlobalLogger rootLoggerName (setLevel ERROR)
  let contexto = RC_Nome "federal"
  let lo = defaultLinkerOptions {
            loContext = contexto
          , loResolverUrl = Nothing 
          , loOutputType = OT_XLINK
          , loInputType = IT_TAGGED
          , loDebugRegras = False
          , loDebugTokens = False        
          , loConstituicaoSimples = True
        }
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  let processLoop = do
        eof <- isEOF
        unless eof $ do
          line <- BIO.getLine
          let l = toString line
          res <- runExceptT $ linker lo l
          case res of
            Left err -> putStrLn $ "Error: " ++ show err
            Right out -> do
              let content = resContent out
              hSetEncoding stdout utf8
              putStrLn content
              hFlush stdout
          processLoop
  processLoop
