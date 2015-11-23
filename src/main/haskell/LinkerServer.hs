module Main where

import Data.Time
import System.CPUTime
import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import qualified Data.ByteString as BIO
import Data.Char
import LexML.URN.Types
import Data.ByteString.UTF8 (toString, fromString)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import LexML.Linker
import qualified Data.Map as M
import LexML.URN.Parser
import qualified LexML.ServerMode as SM
import System.Console.CmdArgs
import System.Posix.Process
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Prelude hiding (log)
import LexML.Version
import Data.Digest.OpenSSL.MD5
import LexML.Cache
import Network.URI
import qualified Data.Set as S

log = debugM "LexML.Server"

notice = noticeM "LexML.Server"

data Options = Options {
    opURNDocumento :: URNLexML,
    opURLContexto :: Maybe String,
    opTokenLimit :: Maybe Int,
    opURLResolver :: Maybe String
  } deriving (Show)

defOptions = Options { opURNDocumento = contextoFederal, opURLContexto = Nothing, opTokenLimit = Nothing, opURLResolver = Nothing }

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

processOptions l = foldM processOption defOptions [ (trim on, trim ov) | (on,_:ov) <- map (break (== ':')) l ]
  where
    split x = case span (/= '.') x of
                (y,"") -> [y]
                (y,_:r) -> y : split r
    autoridadesFederais = S.fromList [
                            A_Normal [SJ_Cargo (Cargo (Nome (split nome)))] |
                                    nome <- nomesAutoridadesFederais]
    nomesAutoridadesFederais = [
                    "advocacia.geral.uniao"
                , "controladoria.geral.uniado"
                , "ministerio.planejamento.gestao"
        ]
    filterDocumento u@(URNLexML _ (Documento aut@(A_Normal _) _ _) _ _ _) 
        | aut `S.member` autoridadesFederais = contextoFederal
    filterDocumento u = u
    processOption opt ("urndocumento", urnDocumento) = parseURNLexML urnDocumento >>= \ (Just u) -> return $ opt { opURNDocumento = filterDocumento u }
    processOption opt ("urlcontexto",contexto) = return $ opt { opURLContexto = Just contexto }
    processOption opt ("limiteprocessamento",limitStr) = return $ opt { opTokenLimit = Just $ read limitStr }
    processOption opt ("urlresolver",urlresolver) = return $ opt { opURLResolver = Just urlresolver } 
    processOption opt _ = return opt

type HandlerFunc = Options -> String -> IO (String, M.Map String String)

serve :: String              -- ^ Port number or name; 514 is default
         -> CacheFunc (M.Map String String)
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serve port cacheFunc handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       
       setSocketOption sock ReuseAddr 1

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)


       -- Start listening for connection requests.  Maximum queue size
       -- of 5 connection requests waiting to be accepted.
       listen sock 5

       -- Create a lock to use for synchronizing access to the handler
       lock <- newMVar ()

       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- accept mastersock
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl (BlockBuffering (Just 1024))
                 procDocument lock connhdl

          procDocument :: MVar () -> Handle -> IO ()
          procDocument lock connhdl = 
             do  startTime <- getCurrentTime
                 notice $ "procDocument: processing request at " ++ show startTime
                 log $ "Reading options"
                 optionLines <- readOptionLines connhdl
                 log $ "optionLines : " ++ show optionLines
                 options <- processOptions optionLines
                 log $ "options: " ++ show options
                 (contentsBS,close) <- readChunks connhdl
                 let generateFunc cbs = do
                       let intext = toString cbs 
                       (s, resHeader) <- handle lock options intext
                       return (fromString s, M.insert "cached" "False" resHeader)
                     defHeader = M.fromList [("status","ok"),("version",version),("cached","True")] 
                 (outBS, resHeader) <- cacheFunc (extractDomain (opURLContexto options)) defHeader contentsBS generateFunc
                 log $ "Writing result: " ++ (toString outBS)
                 log $ "Writing header: " ++ show resHeader 
                 writeHeader connhdl resHeader
                 hPutStrLn connhdl $ show $ BIO.length outBS
                 BIO.hPut connhdl outBS
                 log $ "Flushing"
                 hFlush connhdl
                 endTime <- getCurrentTime
                 notice $ "procDocument: finished request at " ++ show endTime ++ " (" ++ show (diffUTCTime endTime startTime) ++ ")"
                 if close then (do
                     log $ "Closing"
                     hClose connhdl) else procDocument lock connhdl

          -- Lock the handler before passing data to it.
          handle :: MVar () -> HandlerFunc
          -- This type is the same as
          -- handle :: MVar () -> Options -> String -> IO ()
          handle lock options msg =
              withMVar lock 
                 (\a -> handlerfunc options msg)
          readChunks hdl = do
            let readChunks' = do
                  log $ "reading chunk size or close request"
                  chunkSizeOrCloseStr <- hGetLine hdl
                  log $ "chunk size or close = " ++ show chunkSizeOrCloseStr
                  case chunkSizeOrCloseStr of
                    "close" -> return (BIO.empty, True)
                    _ -> do let chunkSize = read chunkSizeOrCloseStr
                            if chunkSize > 0 then (do
                              log $ "chunk size = " ++ show chunkSize ++ ", reading chunk" 
                              chunk <- BIO.hGet hdl chunkSize
                              (res,close) <- readChunks' 
                              log $ "chunk read"
                              return $ (chunk `BIO.append` res,close))
                                else ( (log $ "no more chunks") >> return (BIO.empty,False) )                        
            res <- readChunks'  
            return res

extractDomain :: Maybe String -> String
extractDomain Nothing = ""
extractDomain (Just s) = case parseURI s of
    Nothing -> ""
    Just uri -> case uriAuthority uri of 
      Nothing -> ""
      Just aut -> uriRegName aut
                  

readOptionLines h = do
  l <- hGetLine h
  case l of
    "" -> return []
    _  -> readOptionLines h >>= return . (l :)

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler options msg = do
    log $ "Processing (" ++ show options ++ "): " ++ msg
    return (map toLower msg, M.empty)

linkerHandler :: HandlerFunc
linkerHandler options txt = do
  let lo = defaultLinkerOptions {
              loBaseUrl = opURLContexto options,
              loContext = RC_URNLexML (opURNDocumento options),
--              loTokenLimit = opTokenLimit options,
              loResolverUrl = opURLResolver options
           }
  res <- runExceptT (linker lo txt)
  case res of
    Left err -> return (txt, M.singleton "erro" (show err))
    Right out -> 
      let content = resContent out
          partial = resPartial out
          headers = M.fromList [
                ("status", if partial then "parcial" else "ok")
              , ("version",version)
            ]
             in return (content,headers)

writeHeader :: Handle -> M.Map String String -> IO ()
writeHeader handle m = do
   flip mapM_ (M.toAscList m) (\ (k, v) -> hPutStrLn handle $ k ++ ":" ++ v) 
   hPutStrLn handle ""

main = traplogging "LexML.Errors" ERROR "" $  do
  serverOpts <- SM.parseServerCommandLine 
  normal <- isNormal
  when normal $ updateGlobalLogger rootLoggerName (setLevel INFO)
  loud <- isLoud
  when loud $ updateGlobalLogger rootLoggerName (setLevel DEBUG)
  noticeM "LexML.Server" $ "starting LinkerServer version " ++ version
  log $ "options : " ++ show serverOpts
  baseHandlers <- if not (null (SM.logfile serverOpts)) then return [] 
                      else verboseStreamHandler stderr DEBUG >>= \l -> return [l]
  updateGlobalLogger rootLoggerName $ setHandlers baseHandlers
  log $ "handlers updated"
  when (SM.syslog serverOpts) $ do
    s <- openlog "LinkerServer" [PID] DAEMON DEBUG
    updateGlobalLogger rootLoggerName (addHandler s)
    noticeM "LexML.Server" $ "starting LinkerServer log on syslog"
  case SM.logfile serverOpts of
    "" -> return ()
    lf -> do lh <- openFile lf WriteMode
             h <- verboseStreamHandler lh DEBUG
             updateGlobalLogger rootLoggerName $ addHandler h
  case SM.pidfile serverOpts of
    "" -> return ()
    pf -> do pid <- getProcessID
             writeFile pf (show pid)
  log "LinkerServer starting"
  udomains <- case SM.uncacheableDomainsFile serverOpts of
                "" -> return []
                fp -> fmap lines $ readFile fp
  cacheFunc <- case SM.cacheDir serverOpts of
    "" -> noticeM "LexML.Server" "Not using cache" >> return noCache
    cdir -> noticeM "LexML.Server" ("Using cache at " ++ cdir) >> initCache cdir udomains >>= return . cached
  serve (show $ SM.port serverOpts) cacheFunc linkerHandler


