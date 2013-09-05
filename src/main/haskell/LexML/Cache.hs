module LexML.Cache where

import Data.Binary
import LexML.Version (version)
import System.Directory
import System.FilePath
import Data.List
import qualified Data.ByteString as B
import Data.Digest.OpenSSL.MD5

type Domain = String

data CacheContext = CC {
      ccVersion :: String,
      ccCacheRoot :: FilePath,
      ccBaseDir :: FilePath,
      ccUncacheableDomains :: [Domain]
  } deriving (Show)

initCache :: FilePath -> [Domain] -> IO CacheContext
initCache root udomains = do
  createDirectoryIfMissing True root
  files <- getDirectoryContents root
  let outdated = [ f | f <- files, f /= ".", f /= "..", f /= version ]
      baseDir = root </> version
  putStrLn $ "Outdated files: " ++ show outdated
  return $ CC { ccVersion = version, ccCacheRoot = root, ccBaseDir = baseDir, ccUncacheableDomains = udomains }

breakPath :: FilePath -> [FilePath]
breakPath = unfoldr f
  where
    f (x:y:z) = Just ([x,y],z) 
    f (x:z)   = Just ([x],z) 
    f []      = Nothing

type CacheFunc a = Domain -> a -> B.ByteString -> (B.ByteString -> IO (B.ByteString,a)) -> IO (B.ByteString,a)

cached :: CacheContext -> CacheFunc a
cached context dom defAux input action = do
    putStrLn $ "dom = " ++ dom ++ ", uncacheable = " ++ show (ccUncacheableDomains context)
    if any (`isSuffixOf` dom) (ccUncacheableDomains context) then action input else do 
      let md5 = md5sum input
          (fname:pathcs) = breakPath md5
          dirpath = foldr1 (</>) (ccBaseDir context : pathcs)
          path = dirpath </> fname
      case pathcs of [] -> return ()
                     _  -> createDirectoryIfMissing True dirpath
      e <- doesFileExist path
      if e then readCached path else generateAndCache path
  where
    readCached path = do
      putStrLn $ "Reading from cache: " ++ path
      res <- B.readFile path
      return (res,defAux)
    generateAndCache path = do
      putStrLn $ "Generating to cache: " ++ path
      (res,aux) <- action input
      B.writeFile path res
      return (res,aux)          

noCache :: CacheFunc a
noCache _ _ input action = action input

