module LexML.Render where

import Text.HTML.TagSoup 
import Text.HTML.TagSoup.Entity 
import Control.Monad.State
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Char

type RenderState = Int

initialState :: RenderState
initialState = 0

type RenderM a = StateT RenderState (Writer String) a

notCollapsable = S.fromList [
  "div","span", "script"]

protectedTags = S.fromList [
  "script" ]

openProtected :: RenderM ()
openProtected = modify (+1)

closeProtected :: RenderM ()
closeProtected = modify (\f -> if f > 0 then f - 1 else f)

ifProtected :: a -> a -> RenderM a
ifProtected prot unprot = do
  v <- get
  return $  if v > 0 then prot else unprot

escMap = IM.fromList [(b, "&"++a++";") | (a,b) <- htmlEntities]
escape = concatMap (\x -> IM.findWithDefault [x] (ord x) escMap)

render :: [Tag String] -> String
render = execWriter . flip evalStateT initialState . tags
  where
    tags (TagOpen name atts:TagClose name2:xs)
              | name == name && (not ((map toLower name) `S.member` notCollapsable)) = open name atts " /" >> tags xs
    tags (x:xs) = tag x >> tags xs
    tags []     = return ()

    tag (TagOpen name atts) = do
      open name atts ""
      if map toLower name `S.member` protectedTags then openProtected else return()
    tag (TagClose name) = do
      tell $ "</" ++ name ++ ">"
      if map toLower name `S.member` protectedTags then closeProtected else return()
    tag (TagText text) = tellText text
    tag (TagComment text) = tell "<!--" >> tellComment text >> tell "-->"
    tag _ = return ()

    tellText t = ifProtected t (escape t) >>= tell
    open name atts shut = tell $ "<" ++ name ++ concatMap att atts ++ shut ++ ">"
    
    att (x,"") = " " ++ x
    att ("",y) = " " ++ "\"" ++ escape y ++ "\""
    att (x,y) = " " ++ x ++ "=\"" ++ escape y ++ "\""

    tellComment = tell . com
    
    com ('-':'-':'>':xs) = "-- >" ++ com xs
    com (x:xs) = x : com xs
    com [] = []

