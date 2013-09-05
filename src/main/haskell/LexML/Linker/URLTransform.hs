module LexML.Linker.URLTransform ( rewriteAnchors ) where

import Data.Maybe
import Network.URI
import Text.HTML.TagSoup
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char

refAttrs = M.map S.fromList $ M.fromList $ 
  [ ("a",["href"]),
    ("img", ["src"]),
    ("script",["src"]),
    ("link",["href"]) ]

rewriteAnchors :: String -> [Tag String] -> [Tag String]
rewriteAnchors baseString = map rewriteAnchor 
  where
    base = fromJust $ parseURI baseString
    rewriteAnchor t@(TagOpen tagName attrs) = 
      case M.lookup (map toLower tagName) refAttrs of
        Just attrSet -> TagOpen tagName (map (rewriteAttr attrSet) attrs)
        Nothing -> t
    rewriteAnchor t = t
    rewriteAttr as a@(name,value) 
      | (map toLower name) `S.member` as = (name, absolutize value)
      | otherwise = a
    absolutize ref 
      | isRelativeReference ref = 
            show $ fromJust $ (fromJust $ parseRelativeReference ref) `relativeTo` base
      | otherwise = ref
