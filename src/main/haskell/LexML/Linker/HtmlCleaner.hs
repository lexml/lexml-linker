module LexML.Linker.HtmlCleaner ( cleanEmptyAnchors ) where

import Data.Maybe
import Network.URI
import Text.HTML.TagSoup
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char


cleanEmptyAnchors :: [Tag String] -> [Tag String]
cleanEmptyAnchors [] = []
cleanEmptyAnchors ((TagOpen "a" _):(TagClose "a"):r) = cleanEmptyAnchors r
cleanEmptyAnchors ((TagOpen "a" _):(TagText tx):(TagClose "a"):r) | onlySpace tx = cleanEmptyAnchors r
  where onlySpace = and . map isSpace
cleanEmptyAnchors (t:r) = t : cleanEmptyAnchors r
