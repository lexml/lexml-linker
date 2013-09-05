module LexML.Linker.Decorator where

import Text.HTML.TagSoup
import qualified Data.Map as M
import qualified Data.Set as S

type SplitPos = (Int,Int)

--splitTags :: SplitPos -> [(Int,Tag String)] -> ([Tag String],[(Int,Tag String)])
splitTags k@(x,0) l@((p,t):r)
    | x <= p     = ([],l)
    | otherwise  = let (a,b) = splitTags k r in (t : a, b)
splitTags k@(x,y) l@((p,TagText text):r)
    | x < p      = ([],l)
    | x == p     = if y > len then splitTags (nextX, y - len) nextTagText else ([TagText t1], tl)
    | otherwise  = let (a,b) = splitTags k r in (TagText text : a, b)
  where
    len = length text
    (t1,t2) = splitAt y text
    tl = if null t2 then r else (p,TagText t2) : r
    nextTagText = dropWhile notText r
    nextX = case nextTagText of
        ((p,_):_) -> p 
        [] -> x
splitTags k@(x,y) l@((p,t):r)
    | x <= p     = ([],l)
    | otherwise  = let (a,b) = splitTags k r in (t : a, b)
splitTags k [] = ([],[])

notText (_,TagText _) = False
notText _ = True
    

encloseTagText to tc = concatMap enclose
  where
    enclose t@(TagText _) = [to , t, tc]
    enclose t = [t]

type DecorationMap = M.Map Int (M.Map Int (Int,Int,String))

addURN t1 p1 t2 p2 urn = M.insertWith M.union t1 (M.singleton p1 (t2,p2,urn))

--addDecoration t1 p1 t2 p2 tagName attributes =
--  M.insertWith M.union t1 (M.singleton p1 (t2,p2,TagOpen tagName (M.toAscList attributes),TagClose tagName))

decorate :: (String -> (Tag String,Tag String)) -> DecorationMap -> [Tag String] -> [Tag String]
decorate makeTags dm tl = res
  where
    l = [ (x, y, k) | (x,q) <- M.toAscList dm, (y,k) <- M.toAscList q ]
    (_,revDecorated,_,rest) = foldl decorateSingle (0,[],0,(zip [0 ..] tl)) l
    res = concat (reverse (map snd rest : revDecorated)) 
    
    decorateSingle (lastX,rl,d,tl) (x1,y1,(x2,y2,u)) = (x2,encloseTagText to tc l2 : l1 : rl,y2,tl2)
      where
        (to,tc) = makeTags u 
        d' = if lastX == x1 then d else 0
        (l1,tl1) = splitTags (x1,y1 - d') tl
        y2' = if x1 == x2 then y2 - y1 else y2
        (l2,tl2) = splitTags (x2,y2') tl1

makeTagsAHRef resolverURL urn = (TagOpen "a" [("href",replace "" "URNLEXML" urn resolverURL), ("class","lexmlurnlink")],TagClose "a")

replace r _ _ [] = reverse r
replace r [] subst text = reverse r ++ subst ++ text
replace r pat@(p:ps) subs@(s:ss) (t:text) | p == t = 
    maybe (replace (t:r) pat subs text) id (replace' (s:r) ps ss text)
  where
    replace' r pat@(p:ps) subs@(s:ss) (t:text) | p == t = replace' (s:r) ps ss text
    replace' r [] subs text = Just $ reverse r ++ subs ++ text
    replace' _ _ _ _ = Nothing
replace r pat subs (t:text) = replace (t:r) pat subs text

makeTagsXLink urn = (TagOpen "span" [("xlink:href",urn)],TagClose "span")

