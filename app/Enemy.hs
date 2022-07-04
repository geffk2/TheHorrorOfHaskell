module Enemy where

import Graphics.Gloss
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Extent
import Data.Graph
import Config ( (!!?), (!?), GameMap, Tile(Air), tileToInt, Tile(Floor) )
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BFS
import Data.Maybe ( fromMaybe, isJust )
import Control.Lens
import Data.Aeson.Encoding (list)

search :: (Eq a) => a -> [(a,b)] -> Maybe b
search _ [] = Nothing
search x ((a,b):xs) = if x == a then Just b else search x xs

edgeConvert :: [[LEdge Integer]] -> [LEdge Integer]
edgeConvert = concat

transformCoords :: (Coord, Tile) -> (Int, Integer)
transformCoords tup = (coef, fromIntegral coef)
  where
    coef = fst(fst tup)+(snd(fst tup)*49)

problemMap :: Extent -> QuadTree Tile -> [(Coord, Tile)]
problemMap = flattenQuadTree

makeLNode :: Extent -> QuadTree Tile -> [LNode Integer] -> Coord  -> [LNode Integer]
makeLNode ex tree list (x, y)
  | x == 49 && y == 49 = list
  | x == 49 = makeLNode ex tree list (0, y+1)
  | lookupByCoord ex (x, y) tree == Nothing = makeLNode ex tree (list ++ [(x+y*49, toInteger (x+y*49))]) (x+1, y) 
  | otherwise = makeLNode ex tree list (x+1, y)

-- (n, n-1, (m' n (n-1)))
--(n, n+1, (m' n (n+1)))
makeLEdge :: [[LEdge Integer]] -> [LNode Integer] -> [LNode Integer] -> [[LEdge Integer]]
makeLEdge list1 compList [] = list1
--makeLEdge _ [] (_:_)= []
makeLEdge list1 compList ((a, b):pairs) = newList : makeLEdge list1 compList pairs
  where
    list n = [
          (n, n-49, m'' n (n-49)),
          (n, n+49, m'' n (n+49))]
    fa = a
    newList
      | fa `mod` 49 == 48 = filter (\a -> isJust (search (a ^. _2) compList)) (filter (\a -> (a ^. _2) <= (49*49-1) && (a ^. _2) >= 0)
           (list fa ++ [(fa, fa-1, m' fa (fa-1))]))
      | fa `mod` 49 == 0 = filter (\a -> isJust (search (a ^. _2) compList)) (filter (\a -> (a ^. _2) <= (49*49-1) && (a ^. _2) >= 0)
           (list fa ++ [(fa, fa+1, m' fa (fa+1))]))
      | otherwise = filter (\a -> isJust (search (a ^. _2) compList)) (filter (\a -> (a ^. _2) <= (49*49-1) && (a ^. _2) >= 0)
           (list fa ++ [(fa, fa-1, m' fa (fa-1))] ++ [(fa, fa+1, m' fa (fa+1))]))
    m'' a b = toInteger(max a b*49-min a b*48)
    m' a b = toInteger (max a b)


graph :: Extent -> QuadTree Tile -> Gr Integer Integer
graph ex tree = mkGraph lnode (edgeConvert (makeLEdge [] lnode lnode))
  where
    lnode = makeLNode ex tree [] (0, 0)

bfs :: Extent -> QuadTree Tile -> Point -> Point -> [(Int, Int)]
bfs ex tree (bx,by) (hx,hy) = analyze (Data.Graph.Inductive.Query.BFS.esp (x''+(y''*49)) (x'+(y'*49)) (graph ex tree))
  where
    x'' = round bx
    y'' = round by
    x' = round hx
    y' = round hy
    analyze :: [Node] -> [(Int, Int)]
    analyze node = map (\a -> (a`mod`49, a`div`49)) node



  