module Main where
import Graphics.Gloss
import Graphics.Gloss.Algorithms.RayCast
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Extent

import qualified Data.Set as S
import Data.Maybe

raycast :: Point -> Point -> Extent -> QuadTree a -> Maybe (Point, Extent, a)
raycast = castSegIntoCellularQuadTree

raytrace :: Point -> Point -> Extent -> QuadTree a -> [(Point, Extent, a)]
raytrace = traceSegIntoCellularQuadTree

fov :: Float
fov = pi / 2.5

renderDistance :: Float
renderDistance = 10

windowSize :: (Int, Int)
windowSize = (500, 500)

main :: IO ()
main = play window black 30 initState renderFrame handleEvents handleTime
  where
    window = InWindow "hehe" windowSize (700, 200)
    pic = renderFrame initState

handleEvents :: Event -> State -> State
handleEvents (EventKey k Down _ _) s = s{keysPressed = S.insert k (keysPressed s)}
handleEvents (EventKey k Up _ _) s   = s{keysPressed = S.delete k (keysPressed s)}
handleEvents _ s = s

handleTime :: Float -> State -> State
handleTime dt s = s{playerDir = newDir, playerPos = nextPos}
  where
    keyToDir k dir = if S.member k (keysPressed s) then dir else (0, 0)
    speed = keyToDir (Char 'w') (playerDir s)
            `addVV` keyToDir (Char 's') (mulSV (-1) (playerDir s))
    
    nextPos = let
        newPos = playerPos s `addVV` mulSV (2*dt) speed
        i2fV (x, y) = (floor x, floor y)
        isPosTaken p = isJust (lookupByCoord (ext s) p (gameMap s))
      in if isPosTaken (i2fV newPos) then newPos `addVV` mulSV (-2*dt) speed else newPos

    newDir
      | S.member (Char 'd') (keysPressed s) = rotateV (dt*pi/2) (playerDir s)
      | S.member (Char 'a') (keysPressed s) = rotateV (-dt*pi/2) (playerDir s)
      | otherwise = playerDir s

addVV :: Vector -> Vector -> Vector
addVV (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

data State = State {
  ext :: Extent,
  gameMap :: QuadTree Tile,
  playerPos :: Point,
  playerDir :: Vector,
  keysPressed :: S.Set Key
}

initState :: State
initState = State ext m (1, 1) (1, 1) S.empty
  where
    (ext, m) = constructMap sampleMap

type GameMap = [[Tile]]

sampleMap :: GameMap
sampleMap = [
    [w, w, w, w]
  , [w, a, a, w]
  , [w, a, a, w]
  , [w, a, a, w]
  , [w, a, a, w]
  , [w, a, w, w]
  , [w, w, w, w]
  ]
  where
    (a, w) = (Air, Wall)

data Tile = Wall | Air
  deriving (Show, Eq)

constructMap :: GameMap -> (Extent, QuadTree Tile)
constructMap m = (ext, foldr insTile emptyTree tiles)
  where
    ext = makeExtent (maximum (map length m)) 0 (length m) 0
    tiles = filter (isJust . fst) [(m !!? (i, j), (i, j))
                                  | i <- [0..(length m)]
                                  , j <- [0..(maybe 0 length (m !? i))]]

    insTile :: (Maybe Tile, Coord) -> QuadTree Tile -> QuadTree Tile
    insTile (Just Air, c) t = t
    insTile (Just tile, c) t = fromMaybe t (insertByCoord ext c tile t)
    insTile (Nothing, _)  t  = t


-- | A modified, safe version of !!
(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_)  !? 0 = Just x
(_:xs) !? i = xs !? (i-1)
infixl 9 !?

(!!?) :: [[a]] -> (Int, Int) -> Maybe a
l !!? (i, j) = (!? j) =<< (l !? i)


renderFrame :: State -> Picture
renderFrame (State ext m pos dir _) = pictures (map drawRay [-halfW .. halfW])
  where
    halfW = fst windowSize `div` 2
    screenW = i2f (fst windowSize)

    drawRay i = let
        vecDir = rotateV (i2f i * fov / screenW) dir
        end = pos `addVV` mulSV renderDistance vecDir
      in renderWall i pos (raycast pos end ext m)


renderWall :: Int -> Point -> Maybe (Point, Extent, Tile) -> Picture
renderWall _ _ Nothing = blank
renderWall i pos (Just (p, ext, t)) = color col $ line [(x, y), (x, -y)]
  where
    halfH = i2f (snd windowSize `div` 2)
    dist = magV (pos `addVV` mulSV (-1) p)
    side = hitToSide p ext
    col = wallColor t side
    x = i2f i
    y = halfH / dist

wallColor :: Tile -> Side -> Color
wallColor Wall W = dark $ dark $ dark red
wallColor Wall E = dark $ dark $ dark red
wallColor Wall _ = dark $ dark red
wallColor _    _ = blue

data Side = N | S | W | E | Inside

hitToSide :: (Float, Float) -> Extent -> Side
hitToSide (x, y) ext
  | y == i2f n = N
  | y == i2f s = S
  | x == i2f e = E
  | x == i2f w = W
  | otherwise  = Inside
  where
    (n, s, e, w) = takeExtent ext

i2f :: Int -> Float
i2f = fromIntegral