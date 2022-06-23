module Main where
import Graphics.Gloss
import Graphics.Gloss.Algorithms.RayCast
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Extent

import qualified Data.Set as S
import Data.Maybe

import ParseMap

raycast :: Point -> Point -> Extent -> QuadTree a -> Maybe (Point, Extent, a)
raycast = castSegIntoCellularQuadTree

raytrace :: Point -> Point -> Extent -> QuadTree a -> [(Point, Extent, a)]
raytrace = traceSegIntoCellularQuadTree

fov :: Float
fov = pi / 2

renderDistance :: Float
renderDistance = 10

windowSize :: (Int, Int)
windowSize = (800, 600)

textureResolution :: (Int, Int) 
textureResolution = (250, 250)

type Textures = [(Tile, BitmapData)]

loadTextures :: IO Textures 
loadTextures = do
  Bitmap wallBmp <- loadBMP "textures/wall.bmp"
  Bitmap floorBmp <- loadBMP "textures/floor.bmp"
  return [(Wall, wallBmp), (Floor, floorBmp)]
 
main :: IO ()
main = do
  textures <- loadTextures
  sampleMap <- parseMap "maps/map.json"
  let (ext, game) = constructMap sampleMap
  let st = initState{textures = textures, ext = ext, gameMap = game}
  play window black 30 st renderFrame handleEvents handleTime
  where
    window = InWindow "Boo" windowSize (700, 200)

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
  keysPressed :: S.Set Key,
  textures :: Textures 
}

initState :: State
initState = State ext m (8, 8) (1, 1) S.empty []
  where
    (ext, m) = constructMap []

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
renderFrame s@(State ext m pos dir _ textures) = walls
  where
    halfW = fst windowSize `div` 2
    screenW = i2f (fst windowSize)

    drawRay i = let
        vecDir = rotateV (i2f i * fov / screenW) dir
        end = pos `addVV` mulSV renderDistance vecDir
      in renderWall textures i pos (raycast pos end ext m)
    walls = pictures (map drawRay [-halfW .. halfW])


renderWall :: Textures -> Int -> Point -> Maybe (Point, Extent, Tile) -> Picture
renderWall _ _ _ Nothing = blank
renderWall tex i pos (Just (p, ext, t)) = res
  where
    screenW = i2f (fst windowSize)
    halfH = i2f (snd windowSize `div` 2)
    side = hitToSide p ext
    dist = magV (pos `addVV` mulSV (-1) p) * cos (i2f i * fov / screenW)
    col = wallColor t side
    x = i2f i
    y = halfH / dist

    scaleFactor = y / fromIntegral (snd textureResolution)
    res = case lookup t tex of
      Nothing  -> color col $ line [(x, y), (x, -y)]
      Just bmp -> scale 1 scaleFactor (translate x 0 (hitToTexture bmp p side))

wallColor :: Tile -> Side -> Color
wallColor Wall W = dark $ dark $ dark red
wallColor Wall E = dark $ dark $ dark red
wallColor Wall _ = dark $ dark red
wallColor _    _ = blue

data Side = N | S | W | E | Inside

hitToTexture :: BitmapData -> (Float, Float) -> Side -> Picture
hitToTexture bmp (x, y) side = modifier (BitmapSection textureRect bmp)
  where
    frac = max (snd $ properFraction x) (snd $ properFraction y)
    resX = fromIntegral (fst textureResolution) :: Float
    textureX = round (frac * resX)
    textureRect = Rectangle (textureX, 0) (1, snd textureResolution)
    
    halfH = i2f (snd windowSize `div` 2)

    darkRect = color (makeColor 0 0 0 0.25) (line [(0, -halfH), (0, halfH)])
    darken p = pictures [p,darkRect]
    modifier = case side of S -> darken
                            N -> darken
                            _ -> id
                        
hitToSide :: (Float, Float) -> Extent -> Side
hitToSide (x, y) ext
  | y == i2f n = N
  | y == i2f s = S
  | x == i2f e = E
  | x == i2f w = W
  | otherwise  = Inside
  where
    (n, s, e, w) = takeExtent ext

fractionalOfV :: Vector -> Vector
fractionalOfV (x, y) = (snd (properFraction x), snd (properFraction y))

i2f :: Int -> Float
i2f = fromIntegral
