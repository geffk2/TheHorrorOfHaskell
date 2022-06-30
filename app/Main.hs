{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Main where
import Graphics.Gloss
import Graphics.Gloss.Algorithms.RayCast
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Extent

import qualified Data.Set as S
import Data.Maybe ( fromMaybe, isJust )
import Data.List
import Data.Matrix
import Data.Complex

import ParseMap (parseMap)
import MusicPlayer
    ( playSound, Sound(Walking, Silence, Doorbell, StopWalking), Sounds, loadSounds, initMusicPlayer )
import SDL.Mixer (Chunk)
import qualified SDL.Mixer as Mix
import Config

-- | Aliases for functions with extra long names
raycast :: Point -> Point -> Extent -> QuadTree a -> Maybe (Point, Extent, a)
raycast = castSegIntoCellularQuadTree

raytrace :: Point -> Point -> Extent -> QuadTree a -> [(Point, Extent, a)]
raytrace = traceSegIntoCellularQuadTree
main :: IO ()

main = do
  initMusicPlayer

  textures <- loadTextures
  sampleMap <- parseMap "maps/map.json"
  sounds <- loadSounds

  let sprites = [Sprite (6, 6) Barrel]

  let (ext, game) = constructMap sampleMap

  let st = State ext game (8, 8) (1, 1) S.empty textures sounds 1 sprites 
  playIO window black fps st renderFrame handleEvents handleTime
  where
    window = InWindow "Boo" windowSize (700, 200)



-- | All about the state
data State = State {
  ext :: Extent,
  gameMap :: QuadTree Tile,
  playerPos :: Point,
  playerDir :: Vector,
  keysPressed :: S.Set Key,
  textures :: Textures,
  sounds :: Sound -> Mix.Chunk,
  timePassed :: Float,
  sprites :: [Sprite]
}

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


-- | Handlers
handleEvents :: Event -> State -> IO State
handleEvents (EventKey k Down _ _) s = return s{keysPressed = S.insert k (keysPressed s)}
handleEvents (EventKey k Up _ _) s   = return s{keysPressed = S.delete k (keysPressed s)}
handleEvents _ s = return s


handleTime :: Float -> State -> IO State
handleTime dt s = do
  playSound sound (sounds s)
  return s{playerDir = newDir, playerPos = nextPos, timePassed = newTime, gameMap = newMap}
  where
    keyToDir k dir = if S.member k (keysPressed s) then dir else (0, 0)
    speed = keyToDir (Char 'w') (playerDir s)
            `addVV` keyToDir (Char 's') (mulSV (-1) (playerDir s))
    nextPos = let
        newPos = playerPos s `addVV` mulSV (2*dt) speed
        i2fV (x, y) = (floor x, floor y)
        isPosTaken p = isJust (lookupByCoord (ext s) p (gameMap s))
      in if isPosTaken (i2fV newPos) then newPos `addVV` mulSV (-2*dt) speed else newPos
    newTime = timePassed s + dt

    newDir
      | S.member (Char 'd') (keysPressed s) = rotateV (dt*pi/2) (playerDir s)
      | S.member (Char 'a') (keysPressed s) = rotateV (-dt*pi/2) (playerDir s)
      | otherwise = playerDir s
    
    newMap
      | S.member (Char 'e') (keysPressed s) = tryOpenDoors s
      | otherwise = gameMap s

    sound
      | floor newTime `mod` 50 == 0 = Doorbell
      | otherwise                   = Silence


tryOpenDoors :: State -> QuadTree Tile
tryOpenDoors s@(State ext m _ (px,py) _ _ _ _ _) = fmap f m
  where
    doors = checkForDoors ext m (floor px, floor py) 
    f (Door c) = if c `elem` doors 
                    then Air
                    else Door c
    f a = a

checkForDoors :: Extent -> QuadTree Tile -> Coord -> [DoorColor]
checkForDoors ext m (x, y) = concatMap checkPoint points 
  where
    points = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    checkCell (Just (Door c)) = [c] 
    checkCell _ = []

    checkPoint pos = checkCell (lookupByCoord ext pos m)


-- | Rendering
renderFrame :: State -> IO Picture
renderFrame s@(State ext m pos dir keys textures sounds _ _) = do
  playSound sound sounds
  return (floorAndCeiling <> walls <> sprites)
  where
    halfW = fst windowSize `div` 2
    screenW = i2f (fst windowSize)

    drawRay i = let
        vecDir = rotateV (i2f i * fov / screenW) dir
        end = pos `addVV` mulSV renderDistance vecDir
      in renderWall textures i pos dir (raycast pos end ext m)

    rayResults = map drawRay [-halfW .. halfW]
    walls = pictures $ map fst rayResults
    zBuf = map snd rayResults
    floorAndCeiling = renderFloorAndCeiling
    sprites = renderSprites s zBuf

    sound
      | S.member (Char 'w') keys = Walking
      | S.member (Char 's') keys = Walking
      | otherwise                = StopWalking


renderWall :: Textures -> Int -> Point -> Vector -> Maybe (Point, Extent, Tile) -> (Picture, Float)
renderWall _ _ _ _ Nothing = (blank, 1)
renderWall tex i pos dir (Just (p, ext, t)) = (res, dist)
  where
    screenW = i2f (fst windowSize)
    halfH = i2f (snd windowSize `div` 2)
    side = hitToSide p ext
    dist = magV (pos `addVV` mulSV (-1) p) * cos (i2f i * fov / screenW)
    col = wallColor t side
    x = i2f i
    y = halfH / dist
   
    scaleFactor = 2 * y / fromIntegral (snd textureResolution)
    distDarkCoef = dist / renderDistance
    res = let 
            Bitmap bmp = tex (Left t) 
           in scale 1 scaleFactor
              $ translate x 0
              $ darkenImg (min 1 distDarkCoef)
              $ darkenImg 0.3
              $ hitToTexture bmp p side

renderFloorAndCeiling :: Picture
renderFloorAndCeiling = pictures (map (horLine (greyN 0.18)) [-halfH .. -1])
                          <> pictures (map (horLine (greyN 0.1)) [2 .. halfH])
  where
    brownCol = makeColor 0.27 0.21 0.18 1
    halfW = i2f (fst windowSize `div` 2)
    halfH = i2f (snd windowSize `div` 2)
    horLine col y = let
        darkCoef = halfH / (renderDistance * abs y)
                 in color (darkenColor (min 1 darkCoef) col)
                    $ line [(-halfW, y), (halfW, y)]

                  
    

wallColor :: Tile -> Side -> Color
wallColor Wall W = dark $ dark $ dark red
wallColor Wall E = dark $ dark $ dark red
wallColor Wall _ = dark $ dark red
wallColor _    _ = blue

-- | Make color darker by coefficient that should be normalized (from 0 to 1)
darkenColor :: Float -> Color -> Color
darkenColor alpha color = mixColors (1 - alpha) alpha color black


-- | Make image darker by coefficient
darkenImg :: Float -> Picture -> Picture
darkenImg alpha p = pictures [p, darkRect]
  where
    halfH = i2f (snd textureResolution) / 2 + 0.5
    darkRect = color (makeColor 0 0 0 alpha) (line [(0, -halfH), (0, halfH)])

-- | Some raycasting calculations
data Side = N | S | W | E | Inside

hitToTexture :: BitmapData -> (Float, Float) -> Side -> Picture
hitToTexture bmp (x, y) side = modifier (BitmapSection textureRect bmp)
  where
    frac = max (snd $ properFraction x) (snd $ properFraction y)
    resX = fromIntegral (fst textureResolution) :: Float
    textureX = round (frac * resX)
    textureRect = Rectangle (textureX, 0) (1, snd textureResolution)

    modifier = case side of S -> darkenImg 0.25
                            N -> darkenImg 0.25
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

-- NPC and other non-wall objects rendering

renderSprites :: State -> [Float] -> Picture
renderSprites st@(State _ _ pos dir _ textures _ _ sprites) zBuf = pictures 
                                                                   $ map (renderSprite st) sprites

renderSprite :: State -> Sprite -> Picture
renderSprite (State _ _ ppos (pdx,pdy) _ tex _ _ _) (Sprite pos st)
  | abs delta <= fov / 2 = translate screenX 0
                       $ scale (2/dist) (2/dist)
                       $ translate offx offy
                       $ tex (Right st) 
  | otherwise = blank 
  where
    halfW = i2f (fst windowSize) / 2
    (tempX, tempY) = addVV pos (mulSV (-1) ppos)
    spriteAngle = snd (polar (tempX :+ tempY)) 
    playerAngle = snd (polar (pdx :+ pdy)) 
    delta = spriteAngle - playerAngle
    screenX = tan delta * halfW
    halfTexW = i2f (fst textureResolution) / 2
    dist = magV (addVV pos (mulSV (-1) ppos))

    (offx, offy) = (-halfTexW, -halfTexW)
    

-- | Helper functions
addVV :: Vector -> Vector -> Vector
addVV (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

normalV :: Vector -> Vector
normalV (x, y) = (y, -x)

