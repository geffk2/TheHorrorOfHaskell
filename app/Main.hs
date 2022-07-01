{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where
import Graphics.Gloss
import Graphics.Gloss.Algorithms.RayCast
import Graphics.Gloss.Data.QuadTree
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Extent

import qualified Data.Set as S
import Data.Maybe ( fromMaybe, isJust )
import Data.Complex

import GHC.Exts (sortWith)

import ParseMap (parseMap)
import MusicPlayer
import SDL.Mixer (Chunk)
import qualified SDL.Mixer as Mix
import Config
import SDL.Raw.Mixer

-- | Aliases for functions with extra long names
raycast :: Point -> Point -> Extent -> QuadTree a -> Maybe (Point, Extent, a)
raycast = castSegIntoCellularQuadTree

raytrace :: Point -> Point -> Extent -> QuadTree a -> [(Point, Extent, a)]
raytrace = traceSegIntoCellularQuadTree
main :: IO ()

main = do
  initMusicPlayer

  textures <- loadTextures
  sampleMap <- parseMap "maps/customMap.json"
  sounds <- loadSounds

  -- let sprites = [Sprite (6, 6) Barrel]
  let enemy = Sprite (19, 45) Enemy


  let (ext, game) = constructMap sampleMap

  let st = State ext game (11, 50 - 6.5) (1, 0) S.empty textures sounds 1 enemy 0
  -- let st = State ext game (9, 9) (1, 0) S.empty textures sounds 1 sprites 0

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
  enemy :: Sprite,
  difficulty :: Int
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
  playSound sound (sounds s) volume
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

    xf = fst nextPos
    yf = snd nextPos
    x = floor xf
    y = floor yf

    (bx, by) = pos (enemy s)


    newDir
      | S.member (Char 'd') (keysPressed s) = rotateV (dt*pi/2) (playerDir s)
      | S.member (Char 'a') (keysPressed s) = rotateV (-dt*pi/2) (playerDir s)
      | otherwise = playerDir s

    (newMap, hasChanged)
      | S.member (Char 'f') (keysPressed s) = tryOpenDoors s
      | otherwise = (gameMap s, False)


    countDistance cx cy = abs (xf - cx) + abs (yf - cy)

    (sound, volume)
      | hasChanged                    = (Click, MAX_VOLUME)
      | x == 12 && y == 19            = (Fart, MAX_VOLUME)
      | 21 <= x && x <= 50 &&
        0 <= y && y <= 35 &&
        floor newTime `mod` 120 == 0 = (Laugh, MAX_VOLUME)
      | floor newTime `mod` 90   == 0 = (Wind, MAX_VOLUME)
      | x == 4 && 28 <= y && y <= 30  = (Glass, MAX_VOLUME)
      | 21 <= x && x <= 50 &&
        41 <= y && y <= 50            = (Water, MAX_VOLUME - min MAX_VOLUME (floor (4*countDistance 50 41)))
      | ((xf - bx)^2 +
         (yf - by)^2) <= 25           = (Glitch, 70 - min 70 (floor (4*countDistance bx by)))  -- | todo, make it for babayca
      | otherwise                     = (Silence, 0)


tryOpenDoors :: State -> (QuadTree Tile, Bool)
-- tryOpenDoors s@(State ext m (px,py) _ _ _ _ _ _ _)
tryOpenDoors s
  | null doors = (m, False)
  | otherwise  = (fmap replaceButtons (removeLeaves f m), True)
  where
    ext'     = ext s
    m        = gameMap s
    (px, py) = playerPos s

    doors = checkForDoors ext' m (floor px, floor py)
    f (Door c) = c `elem` doors
    f a = False

    replaceButtons (Button c)
      | c `elem` doors = Wall
      | otherwise      = Button c
    replaceButtons tile = tile

checkForDoors :: Extent -> QuadTree Tile -> Coord -> [DoorColor]
checkForDoors ext m (x, y) = concatMap checkPoint points
  where
    points = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    checkCell (Just (Button c)) = [c]
    checkCell _ = []

    checkPoint pos = checkCell (lookupByCoord ext pos m)


-- | Rendering
renderFrame :: State -> IO Picture
-- renderFrame s@(State ext m pos dir keys textures sounds _ _) = do
renderFrame s = do
  playSound sound sounds' volume
  return (floorAndCeiling <> pictures wallsAndSprites)

  where
    ext' = ext s
    m   = gameMap s
    pos = playerPos s
    dir = playerDir s
    keys = keysPressed s
    textures' = textures s
    sounds' = sounds s

    halfW = fst windowSize `div` 2
    screenW = i2f (fst windowSize)

    drawRay i = let
        vecDir = rotateV (i2f i * fov / screenW) dir
        end = pos `addVV` mulSV renderDistance vecDir
      in renderWall textures' i pos dir (raycast pos end ext' m)

    rayResults = map drawRay [-halfW .. halfW]
    zBuf = map snd rayResults
    floorAndCeiling = renderFloorAndCeiling
    sprites = [renderSprite s (enemy s)]

    wallsAndSprites = map fst (sortWith ((* (-1)) . snd) (sprites ++ rayResults))

    textMessage = color white (scale 0.1 0.1 (text (show pos)))

    (sound, volume)
      | S.member (Char 'w') keys = (Walking, MAX_VOLUME)
      | S.member (Char 's') keys = (Walking, MAX_VOLUME)
      | otherwise                = (StopWalking, 0)


renderWall :: Textures -> Int -> Point -> Vector -> Maybe (Point, Extent, Tile) -> (Picture, Float)
renderWall _ _ _ _ Nothing = (blank, 1)
renderWall tex i pos dir (Just (p, ext, t)) = (res (tex (Left t)), dist)
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

    res (Bitmap bmp) = scale 1 scaleFactor
              $ translate x 0
              $ darkenImg (min 1 distDarkCoef)
              $ darkenImg 0.3
              $ hitToTexture bmp p side
    res _ = blank



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

-- renderSprites :: State -> [(Picture, Float)]
-- renderSprites s = map (renderSprite s) (sprites s)



renderSprite :: State -> Sprite -> (Picture, Float)
renderSprite s (Sprite pos st)
  |  abs delta <= fov = (res, dist)
  | otherwise = (blank, dist)

  where
    ppos = playerPos s
    (pdx, pdy) = playerDir s
    tex = textures s

    halfW = i2f (fst windowSize) / 2
    (tempX, tempY) = addVV pos (mulSV (-1) ppos)
    spriteAngle = snd (polar (tempX :+ tempY))
    playerAngle = snd (polar (pdx :+ pdy))

    delta = let d = spriteAngle - playerAngle
              in if d < -pi
                    then d + pi*2
                    else d

    darken alpha = color (makeColor 0 0 0 0) (rectangleSolid texW texW) -- no shading at all bc it works badly
    res = translate screenX 0
                       $ scale (2/dist) (2/dist)
                       $ translate 0 offy
                       $ tex (Right st) <> darken (min 1 (dist/renderDistance))
    screenX = tan delta * halfW
    halfTexW = i2f (fst textureResolution) / 2
    texW = i2f (fst textureResolution)
    dist = magV (addVV pos (mulSV (-1) ppos)) * cos delta

    (offx, offy) = (-halfTexW, -halfTexW)


-- | Helper functions
addVV :: Vector -> Vector -> Vector
addVV (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

normalV :: Vector -> Vector
normalV (x, y) = (y, -x)

