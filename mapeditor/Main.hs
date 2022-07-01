{-# LANGUAGE BlockArguments #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Config
import ParseMap
import qualified Control.Monad


data State = State {
  gameMap :: GameMap,
  textures :: Textures,
  currentTile :: Tile,
  offset :: (Float, Float),
  pointer :: (Int, Int),
  timePassed :: Float
}


handleTime :: Float -> State -> IO State
handleTime dt state = do
  if floor (timePassed state) `mod` 10 == 0
  then do
    saveMap (gameMap state)
    return state{timePassed = dt}
  else
    return state{timePassed = timePassed state + dt}


cellSize :: Float
cellSize = 20

margin :: (Int, Int)
margin = (25, 50)

textureSide :: Int
textureSide = fst textureResolution

handleEvents :: Event -> State -> IO State
handleEvents (EventKey (Char k) Down _ _) state@(State m ts ct (ox,oy) (px, py) t) = case k of
                                              'q' -> return state{currentTile = prevTile ct}
                                              'e' -> return state{currentTile = nextTile ct}
                                              'S' -> do
                                                saveMap m
                                                return state
                                              'w' -> return state{offset=(ox,oy+cellSize)}
                                              'a' -> return state{offset=(ox+cellSize,oy)}
                                              's' -> return state{offset=(ox,oy-cellSize)}
                                              'd' -> return state{offset=(ox-cellSize,oy)}
                                              'i' -> return state{pointer=(px,py-1)}
                                              'k' -> return state{pointer=(px,py+1)}
                                              'j' -> return state{pointer=(px-1,py)}
                                              'l' -> return state{pointer=(px+1,py)}
                                              'h' -> return state{gameMap=updatedMap}
                                              _   -> return state
  where
    updateAt :: Int -> (a -> a) -> [a] -> [a]
    updateAt i f l = take i l ++ updated ++ drop (i + 1) l
      where
        updated = case l !? i of Nothing -> []
                                 Just e  -> [f e]

    updatedMap = updateAt px (updateAt py (const ct)) m
handleEvents _ state = return state


nextTile :: Tile -> Tile
nextTile tile = intToTile $ (tileToInt tile + 1) `mod` nTiles

prevTile :: Tile -> Tile
prevTile tile = intToTile $ (nTiles + tileToInt tile - 1) `mod` nTiles



renderFrame :: State -> IO Picture
renderFrame state@(State gMap tex cTile offset (px, py) t) = return $ pictures (map drawCell items) <> currentTile
  where
    scaled t = let scaleFactor = cellSize / i2f textureSide
                in translate (cellSize / 2) (cellSize / 2)
                  $ scale scaleFactor scaleFactor t
    scaledTex = scaled . tex

    border = color white (line [(0, 0), (0, cellSize),
                               (cellSize, cellSize), (cellSize, 0), (0, 0)])
    items :: [(Int, Int, Tile)]
    items = [(i, j, fromMaybe Air (gMap !!? (i, j)) ) | i <- [0..length gMap - 1]
                                                      , j <- [0..maybe 1 length (gMap !? i) - 1]]


    drawCell (i, j, t) = translate (i2f i * cellSize + fst offset) (-(i2f j * cellSize + snd offset))
                         $ border <> scaledTex (Left t) <>
                            if (px, py) == (i, j)
                               then translate (cellSize / 2) (cellSize / 2) (color green $ thickCircle 5 5)
                               else blank

    currentTile = translate (i2f (fst windowSize) / (-2) + 1) (i2f (snd windowSize) / 2 - cellSize * 2 - 1) (scale 2 2 $ border
                                                                                                              <> polygon [(0,0),(0,cellSize),
                                                                                                                          (cellSize,cellSize),
                                                                                                                          (cellSize,0),(0,0)]
                                                                                                              <> scaledTex (Left cTile))

main :: IO()
main = do
  tex <- loadTextures
  gm <- parseMap "maps/customMap.json"

  let initialState = State {
    gameMap = gm,
    textures = tex,
    currentTile = Air,
    offset = (-100, -100),
    pointer = (0,0),
    timePassed = 0
  }

  playIO window black fps initialState renderFrame handleEvents handleTime
  where
    window = InWindow "Boo" windowSize (700, 200)

