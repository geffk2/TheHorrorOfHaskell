module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import Config
import ParseMap


data State = State {
  gameMap :: GameMap,
  textures :: Textures,
  currentTile :: Tile,
  offset :: (Float, Float),
  pointer :: (Int, Int)
}


handleTime :: Float -> State -> IO State
handleTime _ = return

cellSize :: Int
cellSize = 20

margin :: (Int, Int)
margin = (25, 50)

textureSide :: Int
textureSide = fst textureResolution

handleEvents :: Event -> State -> IO State
handleEvents (EventKey (Char k) Down _ _) state@(State m ts ct (ox,oy) (px, py)) = case k of
                                              'q' -> return state{currentTile = nextTile ct}
                                              'e' -> return state{currentTile = nextTile ct}
                                              'S' -> do
                                                saveMap m 
                                                return state
                                              'w' -> return state{offset=(ox,oy-20)}
                                              'a' -> return state{offset=(ox+20,oy)}
                                              's' -> return state{offset=(ox,oy+20)}
                                              'd' -> return state{offset=(ox-20,oy)}
                                              'i' -> return state{pointer=(px,py+1)}
                                              'k' -> return state{pointer=(px,py-1)}
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
nextTile Air = Wall
nextTile Wall = Air
nextTile _ = Air

renderFrame :: State -> IO Picture
renderFrame state@(State gMap tex cTile offset (px, py)) = return $ pictures (map drawCell items) <> currentTile 
  where
    scaled t = let scaleFactor = cellSizeF / i2f textureSide 
                in translate (cellSizeF / 2) (cellSizeF / 2)
                  $ scale scaleFactor scaleFactor t
    scaledTex = scaled . tex 

    cellSizeF = i2f cellSize
    border = color white (line [(0, 0), (0, cellSizeF), 
                               (cellSizeF, cellSizeF), (cellSizeF, 0), (0, 0)])
    items :: [(Int, Int, Tile)]
    items = [(i, j, fromMaybe Air (gMap !!? (i, j)) ) | i <- [0..length gMap - 1]
                                                      , j <- [0..maybe 1 length (gMap !? i) - 1]]
    

    drawCell (i, j, t) = translate (i2f i * cellSizeF + fst offset) (i2f j * cellSizeF + snd offset)
                         $ border <> scaledTex (Left t) <> 
                            if (px, py) == (i, j) 
                               then translate (cellSizeF / 2) (cellSizeF / 2) (color green $ thickCircle 5 5)
                               else blank
    
    currentTile = translate (i2f (fst windowSize) / (-2) + 1) (i2f (snd windowSize) / 2 - cellSizeF * 2 - 1) (scale 2 2 $ border 
                                                                                                              <> polygon [(0,0),(0,cellSizeF),
                                                                                                                          (cellSizeF,cellSizeF),
                                                                                                                          (cellSizeF,0),(0,0)]
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
    pointer = (0,0)
  }

  playIO window black fps initialState renderFrame handleEvents handleTime
  where
    window = InWindow "Boo" windowSize (700, 200)

