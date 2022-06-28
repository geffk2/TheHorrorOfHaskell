module MapEditor where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Config



data State = State {
  gameMap :: GameMap,
  textures :: Textures,
  currentTile :: Tile
}


initialState :: GameMap
initialState = replicate 100 (replicate 100 Air)


handleTime :: Float -> State -> IO State
handleTime _ = return


handleEvents :: Event -> State -> IO State
handleEvents _ = return


renderFrame :: State -> IO Picture
renderFrame state = return blank
  where
    tileMap = gameMap state
    



main :: IO()
main = do
  textures <- loadTextures
  
  playIO window black fps initialState renderFrame handleEvents handleTime
  where
    window = InWindow "Boo" windowSize (700, 200)

