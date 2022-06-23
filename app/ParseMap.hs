module ParseMap where

import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy as B


data Tile = Wall | Air
  deriving (Show, Eq)


type GameMap = [[Tile]]

intToTile :: Int -> Tile
intToTile 1 = Wall
intToTile _ = Air

parseMap :: String -> IO GameMap
parseMap mapName = do
    jsonData <- B.readFile mapName
    let rawMap = decode jsonData :: Maybe [[Int]]
    let tileMap = case rawMap of
            Nothing      -> []
            Just someMap -> map (map (intToTile . (`mod` 2)) ) someMap
    return tileMap


