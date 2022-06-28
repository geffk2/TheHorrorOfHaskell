module Config where
import Graphics.Gloss
-- | A modified, safe version of !!
(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_)  !? 0 = Just x
(_:xs) !? i = xs !? (i-1)
infixl 9 !?

(!!?) :: [[a]] -> (Int, Int) -> Maybe a
l !!? (i, j) = (l !? i) >>= (!? j) 
fov :: Float
fov = pi / 2

i2f :: Int -> Float
i2f = fromIntegral


fps :: Int
fps = 30

renderDistance :: Float
renderDistance = 6

windowSize :: (Int, Int)
windowSize = (800, 600)

textureResolution :: (Int, Int) 
textureResolution = (250, 250)


data Tile = Air | Wall | Floor
  deriving (Show, Eq, Ord)

type GameMap = [[Tile]]

intToTile :: Int -> Tile
intToTile 1 = Wall
intToTile _ = Air


data SpriteType = Enemy | Pillar | Barrel
  deriving (Show, Eq)

data Sprite = Sprite {
  pos :: Point,
  texture :: SpriteType
}


type Textures = Either Tile SpriteType -> Picture

loadTextures :: IO Textures 
loadTextures = do
  wallBmp <- loadBMP "textures/wall.bmp"
  floorBmp <- loadBMP "textures/floor.bmp"
  barrelBmp <- loadBMP "textures/barrel.bmp"

  let f (Left Floor) = floorBmp
      f (Left Wall) = wallBmp
      f (Left Air) = blank
      f (Right Barrel) = barrelBmp
      f (Right Enemy) = blank
      f (Right Pillar) = blank

  return f
