module Config where
import Graphics.Gloss

fov :: Float
fov = pi / 2

renderDistance :: Float
renderDistance = 6

windowSize :: (Int, Int)
windowSize = (800, 600)

textureResolution :: (Int, Int) 
textureResolution = (250, 250)


data Tile = Wall | Air | Floor
  deriving (Show, Eq)


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


type Textures = [(Either Tile SpriteType, BitmapData)]

loadTextures :: IO Textures 
loadTextures = do
  Bitmap wallBmp <- loadBMP "textures/wall.bmp"
  Bitmap floorBmp <- loadBMP "textures/floor.bmp"
  Bitmap barrelBmp <- loadBMP "textures/barrel.bmp"
  return [(Left Wall, wallBmp), (Left Floor, floorBmp),
          (Right Barrel, barrelBmp)]
