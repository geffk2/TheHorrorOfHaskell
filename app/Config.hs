module Config where
import Graphics.Gloss
import Graphics.Gloss.Data.QuadTree  
-- | A modified, safe version of !!
(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_)  !? 0 = Just x
(_:xs) !? i = xs !? (i-1)
infixl 9 !?

(!!?) :: [[a]] -> (Int, Int) -> Maybe a
l !!? (i, j) = (l !? i) >>= (!? j) 

fov :: Float
fov = pi / 2 - pi/18

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

data DoorColor = Red | Green | Blue | Yellow | Cyan
  deriving (Show, Eq)

data Tile = Air | Wall | Floor | Button DoorColor | Door DoorColor
  deriving (Show, Eq)

type GameMap = [[Tile]]


nTiles :: Int
nTiles = 12

intToTile :: Int -> Tile
intToTile 11 = Door Cyan
intToTile 10 = Door Yellow
intToTile 9  = Door Blue
intToTile 8  = Door Green
intToTile 7  = Door Red
intToTile 6  = Button Cyan
intToTile 5  = Button Yellow
intToTile 4  = Button Blue
intToTile 3  = Button Green
intToTile 2  = Button Red
intToTile 1  = Wall
intToTile _  = Air

tileToInt :: Tile -> Int
tileToInt (Door Cyan)     = 11
tileToInt (Door Yellow)   = 10
tileToInt (Door Blue)     = 9
tileToInt (Door Green)    = 8
tileToInt (Door Red)      = 7
tileToInt (Button Cyan)   = 6
tileToInt (Button Yellow) = 5
tileToInt (Button Blue)   = 4
tileToInt (Button Green)  = 3
tileToInt (Button Red)    = 2
tileToInt Wall            = 1
tileToInt Air             = 0
tileToInt Floor           = -1

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

  greenButtonBmp  <- loadBMP "textures/green button.bmp"
  redButtonBmp    <- loadBMP "textures/red button.bmp"
  blueButtonBmp   <- loadBMP "textures/blue button.bmp"
  cyanButtonBmp   <- loadBMP "textures/cyan button.bmp"
  yellowButtonBmp <- loadBMP "textures/yellow button.bmp"
  
  greenDoorBmp  <- loadBMP "textures/green door.bmp"
  redDoorBmp    <- loadBMP "textures/red door.bmp"
  blueDoorBmp   <- loadBMP "textures/blue door.bmp"
  cyanDoorBmp   <- loadBMP "textures/cyan door.bmp"
  yellowDoorBmp <- loadBMP "textures/yellow door.bmp"
  

  let f (Left Floor)           = floorBmp
      f (Left Wall)            = wallBmp
      f (Left Air)             = blank

      f (Left (Button Red))    = redButtonBmp
      f (Left (Button Green))  = greenButtonBmp
      f (Left (Button Blue))   = blueButtonBmp
      f (Left (Button Yellow)) = yellowButtonBmp
      f (Left (Button Cyan))   = cyanButtonBmp
      
      f (Left (Door Red))      = redDoorBmp
      f (Left (Door Green))    = greenDoorBmp
      f (Left (Door Blue))     = blueDoorBmp
      f (Left (Door Yellow))   = yellowDoorBmp
      f (Left (Door Cyan))     = cyanDoorBmp

      f (Right Barrel)         = barrelBmp
      f (Right Enemy)          = blank
      f (Right Pillar)         = blank

  return f

removeLeaves :: (a -> Bool) -> QuadTree a -> QuadTree a
removeLeaves _ TNil = TNil
removeLeaves p (TNode a b c d) = TNode (removeLeaves p a) (removeLeaves p b) (removeLeaves p c) (removeLeaves p d)
removeLeaves p (TLeaf x)
  | p x = TNil
  | otherwise = TLeaf x


instance Functor QuadTree where
  -- | fmap (a -> b) -> f a -> f b 
  fmap f TNil = TNil
  fmap f (TLeaf a) = TLeaf (f a)
  fmap f (TNode a b c d) = TNode (fmap f a) (fmap f b) (fmap f c) (fmap f d)
