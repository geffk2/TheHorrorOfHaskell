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

data DoorColor = Red | Green | Blue
  deriving (Show, Eq)

data Tile = Air | Wall | Floor | Button DoorColor | Door DoorColor
  deriving (Show, Eq)

type GameMap = [[Tile]]

intToTile :: Int -> Tile
intToTile 7 = Door Blue
intToTile 6 = Door Green
intToTile 5 = Door Red
intToTile 4 = Button Blue
intToTile 3 = Button Green
intToTile 2 = Button Red
intToTile 1 = Wall
intToTile _ = Air

tileToInt :: Tile -> Int
tileToInt (Door Blue)    = 7
tileToInt (Door Green)   = 6
tileToInt (Door Red)     = 5
tileToInt (Button Blue)  = 4
tileToInt (Button Green) = 3
tileToInt (Button Red)   = 2
tileToInt Wall           = 1
tileToInt Air            = 0
tileToInt Floor          = -1

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

  let f (Left Floor)       = floorBmp
      f (Left Wall)        = wallBmp
      f (Left Air)         = blank
      f (Left (Button dc)) = btnTexture wallBmp dc
      f (Left (Door dc))   = doorTexture wallBmp dc
      f (Right Barrel)     = barrelBmp
      f (Right Enemy)      = blank
      f (Right Pillar)     = blank

  return f

doorTexture :: Picture -> DoorColor -> Picture
doorTexture p dc = p <> door
  where
    texSize = i2f (fst textureResolution)
    door = translate (texSize/2) (5*texSize/8) (rectangleSolid (texSize/2) (3*texSize/4)) 
    col = case dc of 
      Red -> red
      Green -> green
      Blue -> blue

btnTexture :: Picture -> DoorColor -> Picture
btnTexture p dc = p <> btn
  where
    texSize = i2f (fst textureResolution)
    btn = translate (texSize/2) (texSize/2) (circleSolid (texSize/4)) 
    col = case dc of 
      Red -> red
      Green -> green
      Blue -> blue

instance Functor QuadTree  where
  fmap _ TNil = TNil
  fmap f (TLeaf x) = TLeaf (f x)
  fmap f (TNode a b c d) = TNode (fmap f a) (fmap f b) (fmap f c) (fmap f d)

