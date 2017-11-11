module Main(main, GameState(..), initialState, render) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector

type Radius = Float
type Position = (Float, Float)

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Moria" (width, height) (offset, offset)

background :: Color
background = black

playerSpeed :: Float
playerSpeed = 25

playerSize, blockSize :: Float
playerSize = 20
blockSize = 10

-- | Number of frames to show per seconds
fps :: Int
fps = 60

class Show a => Entity a where
    location  :: a -> Point
    direction :: a -> Vector
    move :: Float -> a ->  a
    canJump :: a -> Bool
    upCollisionBlock :: Block -> a -> a
    downCollisionBlock :: Block -> a -> a
    leftCollisionBlock :: Block -> a -> a
    rightCollisionBlock :: Block -> a -> a
    upCollisionEntity :: a -> b -> a
    downCollisionEntity :: a -> b -> a
    leftCollisionEntity :: a -> b -> a
    rightCollisionEntity :: a -> b -> a
    

instance Entity Player where
    location (Player pos _ _)  = pos
    direction (Player _ dir _) = dir
    move = movePlayer
    canJump (Player _ _ can) = can
    upCollisionBlock block player = undefined
    downCollisionBlock block player = undefined
    leftCollisionBlock block player = undefined
    rightCollisionBlock block player = undefined
    upCollisionEntity player entity = undefined
    downCollisionEntity player entity = undefined
    leftCollisionEntity player entity = undefined
    rightCollisionEntity player entity = undefined

data Player = Player Point Vector Bool
    deriving Show

instance Entity Enemy where
    location (Enemy _ pos _) = pos
    direction (Enemy _ _ dir) = dir
    move = moveEnemy
    canJump _ = False
    upCollisionBlock block enemy = undefined
    downCollisionBlock block enemy = undefined
    leftCollisionBlock block enemy = undefined
    rightCollisionBlock block enemy = undefined
    upCollisionEntity enemy entity = undefined
    downCollisionEntity enemy entity = undefined
    leftCollisionEntity enemy entity = undefined
    rightCollisionEntity enemy entity = undefined

data Enemy = Enemy EnemyType Point Vector
    deriving Show

data EnemyType = Goomba | Koopa | PiranhaPlant | BulletBill deriving Show
    
-- | Data describing the state of the game.
data GameState = Game
    { player :: Player
    , enemies :: [Enemy] -- ^ Right player paddle height.
    , blocks :: [Block]
    , pressedKeys :: PressedKeys
    } deriving Show
    
data PressedKeys = PressedKeys
    { upKey :: Bool
    , downKey :: Bool
    , leftKey :: Bool
    , rightKey :: Bool
    , xKey :: Bool
    , zKey :: Bool
    , pKey :: Bool
    } deriving Show

data Block = Block BlockType Point deriving Show
  
data BlockType = Stone | Brick | Coin | PowerUp deriving Show
    
-- | The starting state of the game.
initialState :: GameState
initialState = Game
    { player = (Player (0,0) (0,0) True)
    , enemies = []
    , blocks = []
    , pressedKeys = PressedKeys
        { upKey = False
        , downKey = False
        , leftKey = False
        , rightKey = False
        , zKey = False
        , xKey = False        
        , pKey = False
        }
    }

-- | Convert a game state into a picture.
render :: GameState -> Picture
render game =
    pictures [ walls
             , mkPlayer white (player game)
             ]
  where
    wall :: Float -> Picture
    wall offset = translate 0 offset $
                    color wallColor $
                      rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

mkPlayer :: Color -> Player -> Picture
mkPlayer col (Player (x,y) _ _) = translate x y $ color col $ rectangleSolid 26 86
    
-- Make a paddle of a given border and vertical offset.

movePoint :: Float -> Point -> Vector -> Point
movePoint seconds (x,y) (vx, vy) = (x + vx * seconds, y + vy * seconds)

moveGame :: Float -> GameState -> GameState
moveGame seconds game = collision seconds ( physicsSim seconds (moveEntities seconds game))

physicsSim :: Float -> GameState -> GameState
physicsSim seconds game = game {player = (Player (location playerInstance) (x, y-60*seconds) (canJump playerInstance))}
  where
    playerInstance = player game
    (x,y) = direction playerInstance
   

collision :: Float -> GameState -> GameState
collision seconds game = game {player = foldr subFoldr playerInstance blockList}
  where
    playerInstance = player game
    (x,y) = location playerInstance
    blockList = blocks game
    enemyList = enemies game

subFoldr :: Entity a => Block -> a -> a
subFoldr block = upCollisionBlock block . downCollisionBlock block . rightCollisionBlock block . 
                 leftCollisionBlock block
    
moveEntities :: Float -> GameState -> GameState
moveEntities seconds game@(Game player enemies _ keys) = game{player = movePlayer seconds player, enemies = map (moveEnemy seconds) enemies}    

movePlayer :: Float -> Player -> Player
movePlayer seconds (Player pos dir jump) = (Player (movePoint seconds pos dir) dir jump)

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy a enemy = enemy



-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> GameState -> GameState
-- moveBall wordt moveGame
update seconds game = moveGame seconds (handleKeys game)

handleKeys :: GameState -> GameState
handleKeys = doJump . moveX

moveX :: GameState -> GameState
moveX game
    | left && (not right) = game { player = (Player (location playerObject)) (-finalSpeed, ySpeed) (canJump playerObject)}
    | right && (not left) = game { player = (Player (location playerObject)) (finalSpeed, ySpeed) (canJump playerObject)}
    | (not right) && (not left) = game { player = (Player (location playerObject)) (0, ySpeed) (canJump playerObject)}
    | otherwise = game
  where
    keys = pressedKeys game
    right = rightKey keys
    left = leftKey keys
    x = xKey keys
    playerObject = player game
    (_, ySpeed) = direction playerObject
    finalSpeed = if x then playerSpeed * 5 else playerSpeed

doJump :: GameState -> GameState
doJump game
    | up && (canJump playerObject) = game { player = (Player (location playerObject)) (xSpeed, ySpeed+50) False}
    | otherwise = game
  where
    keys = pressedKeys game
    up = upKey keys
    playerObject = player game
    (xSpeed, ySpeed) = direction playerObject

-- | Respond to key events.
checkKeys :: Event -> GameState -> GameState
-- For an 's' keypress, reset the ball to the center.
-- right
checkKeys (EventKey (SpecialKey KeyRight) Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { rightKey = True }}
checkKeys (EventKey (SpecialKey KeyRight) Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { rightKey = False }}
-- left
checkKeys (EventKey (SpecialKey KeyLeft) Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { leftKey = True }}
checkKeys (EventKey (SpecialKey KeyLeft) Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { leftKey = False }}
-- up
checkKeys (EventKey (SpecialKey KeyUp) Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { upKey = True }}
checkKeys (EventKey (SpecialKey KeyUp) Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { upKey = False }}
-- down
checkKeys (EventKey (SpecialKey KeyDown) Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { downKey = True }}
checkKeys (EventKey (SpecialKey KeyDown) Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { downKey = False }}
-- z
checkKeys (EventKey (Char 'z') Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { zKey = True }}
checkKeys (EventKey (Char 'z') Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { zKey = False }}
-- x
checkKeys (EventKey (Char 'x') Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { xKey = True }}
checkKeys (EventKey (Char 'x') Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { xKey = False }}
-- p
checkKeys (EventKey (Char 'p') Down _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { pKey = True }}
checkKeys (EventKey (Char 'p') Up _ _) game@(Game _ _ _ keys) =
    game { pressedKeys = keys { pKey = False }}
-- Do nothing for all other events.
checkKeys _ game = game



main :: IO ()
main = play window background fps initialState render checkKeys update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveGame seconds initialState
