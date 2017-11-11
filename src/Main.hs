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

class Show a => Entity a where
    location  :: a -> Point
    direction :: a -> Vector
    move :: Float -> a ->  a

instance Entity Player where
    location (Player pos _)  = pos
    direction (Player _ dir) = dir
    move = movePlayer

data Player = Player Point Vector
    deriving Show

instance Entity Enemy where
    location (Enemy _ pos _) = pos
    direction (Enemy _ _ dir) = dir
    move = moveEnemy

data Enemy = Enemy EnemyType Point Vector
    deriving Show

data EnemyType
    = Goomba
    | Koopa
    | PiranhaPlant
    | BulletBill
  deriving Show
    
-- | Data describing the state of the game.
data GameState = Game
    { player :: Player
    , enemies :: [Enemy] -- ^ Right player paddle height.
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

-- | The starting state of the game.
initialState :: GameState
initialState = Game
    { player = (Player (120,0) (0,0))
    , enemies = []
    , pressedKeys = PressedKeys
        { upKey = False
        , downKey = False
        , leftKey = False
        , rightKey = False
        , xKey = False
        , zKey = False
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
mkPlayer col (Player (x,y) _) = translate x y $ color col $ rectangleSolid 26 86
    
-- Make a paddle of a given border and vertical offset.

movePoint :: Float -> Point -> Vector -> Point
movePoint seconds (x,y) (vx, vy) = (x + vx * seconds, y + vy * seconds)

moveGame :: Float -> GameState -> GameState
moveGame seconds game@(Game player enemies keys) =
    game{player = movePlayer seconds player, enemies = map (moveEnemy seconds) enemies}

movePlayer :: Float -> Player -> Player
movePlayer seconds (Player pos dir) = (Player (movePoint seconds pos dir) dir)

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy a = undefined

-- | Number of frames to show per seconds
fps :: Int
fps = 60

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> GameState -> GameState
-- moveBall wordt moveGame
update seconds = moveGame seconds




-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState
-- For an 's' keypress, reset the ball to the center.
-- right
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { rightKey = True }}
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { rightKey = False }}
-- left
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { leftKey = True }}
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { leftKey = False }}
-- up
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { upKey = True }}
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { upKey = False }}
-- down
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { downKey = True }}
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { downKey = False }}
-- z
handleKeys (EventKey (Char 'z') Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { zKey = True }}
handleKeys (EventKey (Char 'z') Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { zKey = False }}
-- x
handleKeys (EventKey (Char 'x') Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { xKey = True }}
handleKeys (EventKey (Char 'x') Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { xKey = False }}
-- p
handleKeys (EventKey (Char 'p') Down _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { pKey = True }}
handleKeys (EventKey (Char 'p') Up _ _) game@(Game _ _ keys) =
    game { pressedKeys = keys { pKey = False }}
-- Do nothing for all other events.
handleKeys _ game = game

playerSpeed :: Float
playerSpeed = 25

main :: IO ()
main = play window background fps initialState render handleKeys update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveGame seconds initialState
