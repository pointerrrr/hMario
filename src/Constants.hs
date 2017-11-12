module Constants where

import Graphics.Gloss
import System.Random

import Definitions

-- | The starting state of the game.
initialState :: Int -> GameState
initialState rando = Game
    { player = Player (0,100) (0,0) True playerSize Alive
    , enemies = [ (Enemy Goomba (0,0) (-10,0) goombaSize Alive)
                , (Enemy PiranhaPlant (-75,65) (0,0) goombaSize Alive)
                ]
    , projectiles = []
    , blocks = initialBlockList
    , generator = mkStdGen (rando)
    , score = 0
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

-- | The initial list of Blocks, making up the level
initialBlockList :: [Block]
initialBlockList = [ Block Stone (x, -120)
                   | x <- [-10 * blockSize, -9 * blockSize .. 10 * blockSize]
                   ] ++ [Block Stone (x,y)
                        | x <- [-5 * blockSize]
                        , y <- [-110,-110+blockSize..50]
                        ]

-- | Window size and placement settings
width, height, offset :: Int
width = 300
height = 300
offset = 100

-- | Window data
window :: Display
window = InWindow "Moria" (width, height) (offset, offset)

-- | Background color
background :: Color
background = black

-- | Set player speed used in movement calculations
playerSpeed :: Float
playerSpeed = 50

-- | Set default properties for Entities and Projectiles
-- Used for movement and collision calculations
playerSize, blockSize, goombaSize, jumpMomentum, gravity :: Float
projectileSpeed, projectileSize :: Float
playerSize = 20
blockSize = 15
goombaSize = 15
jumpMomentum = 60
gravity = 60
projectileSpeed = 30
projectileSize = 5

-- | Number of frames to show per seconds
fps :: Int
fps = 60
