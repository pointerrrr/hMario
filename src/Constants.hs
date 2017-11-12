module Constants where

import Graphics.Gloss

import Definitions

-- | The starting state of the game.
initialState :: GameState
initialState = Game
    { player = Player (0,100) (0,0) True playerSize
    , enemies = [(Enemy Goomba (0,0) (-10,0) goombaSize)]
    , blocks = initialBlockList
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

initialBlockList :: [Block]
initialBlockList = [ Block Stone (x, -120)
                   | x <- [-10 * blockSize, -9 * blockSize .. 10 * blockSize]
                   ] ++ [Block Stone (x,y)
                        | x <- [-5 * blockSize]
                        , y <- [-110,-100..50]
                        ]

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Moria" (width, height) (offset, offset)

background :: Color
background = black

playerSpeed :: Float
playerSpeed = 50

playerSize, blockSize, goombaSize :: Float
playerSize = 20
blockSize = 10
goombaSize = 15

-- | Number of frames to show per seconds
fps :: Int
fps = 60
