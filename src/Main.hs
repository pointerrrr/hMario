module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Definitions
import Entities
import KeyHandling
import Movement

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

-- | The starting state of the game.
initialState :: GameState
initialState = Game
    { player = Player (0,0) (0,0) True
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

handleKeys :: GameState -> GameState
handleKeys = doJump . moveX

moveX :: GameState -> GameState
moveX game
    | left && not right = game { player = Player (location playerObject)
        (-finalSpeed, ySpeed) (canJump playerObject)}
    | right && not left = game { player = Player (location playerObject)
        (finalSpeed, ySpeed) (canJump playerObject)}
    | not right && not left =
        game { player = Player (location playerObject) (0, ySpeed)
          (canJump playerObject)}
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
    | up && canJump playerObject = game { player =
        Player (location playerObject) (xSpeed, ySpeed+50) False}
    | otherwise = game
  where
    keys = pressedKeys game
    up = upKey keys
    playerObject = player game
    (xSpeed, ySpeed) = direction playerObject

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
mkPlayer col (Player (x,y) _ _) = translate x y $ color col $
                                    rectangleSolid 26 86

-- | Update the game.
update :: Float -> GameState -> GameState
update seconds game = moveGame seconds (handleKeys game)

main :: IO ()
main = play window background fps initialState render checkKeys update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveGame seconds initialState
