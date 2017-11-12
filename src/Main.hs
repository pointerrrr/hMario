module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import Constants
import Definitions
import Entities
import KeyHandling
import Movement

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
    pictures [ pictureBlocks game
             , mkPlayer white (player game)
             ]
  where
    drawBlock :: Block -> Picture
    drawBlock (Block eType (x, y)) = translate x y $
                                color (blockColor eType) $
                                    rectangleSolid blockSize blockSize

    blockColor :: BlockType -> Color
    blockColor Stone = green
    blockColor Brick = orange
    blockColor Coin = yellow
    blockColor PowerUp = blue

    pictureBlocks :: GameState -> Picture
    pictureBlocks = pictures . map drawBlock . blocks

mkPlayer :: Color -> Player -> Picture
mkPlayer col (Player (x,y) _ _) = translate x y $ color col $
                                    rectangleSolid 25 25

-- | Update the game.
update :: Float -> GameState -> GameState
update seconds game
    | pKey $ pressedKeys game = game
    | otherwise = moveGame seconds (handleKeys game)

main :: IO ()
main = play window background fps initialState render checkKeys update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveGame seconds initialState
