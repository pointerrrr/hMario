module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import System.Random
import System.IO
import System.Directory

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
        (-finalSpeed, ySpeed) (canJump playerObject) (size playerObject) (state playerObject)}
    | right && not left = game { player = Player (location playerObject)
        (finalSpeed, ySpeed) (canJump playerObject) (size playerObject) (state playerObject)}
    | not right && not left =
        game { player = Player (location playerObject) (0, ySpeed)
          (canJump playerObject) (size playerObject) (state playerObject)}
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
        Player (location playerObject) (xSpeed, ySpeed+jumpMomentum) False
            (size playerObject) (state playerObject)}
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
             , pictureEnemies game
             , mkPlayer white (player game)
             , pictureProjectiles game
             ]
  where
    drawBlock :: Block -> Picture
    drawBlock (Block bType (x, y)) = translate x y $
                                color (blockColor bType) $
                                    rectangleSolid blockSize blockSize

    drawEnemies :: Enemy -> Picture
    drawEnemies (Enemy eType (x, y) _ eSize Alive) = translate x y $
                                color (red) $
                                    rectangleSolid eSize eSize
    drawEnemies (Enemy eType (x, y) _ eSize estate) = translate x y $
                                color (red) $
                                    rectangleSolid eSize (eSize/modSize)
                                  where
                                    modSize | estate < Dying5 = 2
                                            | estate < Dying10 = 3
                                            | estate < Dying15 = 4
                                            | otherwise = 5
                                    
    drawProjectile :: Projectile -> Picture
    drawProjectile (Projectile (x,y) _ pSize) = translate x y $
                                                    color (blue) $
                                                        circleSolid pSize

    blockColor :: BlockType -> Color
    blockColor Stone = green
    blockColor Brick = orange
    blockColor Coin = yellow
    blockColor PowerUp = blue

    pictureBlocks :: GameState -> Picture
    pictureBlocks = pictures . map drawBlock . blocks

    pictureEnemies :: GameState -> Picture
    pictureEnemies = pictures . map drawEnemies . enemies
    
    pictureProjectiles :: GameState -> Picture
    pictureProjectiles = pictures . map drawProjectile . projectiles

mkPlayer :: Color -> Player -> Picture
mkPlayer col (Player (x,y) _ _ pSize Alive) = translate x y $ color col $
                                    rectangleSolid pSize pSize
mkPlayer col (Player (x,y) _ _ pSize pstate ) = translate x y $ color col $
                                    rectangleSolid pSize (pSize/modSize)
                                  where
                                    modSize | pstate < Dying5 = 2
                                            | pstate < Dying10 = 3
                                            | pstate < Dying15 = 4
                                            | otherwise = 5
-- | Update the game.
update :: Float -> GameState -> GameState
update seconds game
    | pKey $ pressedKeys game = game
    | otherwise = moveGame seconds (handleKeys game)

main :: IO ()
main = do
    -- source: https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html#t:IOMode
    currentdir <- getCurrentDirectory
    file <- openFile (currentdir ++ "\\src\\playcount.txt") ReadWriteMode
    fileContents <- hGetContents file
    fullList fileContents `seq` hClose file
    write <- writeFile (currentdir ++ "\\src\\playcount.txt") (if (fileContents /= []) then show((stringToInt fileContents) + 1) else (show 1))
    randInt <- randomRIO (1,100000)
    play window background fps (initialState randInt) render checkKeys update

stringToInt :: [Char] -> Int
stringToInt s = read s :: Int
    
fullList :: [a] -> ()
fullList [] = ()
fullList (x:xs) = fullList xs
