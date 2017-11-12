module Movement where

import Definitions
import Entities

moveGame :: Float -> GameState -> GameState
moveGame seconds game = collision seconds
    ( physicsSim seconds (moveEntities seconds game))

physicsSim :: Float -> GameState -> GameState
physicsSim seconds game = game {player =
    Player (location playerInstance) (x, y-60*seconds)
        (canJump playerInstance)}
  where
    playerInstance = player game
    (x,y) = direction playerInstance


collision :: Float -> GameState -> GameState
collision seconds game =
    game {player = foldr subFoldr playerInstance blockList}
  where
    playerInstance = player game
    (x,y) = location playerInstance
    blockList = blocks game
    enemyList = enemies game

subFoldr :: Entity a => Block -> a -> a
subFoldr block = upCollisionBlock block . downCollisionBlock block .
    rightCollisionBlock block . leftCollisionBlock block

moveEntities :: Float -> GameState -> GameState
moveEntities seconds game@(Game player enemies _ keys) =
    game{ player = movePlayer seconds player
        , enemies = map (moveEnemy seconds) enemies
        }
