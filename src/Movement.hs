module Movement where

import Constants
import Definitions
import Entities

moveGame :: Float -> GameState -> GameState
moveGame seconds game = collision  seconds
    (physicsSim  seconds (moveEntities seconds game))

physicsSim :: Float -> GameState -> GameState
physicsSim seconds game = game {player =
    Player (location playerInstance) (x, y-60*seconds)
        (canJump playerInstance) (size playerInstance), enemies = map (enemyPhysics seconds) enemyList}
  where
    playerInstance = player game
    (x,y) = direction playerInstance
    enemyList = enemies game

enemyPhysics :: Float -> Enemy -> Enemy
enemyPhysics seconds (Enemy eType pos (x,y) esize) = Enemy eType pos (x, y-60*seconds) esize

collision :: Float -> GameState -> GameState
collision seconds game =
    game {player = foldr subFoldr playerInstance blockList
          , enemies = map (\x -> foldr subFoldr x blockList) enemyList}
  where
    playerInstance = player game
    (x,y) = location playerInstance
    blockList = blocks game
    enemyList = enemies game

subFoldr :: Entity a => Block -> a -> a
subFoldr block = rightCollisionBlock block . leftCollisionBlock block .
                    upCollisionBlock block . downCollisionBlock block

moveEntities :: Float -> GameState -> GameState
moveEntities seconds game@(Game player enemies _ keys) =
    game{ player = movePlayer seconds player
        , enemies = map (moveEnemy seconds) enemies
        }
