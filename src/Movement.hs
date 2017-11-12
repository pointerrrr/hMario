module Movement where

import Collision
import Constants
import Definitions
import Entities

moveGame :: Float -> GameState -> GameState
moveGame seconds game = collision  seconds
    (physicsSim  seconds (moveEntities seconds game))

physicsSim :: Float -> GameState -> GameState
physicsSim seconds game = game {player =
    Player (location playerInstance) (x, y-60*seconds)
        (canJump playerInstance) (size playerInstance)
        , enemies = map (enemyPhysics seconds) enemyList}
  where
    playerInstance = player game
    (x,y) = direction playerInstance
    enemyList = enemies game

enemyPhysics :: Float -> Enemy -> Enemy
enemyPhysics seconds (Enemy eType pos (x,y) esize) =
    Enemy eType pos (x, y-60*seconds) esize

collision :: Float -> GameState -> GameState
collision seconds game =
    game {player = foldr (flip subFoldr2) subPlayer subEnemyList
          , enemies = subEnemyList}
  where
    playerInstance = player game
    (x,y) = location playerInstance
    blockList = blocks game
    enemyList = enemies game
    subEnemyList = map (\x -> foldr subFoldr x blockList) enemyList
    subPlayer =foldr subFoldr playerInstance blockList

subFoldr :: Entity a => Block -> a -> a
subFoldr block = rightCollisionBlock block . leftCollisionBlock block .
                    upCollisionBlock block . downCollisionBlock block
                    
subFoldr2 :: Entity a => Player -> a -> Player
subFoldr2 player = rightCollisionEntityPlayer player . leftCollisionEntityPlayer player .
                    upCollisionEntityPlayer player . downCollisionEntityPlayer player

moveEntities :: Float -> GameState -> GameState
moveEntities seconds game@(Game player enemies _ keys) =
    game{ player = movePlayer seconds player
        , enemies = map (moveEnemy seconds) enemies
        }
