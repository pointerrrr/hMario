module Movement where

import Constants
import Data.Maybe
import Definitions
import Entities

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import System.Random

moveGame :: Float -> GameState -> GameState
moveGame seconds game = tickEnemies seconds (collision  seconds
    (physicsSim  seconds (moveEntities seconds game)))

tickEnemies :: Float -> GameState -> GameState
tickEnemies seconds game = game{player = killPlayer newPlayer, enemies = aliveEnemies, projectiles = movedProjectileList, generator = snd genTuple}    
  where
    enemyList = enemies game
    aliveEnemies = filter (\x -> state x /= Dead )(map killEnemy enemyList)
    playerPos = location (player game)
    projectileList = mapMaybe (tickEnemy (fst genTuple) playerPos) aliveEnemies ++ (projectiles game)
    movedProjectileList = map (moveProjectile seconds) projectileList
    genTuple = next (generator game)
    playerInstance@(Player ppos pdir jump psize _) = player game
    (plx, ply) = ppos
    (x1, y1) =  (plx - psize / 2, ply + psize / 2)
    (x2, y2) =  (plx + psize / 2, ply - psize / 2)
    newPlayer = if elem True (map (\x -> pointInBox (pLoc x) (x1,y1) (x2,y2)) projectileList) then (Player ppos pdir jump psize Dead) else playerInstance
    
    
    
tickEnemy :: Int -> Point -> Enemy -> Maybe Projectile
tickEnemy rando point (Enemy PiranhaPlant pos _ _ Alive) = if (rando `mod` 60) == 0 then Just (Projectile pos ((projectileSpeed, projectileSpeed) * (normalizeV (point - pos))) projectileSize) else Nothing
tickEnemy _ _ _ = Nothing
    
killEnemy :: Enemy -> Enemy    
killEnemy e@(Enemy eType pos dir eSize Alive) = e
killEnemy (Enemy eType pos dir eSize dying@(Dead)) = (Enemy eType pos dir eSize Dead)
killEnemy (Enemy eType pos dir eSize dying) = (Enemy eType pos dir eSize (succ dying))

physicsSim :: Float -> GameState -> GameState
physicsSim seconds game = game {player =
    Player (location playerInstance) (x, y-gravity*seconds)
        (canJump playerInstance) (size playerInstance) alive
        , enemies = map (enemyPhysics seconds) enemyList}
  where
    playerInstance = player game
    (x,y) = direction playerInstance
    enemyList = enemies game
    alive = state playerInstance

enemyPhysics :: Float -> Enemy -> Enemy
enemyPhysics seconds (Enemy eType pos (x,y) esize alive) =
    Enemy eType pos (x, y-60*seconds) esize alive

collision :: Float -> GameState -> GameState
collision seconds game =
    game {player = foldr (subFoldr2) subPlayer subSubEnemyList
          , enemies = subSubEnemyList}
  where
    playerInstance = player game
    (x,y) = location playerInstance
    blockList = blocks game
    enemyList = enemies game
    subEnemyList = map (\x -> foldr subFoldr x blockList) enemyList
    subSubEnemyList = map (subFoldr3 playerInstance) subEnemyList
    subPlayer =foldr subFoldr playerInstance blockList

subFoldr :: Entity a => Block -> a -> a
subFoldr block = rightCollisionBlock block . leftCollisionBlock block .
                    upCollisionBlock block . downCollisionBlock block
                    
subFoldr2 :: Entity a => a -> Player -> Player
subFoldr2 entity player = rightCollisionEntityPlayer (leftCollisionEntityPlayer (upCollisionEntityPlayer (downCollisionEntityPlayer player entity) entity) entity) entity

subFoldr3 :: Entity a => a -> Enemy -> Enemy
subFoldr3 entity enemy = rightCollisionEntityEnemy (leftCollisionEntityEnemy (upCollisionEntityEnemy (downCollisionEntityEnemy enemy entity) entity) entity) entity

moveEntities :: Float -> GameState -> GameState
moveEntities seconds game@(Game player enemies projectileList _ _ _ keys) =
    game{ player = movePlayer seconds player
        , enemies = map (moveEnemy seconds) enemies
        , projectiles = map (moveProjectile seconds) projectileList
        }

killPlayer :: Player -> Player
killPlayer player@(Player _ _ _ _ Alive) = player
killPlayer player@(Player _ _ _ _ Dead) = player
killPlayer (Player pos dir jump psize pstate) = (Player pos dir jump psize (succ pstate))
        
moveProjectile :: Float -> Projectile -> Projectile
moveProjectile seconds (Projectile (x,y) (vx,vy) pSize) = (Projectile (x+vx*seconds, y+vy*seconds) (vx,vy) pSize)
