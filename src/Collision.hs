module Collision where

import Constants
import Definitions

-- Player collisions

upCollisionBlockPlayer :: Block -> Player -> Player
upCollisionBlockPlayer block player
    |    (py - psize <= by + bsize)
      && (px + psize >= bx - bsize + 3)
      && (px - psize <= bx + bsize - 3)
      && (py >= by)
        = Player (px, py + pbDelta) (vx, 0) True pSize
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) _  pSize = player
    psize = (pSize ) / 2
    bsize = (blockSize ) / 2
    pbDelta = (pSize/2 + blockSize/2) -(py - by)

downCollisionBlockPlayer    :: Block -> Player -> Player
downCollisionBlockPlayer block player
    |    (py + psize >= by - bsize)
      && (px + psize >= bx - bsize + 3)
      && (px - psize <= bx + bsize - 3)
      && (py <= by )
        = Player (px, py) (vx, -5) False pSize
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize) / 2
    bsize = (blockSize) / 2

leftCollisionBlockPlayer    :: Block -> Player -> Player
leftCollisionBlockPlayer block player
    |    (px + psize >= bx - bsize)
      && (py - psize <= by + bsize)
      && (py + psize >= by - bsize)
      && not (py - psize >= by + bsize)
      && not (px >= bx)
        = Player (px + pbDelta, py) (0, vy) jump pSize
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (-blockSize/2 - playerSize/2) + (bx - px)

rightCollisionBlockPlayer :: Block -> Player -> Player
rightCollisionBlockPlayer block player
    |    (px - psize <= bx + bsize)
      && (py - psize <= by + bsize)
      && (py + psize >= by - bsize)
      && not (py - psize >= by + bsize)
      && not (px <= bx )
        = Player (px + pbDelta, py) (0, vy) jump pSize
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (playerSize/2 + blockSize/2) - (px - bx)

upCollisionEntityPlayer :: Entity a => Player -> a -> Player
upCollisionEntityPlayer player entity
    |    (py - psize <= ey + esize)
      && (px + psize >= ex - esize + 3)
      && (px - psize <= ex + esize - 3)
      && (py >= ey)
        = Player (px, py + pbDelta) (vx, 0) True 50
    | otherwise = Player (px, py) (vx, vy) True 50
  where
    (ex, ey)  = location entity
    eSize = size entity
    Player (px, py) (vx, vy) _  pSize = player
    psize = (pSize ) / 2
    esize = (eSize ) / 2
    pbDelta = (playerSize/2 + eSize/2) -(py - ey)

downCollisionEntityPlayer :: Entity a => Player -> a -> Player
downCollisionEntityPlayer player entity
    |    (py + psize >= by - bsize)
      && (px + psize >= bx - bsize + 3)
      && (px - psize <= bx + bsize - 3)
      && (py <= by )
        = Player (px, py) (vx, -5) False 50

    | otherwise = player
  where
    (bx, by)  = location entity
    eSize = size entity
    Player (px, py) (vx, vy) jump pSize = player
    psize = (pSize) / 2
    bsize = (eSize) / 2

leftCollisionEntityPlayer :: Entity a => Player -> a -> Player
leftCollisionEntityPlayer player entity
    |    (px + psize >= bx - bsize) -- (3)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px >= bx) -- not (2)
        = Player (px + pbDelta, py) (0, vy) jump 50
    | otherwise = player
  where
    (bx, by)  = location entity
    eSize = size entity
    Player (px, py) (vx, vy) jump pSize = player
    psize = (pSize - 1) / 2
    bsize = (eSize - 1) / 2
    pbDelta = (-eSize/2 - pSize/2) + (bx - px)

rightCollisionEntityPlayer :: Entity a => Player -> a -> Player
rightCollisionEntityPlayer player entity 
    |    (px - psize <= bx + bsize)
      && (py - psize <= by + bsize)
      && (py + psize >= by - bsize)
      && not (py - psize >= by + bsize)
      && not (px <= bx ) -- not (3)
        = Player (px + pbDelta, py) (0, vy) jump 50

    | otherwise = player
  where
    Player (px, py) (vx, vy) jump pSize = player
    (bx, by) = location entity
    psize = (playerSize - 1) / 2
    eSize = size entity
    bsize = (eSize - 1) / 2
    pbDelta = (playerSize/2 + eSize/2) - (px - bx)

-- Enemy collisions

upCollisionBlockEnemy :: Block -> Enemy -> Enemy
upCollisionBlockEnemy block enemy
    |    (ey - esize <= by + bsize)
      && (ex + esize >= bx - bsize + 3)
      && (ex - esize <= bx + bsize - 3)
      && (ey >= by)
        = Enemy eType (ex, ey + pbDelta) (vx, 0) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize ) / 2
    bsize = (blockSize ) / 2
    pbDelta = (eSize/2 + blockSize/2) - (ey - by)

downCollisionBlockEnemy :: Block -> Enemy -> Enemy
downCollisionBlockEnemy block enemy
    |(ey + esize >= by - bsize)
      && (ex + esize >= bx - bsize + 3)
      && (ex - esize <= bx + bsize - 3)
      && (ey <= by )
        = Enemy eType (ex, ey) (vx, -5) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize) / 2
    bsize = (blockSize) / 2

leftCollisionBlockEnemy :: Block -> Enemy -> Enemy
leftCollisionBlockEnemy block enemy
    |    (ex + esize >= bx - bsize)
      && (ey - esize <= by + bsize)
      && (ey + esize >= by - bsize)
      && not (ey - eSize/2 >= by + bsize)
      && not (ex >= bx)
        = Enemy eType (ex + pbDelta, ey) (-vx, vy) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (-blockSize/2 - eSize/2) + (bx - ex)

rightCollisionBlockEnemy :: Block -> Enemy -> Enemy
rightCollisionBlockEnemy block enemy
    |    (ex - esize <= bx + bsize)
      && (ey - esize <= by + bsize)
      && (ey + esize >= by - bsize)
      && not (ey - eSize/2 + 2 >= by + bsize)
      && not (ex <= bx )
        = Enemy eType (ex + pbDelta, ey) (-vx, vy) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (eSize/2 + blockSize/2) - (ex - bx)

upCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
upCollisionEntityEnemy enemy entity = enemy

downCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
downCollisionEntityEnemy enemy entity = enemy

leftCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
leftCollisionEntityEnemy enemy entity = enemy

rightCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
rightCollisionEntityEnemy enemy entity = enemy
