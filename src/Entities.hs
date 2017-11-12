module Entities where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Definitions
import Constants

instance Entity Player where
    location (Player pos _ _ _ _)  = pos
    direction (Player _ dir _ _ _) = dir
    move = movePlayer
    canJump (Player _ _ can _ _) = can
    size (Player _ _ _ pSize _) = pSize
    state (Player _ _ _ _ pstate) = pstate
    upCollisionBlock        = upCollisionBlockPlayer
    downCollisionBlock      = downCollisionBlockPlayer
    leftCollisionBlock      = leftCollisionBlockPlayer
    rightCollisionBlock     = rightCollisionBlockPlayer
    upCollisionEntity       = upCollisionEntityPlayer
    downCollisionEntity     = downCollisionEntityPlayer
    leftCollisionEntity     = leftCollisionEntityPlayer
    rightCollisionEntity    = rightCollisionEntityPlayer

instance Entity Enemy where
    location (Enemy _ pos _ _ _) = pos
    direction (Enemy _ _ dir _ _) = dir
    move = moveEnemy
    canJump _ = False
    size (Enemy _ _ _ eSize _) = eSize
    state (Enemy _ _ _ _ estate) = estate
    upCollisionBlock        = upCollisionBlockEnemy
    downCollisionBlock      = downCollisionBlockEnemy
    leftCollisionBlock      = leftCollisionBlockEnemy
    rightCollisionBlock     = rightCollisionBlockEnemy
    upCollisionEntity       = upCollisionEntityEnemy
    downCollisionEntity     = downCollisionEntityEnemy
    leftCollisionEntity     = leftCollisionEntityEnemy
    rightCollisionEntity    = rightCollisionEntityEnemy

movePlayer :: Float -> Player -> Player
movePlayer seconds (Player pos dir jump pSize alive) =
    Player (movePoint seconds pos dir) dir jump pSize alive

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy seconds (Enemy eType pos dir eSize alive) =
    Enemy eType (movePoint seconds pos dir) dir eSize alive

movePoint :: Float -> Point -> Vector -> Point
movePoint seconds (x,y) (vx, vy) = (x + vx * seconds, y + vy * seconds)


upCollisionBlockPlayer :: Block -> Player -> Player
upCollisionBlockPlayer block player
    |    (py - psize <= by + bsize)
      && (px + psize >= bx - bsize + 3)
      && (px - psize <= bx + bsize - 3)
      && (py >= by)
        = Player (px, py + pbDelta) (vx, 0) True pSize alive
    | otherwise = player
  where
    alive = state player
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) _  pSize _ = player
    psize = (pSize ) / 2
    bsize = (blockSize ) / 2
    pbDelta = (pSize/2 + blockSize/2) -(py - by)

downCollisionBlockPlayer    :: Block -> Player -> Player
downCollisionBlockPlayer block player
    |    (py + psize >= by - bsize)
      && (px + psize >= bx - bsize + 3)
      && (px - psize <= bx + bsize - 3)
      && (py <= by )
        = Player (px, py) (vx, -5) False pSize alive
    | otherwise = player
  where
    alive = state player
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize _ = player
    psize = (pSize) / 2
    bsize = (blockSize) / 2

leftCollisionBlockPlayer    :: Block -> Player -> Player
leftCollisionBlockPlayer block player
    |    (px + psize >= bx - bsize)
      && (py - psize <= by + bsize)
      && (py + psize >= by - bsize)
      && not (py - psize >= by + bsize)
      && not (px >= bx)
        = Player (px + pbDelta, py) (0, vy) jump pSize alive
    | otherwise = player
  where
    alive = state player
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize _ = player
    psize = (pSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (-blockSize/2 - pSize/2) + (bx - px)

rightCollisionBlockPlayer :: Block -> Player -> Player
rightCollisionBlockPlayer block player
    |    (px - psize <= bx + bsize)
      && (py - psize <= by + bsize)
      && (py + psize >= by - bsize)
      && not (py - psize >= by + bsize)
      && not (px <= bx )
        = Player (px + pbDelta, py) (0, vy) jump pSize alive
    | otherwise = player
  where
    alive = state player
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize _ = player
    psize = (pSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (pSize/2 + blockSize/2) - (px - bx)

upCollisionEntityPlayer :: Entity a => Player -> a -> Player
upCollisionEntityPlayer player entity
    |    (py - psize <= ey + esize)
      && (px + psize >= ex - esize + 3)
      && (px - psize <= ex + esize - 3)
      && (py >= ey)
        = Player (px, py + pbDelta) (vx, 60) False pSize alive
    | otherwise = player
  where
    alive = state player
    (ex, ey)  = location entity
    eSize = size entity
    Player (px, py) (vx, vy) _  pSize _ = player
    psize = (pSize ) / 2
    esize = (eSize ) / 2
    pbDelta = (pSize/2 + eSize/2) -(py - ey)

downCollisionEntityPlayer :: Entity a => Player -> a -> Player
downCollisionEntityPlayer player entity
    |    (py + psize >= by - bsize)
      && (px + psize >= bx - bsize + 3)
      && (px - psize <= bx + bsize - 3)
      && (py <= by )
        = Player (px, py) (vx, -5) False pSize Dying1

    | otherwise = player
  where
    (bx, by)  = location entity
    eSize = size entity
    Player (px, py) (vx, vy) jump pSize _ = player
    psize = (pSize) / 2
    bsize = (eSize) / 2

leftCollisionEntityPlayer :: Entity a => Player -> a -> Player
leftCollisionEntityPlayer player entity
    |    (px + psize >= bx - bsize) -- (3)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px >= bx) -- not (2)
        = Player (px + pbDelta, py) (0, vy) jump pSize Dying1
    | otherwise = player
  where
    (bx, by)  = location entity
    eSize = size entity
    Player (px, py) (vx, vy) jump pSize _ = player
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
        = Player (px + pbDelta, py) (0, vy) jump pSize Dying1

    | otherwise = player
  where
    Player (px, py) (vx, vy) jump pSize _ = player
    (bx, by) = location entity
    psize = (pSize - 1) / 2
    eSize = size entity
    bsize = (eSize - 1) / 2
    pbDelta = (pSize/2 + eSize/2) - (px - bx)

-- Enemy collisions

upCollisionBlockEnemy :: Block -> Enemy -> Enemy
upCollisionBlockEnemy block enemy
    |    (ey - esize <= by + bsize)
      && (ex + esize >= bx - bsize + 3)
      && (ex - esize <= bx + bsize - 3)
      && (ey >= by)
        = Enemy eType (ex, ey + pbDelta) (vx, 0) eSize alive
    | otherwise = enemy
  where
    alive = state enemy
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize _ = enemy
    esize = (eSize ) / 2
    bsize = (blockSize ) / 2
    pbDelta = (eSize/2 + blockSize/2) - (ey - by)

downCollisionBlockEnemy :: Block -> Enemy -> Enemy
downCollisionBlockEnemy block enemy
    |(ey + esize >= by - bsize)
      && (ex + esize >= bx - bsize + 3)
      && (ex - esize <= bx + bsize - 3)
      && (ey <= by )
        = Enemy eType (ex, ey) (vx, -5) eSize alive
    | otherwise = enemy
  where
    alive = state enemy
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize _ = enemy
    esize = (eSize) / 2
    bsize = (blockSize) / 2

leftCollisionBlockEnemy :: Block -> Enemy -> Enemy
leftCollisionBlockEnemy block enemy
    |    (ex + esize >= bx - bsize)
      && (ey - esize <= by + bsize)
      && (ey + esize >= by - bsize)
      && not (ey - eSize/2 >= by + bsize)
      && not (ex >= bx)
        = Enemy eType (ex + pbDelta, ey) (-vx, vy) eSize alive
    | otherwise = enemy
  where
    alive = state enemy
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize _ = enemy
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
        = Enemy eType (ex + pbDelta, ey) (-vx, vy) eSize alive
    | otherwise = enemy
  where
    alive = state enemy
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize _ = enemy
    esize = (eSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (eSize/2 + blockSize/2) - (ex - bx)

upCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
upCollisionEntityEnemy enemy entity
    |    (ey - esize <= by + bsize)
      && (ex + esize >= bx - bsize + 3)
      && (ex - esize <= bx + bsize - 3)
      && (ey >= by)
        = Enemy eType (ex, ey + pbDelta) (vx, 0) eSize alive
    | otherwise = enemy
  where
    (bx, by)  = location entity
    Enemy eType (ex, ey) (vx, vy) eSize alive = enemy
    esize = (eSize ) / 2
    bsize = ((size entity) ) / 2
    pbDelta = (eSize/2 + (size entity)/2) - (ey - by)

downCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
downCollisionEntityEnemy enemy entity
    |(ey + esize >= by - bsize)
      && (ex + esize >= bx - bsize + 3)
      && (ex - esize <= bx + bsize - 3)
      && (ey <= by )
        = Enemy eType (ex, ey) (0, 0) eSize Dying1
    | otherwise = enemy
  where
    (bx, by)  = location entity
    Enemy eType (ex, ey) (vx, vy) eSize alive = enemy
    esize = (eSize) / 2
    bsize = ((size entity)) / 2

leftCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
leftCollisionEntityEnemy enemy entity
    |    (ex + esize >= bx - bsize)
      && (ey - esize <= by + bsize)
      && (ey + esize >= by - bsize)
      && not (ey - eSize/2 >= by + bsize)
      && not (ex >= bx)
        = Enemy eType (ex , ey) (-vx, vy) eSize alive
    | otherwise = enemy
  where
    (bx, by)  = location entity
    Enemy eType (ex, ey) (vx, vy) eSize alive = enemy
    esize = (eSize - 1) / 2
    bsize = ((size entity) - 1) / 2
    pbDelta = (-(size entity)/2 - eSize/2) + (bx - ex)

rightCollisionEntityEnemy :: Entity a => Enemy -> a -> Enemy
rightCollisionEntityEnemy enemy entity
    |    (ex - esize <= bx + bsize)
      && (ey - esize <= by + bsize)
      && (ey + esize >= by - bsize)
      && not (ey - eSize/2 + 2 >= by + bsize)
      && not (ex <= bx )
        = Enemy eType (ex , ey) (-vx, vy) eSize alive
    | otherwise = enemy
  where
    (bx, by)  = location entity
    Enemy eType (ex, ey) (vx, vy) eSize alive = enemy
    esize = (eSize - 1) / 2
    bsize = ((size entity) - 1) / 2
    pbDelta = (eSize/2 + (size entity)/2) - (ex - bx)
