module Collision where

import Constants
import Definitions

-- Player collisions
upCollisionBlockPlayer      :: Block -> Player -> Player
downCollisionBlockPlayer    :: Block -> Player -> Player
leftCollisionBlockPlayer    :: Block -> Player -> Player
rightCollisionBlockPlayer   :: Block -> Player -> Player
upCollisionEntityPlayer     :: Entity a => Player -> a -> Player
downCollisionEntityPlayer   :: Entity a => Player -> a -> Player
leftCollisionEntityPlayer   :: Entity a => Player -> a -> Player
rightCollisionEntityPlayer  :: Entity a => Player -> a -> Player

-- Predicates
--    1. playerBottom not above blockTop
--      (*) (py - psize <= by + bsize)
--    2. playerRight not left to blockLeft
--      (*) (px + psize >= bx - bsize)
--    3. playerLeft not right to blockRight
--      (*) (px - psize <= bx + bsize)
--    4. playerTop not under blockTop
--      (*) (py + psize <= by + bsize)
--    5. playerTop not under blockBottom
--      (*) (py + psize <= by - bsize)
--    6. playerBottom not above blockBottom
--      (*) (py + psize >= by + bsize)

-- | Up and down are somehow switched, dus upCollision has jump = False (temp)
upCollisionBlockPlayer block player
    |    (py - psize <= by + bsize)     -- (1)
      && (px + psize >= bx - bsize + 3)     -- (2)
      && (px - psize <= bx + bsize - 3)     -- (3)
      && (py >= by)     -- (5)
        = Player (px, py + pbDelta) (vx, 0) True pSize
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) _  pSize = player
    psize = (pSize ) / 2
    bsize = (blockSize ) / 2
    pbDelta = (pSize/2 + blockSize/2) -(py - by)

-- | Up/down switched, thus here jump = True (for now)
downCollisionBlockPlayer block player
    |    (py + psize >= by - bsize) -- (4)
      && (px + psize >= bx - bsize + 3) -- (2)
      && (px - psize <= bx + bsize - 3) -- (3)
      && (py <= by ) -- (1)
        = Player (px, py) (vx, -5) False pSize
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize) / 2
    bsize = (blockSize) / 2

-- | Seems to find the right collision, but move player like downCollision
leftCollisionBlockPlayer block player
    |    (px + psize >= bx - bsize) -- (3)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px >= bx) -- not (2)
        = Player (px + pbDelta, py) (0, vy) jump pSize
        
        
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (-blockSize/2 - playerSize/2) + (bx - px)

-- | Seems to find the right collision, but move player like downCollision
rightCollisionBlockPlayer block player
    |    (px - psize <= bx + bsize) -- (2)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px <= bx ) -- not (3)
        = Player (px + pbDelta, py) (0, vy) jump pSize
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (playerSize/2 + blockSize/2) - (px - bx)

upCollisionEntityPlayer player entity
    |    (py - psize <= ey + esize)     -- (1)
      && (px + psize >= ex - esize + 3)     -- (2)
      && (px - psize <= ex + esize - 3)     -- (3)
      && (py >= ey)     -- (5)
        = Player (px, py + pbDelta) (vx, 0) True pSize
    | otherwise = player
  where
    (ex, ey)  = location entity
    Player (px, py) (vx, vy) _  pSize = player
    psize = (pSize ) / 2
    esize = (size entity ) / 2
    pbDelta = (playerSize/2 + blockSize/2) -(py - ey)

downCollisionEntityPlayer player entity = undefined
    {- |    (py + psize >= by - bsize) -- (4)
      && (px + psize >= bx - bsize + 3) -- (2)
      && (px - psize <= bx + bsize - 3) -- (3)
      && (py <= by ) -- (1)
        = Player (px, py) (vx, -5) False pSize
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize) / 2
    bsize = (blockSize) / 2-}
    
    
leftCollisionEntityPlayer player entity = undefined
    {-|    (px + psize >= bx - bsize) -- (3)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px >= bx) -- not (2)
        = Player (px + pbDelta, py) (0, vy) jump pSize
        
        
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (-blockSize/2 - playerSize/2) + (bx - px)-}

rightCollisionEntityPlayer player entity = undefined
    {-|    (px - psize <= bx + bsize) -- (2)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px <= bx ) -- not (3)
        = Player (px + pbDelta, py) (0, vy) jump pSize
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (playerSize/2 + blockSize/2) - (px - bx)-}

-- Enemy collisions
upCollisionBlockEnemy       :: Block -> Enemy -> Enemy
downCollisionBlockEnemy     :: Block -> Enemy -> Enemy
leftCollisionBlockEnemy     :: Block -> Enemy -> Enemy
rightCollisionBlockEnemy    :: Block -> Enemy -> Enemy
upCollisionEntityEnemy      :: Entity a => Enemy -> a -> Enemy
downCollisionEntityEnemy    :: Entity a => Enemy -> a -> Enemy
leftCollisionEntityEnemy    :: Entity a => Enemy -> a -> Enemy
rightCollisionEntityEnemy   :: Entity a => Enemy -> a -> Enemy

upCollisionBlockEnemy block enemy 
    |    (ey - esize <= by + bsize)     -- (1)
      && (ex + esize >= bx - bsize + 3)     -- (2)
      && (ex - esize <= bx + bsize - 3)     -- (3)
      && (ey >= by)     -- (5)
        = Enemy eType (ex, ey + pbDelta) (vx, 0) eSize   
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize ) / 2
    bsize = (blockSize ) / 2
    pbDelta = (eSize/2 + blockSize/2) - (ey - by)
    

    
downCollisionBlockEnemy block enemy
    |(ey + esize >= by - bsize) -- (4)
      && (ex + esize >= bx - bsize + 3) -- (2)
      && (ex - esize <= bx + bsize - 3) -- (3)
      && (ey <= by ) -- (1)
        = Enemy eType (ex, ey) (vx, -5) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize) / 2
    bsize = (blockSize) / 2
    
leftCollisionBlockEnemy block enemy
    |    (ex + esize >= bx - bsize) -- (3)
      && (ey - esize <= by + bsize) -- (1)
      && (ey + esize >= by - bsize) -- (5)
      && not (ey - eSize/2 >= by + bsize) -- not (4)
      && not (ex >= bx) -- not (2)
        = Enemy eType (ex + pbDelta, ey) (-vx, vy) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (-blockSize/2 - eSize/2) + (bx - ex)

rightCollisionBlockEnemy block enemy 
    |    (ex - esize <= bx + bsize) -- (2)
      && (ey - esize <= by + bsize) -- (1)
      && (ey + esize >= by - bsize) -- (5)
      && not (ey - eSize/2 + 2 >= by + bsize) -- not (4)
      && not (ex <= bx ) -- not (3)
        = Enemy eType (ex + pbDelta, ey) (-vx, vy) eSize
    | otherwise = enemy
  where
    Block _ (bx, by)  = block
    Enemy eType (ex, ey) (vx, vy) eSize = enemy
    esize = (eSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (eSize/2 + blockSize/2) - (ex - bx)
{-
rightCollisionBlockPlayer block player
    |    (px - psize <= bx + bsize) -- (2)
      && (py - psize <= by + bsize) -- (1)
      && (py + psize >= by - bsize) -- (5)
      && not (py - psize >= by + bsize) -- not (4)
      && not (px <= bx ) -- not (3)
        = Player (px + pbDelta, py) (0, vy) jump pSize
        
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump pSize = player
    psize = (playerSize - 1) / 2
    bsize = (blockSize - 1) / 2
    pbDelta = (playerSize/2 + blockSize/2) - (px - bx) -}   
    
upCollisionEntityEnemy enemy entity = enemy
downCollisionEntityEnemy enemy entity = enemy
leftCollisionEntityEnemy enemy entity = enemy
rightCollisionEntityEnemy enemy entity = enemy
