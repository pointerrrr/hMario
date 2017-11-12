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
--      (*) (py + psize >= by - bsize)
--    2. playerRight not left to blockLeft
--      (*) (px + psize >= bx - bsize)
--    3. playerLeft not right to blockRight
--      (*) (px - psize <= bx + bsize)
--    4. playerTop not under blockTop
--      (*) (py - psize < by - bsize)
--    5. playerTop not under blockBottom
--      (*) (py - psize <= by + bsize)
--    6. playerBottom not above blockBottom
--      (*) (py + psize >= by + bsize)

-- | Up and down are somehow switched, dus upCollision has jump = False (temp)
upCollisionBlockPlayer block player
    |    (py + psize >= by - bsize)     -- (1)
      && (px + psize >= bx - bsize)     -- (2)
      && (px - psize <= bx + bsize)     -- (3)
      && (py - psize < by - bsize)      -- (4)
        = Player (px, by - bsize - psize) (vx, 0) False
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) _ = player
    psize = (playerSize + 1) / 2
    bsize = (blockSize + 1) / 2

-- | Up/down switched, thus here jump = True (for now)
downCollisionBlockPlayer block player
    |    (py - psize <= by + bsize) -- (5)
      && (px + psize >= bx - bsize) -- (2)
      && (px - psize <= bx + bsize) -- (3)
      && (py + psize >= by + bsize) -- (6)
        = Player (px, by + bsize + psize) (vx, 0) True
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump = player
    psize = (playerSize + 1) / 2
    bsize = (blockSize + 1) / 2

-- | Seems to find the right collision, but move player like downCollision
leftCollisionBlockPlayer block player
    |    (px + psize >= bx - bsize) -- (2)
      && (py + psize <= by - bsize) -- (1)
      && (py - psize >= by + bsize) -- (5)
        = Player (bx - bsize + psize, py) (0, vy) jump
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump = player
    psize = (playerSize + 1) / 2
    bsize = (blockSize + 1) / 2

-- | Seems to find the right collision, but move player like downCollision
rightCollisionBlockPlayer block player
    |    (px - psize <= bx + bsize) -- (3)
      && (py + psize <= by - bsize) -- (1)
      && (py - psize >= by + bsize) -- (5)
        = Player (bx + bsize + psize, py) (0, vy) jump
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump = player
    psize = (playerSize + 1) / 2
    bsize = (blockSize + 1) / 2

upCollisionEntityPlayer player entity = undefined
downCollisionEntityPlayer player entity = undefined
leftCollisionEntityPlayer player entity = undefined
rightCollisionEntityPlayer player entity = undefined

-- Enemy collisions
upCollisionBlockEnemy       :: Block -> Enemy -> Enemy
downCollisionBlockEnemy     :: Block -> Enemy -> Enemy
leftCollisionBlockEnemy     :: Block -> Enemy -> Enemy
rightCollisionBlockEnemy    :: Block -> Enemy -> Enemy
upCollisionEntityEnemy      :: Entity a => Enemy -> a -> Enemy
downCollisionEntityEnemy    :: Entity a => Enemy -> a -> Enemy
leftCollisionEntityEnemy    :: Entity a => Enemy -> a -> Enemy
rightCollisionEntityEnemy   :: Entity a => Enemy -> a -> Enemy

upCollisionBlockEnemy block enemy = undefined
downCollisionBlockEnemy block enemy = undefined
leftCollisionBlockEnemy block enemy = undefined
rightCollisionBlockEnemy block enemy = undefined
upCollisionEntityEnemy enemy entity = undefined
downCollisionEntityEnemy enemy entity = undefined
leftCollisionEntityEnemy enemy entity = undefined
rightCollisionEntityEnemy enemy entity = undefined
