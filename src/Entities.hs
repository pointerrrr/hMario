module Entities where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Constants
import Definitions

instance Entity Player where
    location (Player pos _ _)  = pos
    direction (Player _ dir _) = dir
    move = movePlayer
    canJump (Player _ _ can) = can
    upCollisionBlock        = upCollisionBlockPlayer
    downCollisionBlock      = downCollisionBlockPlayer
    leftCollisionBlock      = leftCollisionBlockPlayer
    rightCollisionBlock     = rightCollisionBlockPlayer
    upCollisionEntity       = upCollisionEntityPlayer
    downCollisionEntity     = downCollisionEntityPlayer
    leftCollisionEntity     = leftCollisionEntityPlayer
    rightCollisionEntity    = rightCollisionEntityPlayer

instance Entity Enemy where
    location (Enemy _ pos _) = pos
    direction (Enemy _ _ dir) = dir
    move = moveEnemy
    canJump _ = False
    upCollisionBlock        = upCollisionBlockEnemy
    downCollisionBlock      = downCollisionBlockEnemy
    leftCollisionBlock      = leftCollisionBlockEnemy
    rightCollisionBlock     = rightCollisionBlockEnemy
    upCollisionEntity       = upCollisionEntityEnemy
    downCollisionEntity     = downCollisionEntityEnemy
    leftCollisionEntity     = leftCollisionEntityEnemy
    rightCollisionEntity    = rightCollisionEntityEnemy

movePlayer :: Float -> Player -> Player
movePlayer seconds (Player pos dir jump) =
    Player (movePoint seconds pos dir) dir jump

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy a enemy = enemy

movePoint :: Float -> Point -> Vector -> Point
movePoint seconds (x,y) (vx, vy) = (x + vx * seconds, y + vy * seconds)

-- Collisions

-- Player collisions
upCollisionBlockPlayer      :: Block -> Player -> Player
downCollisionBlockPlayer    :: Block -> Player -> Player
leftCollisionBlockPlayer    :: Block -> Player -> Player
rightCollisionBlockPlayer   :: Block -> Player -> Player
upCollisionEntityPlayer     :: Entity a => Player -> a -> Player
downCollisionEntityPlayer   :: Entity a => Player -> a -> Player
leftCollisionEntityPlayer   :: Entity a => Player -> a -> Player
rightCollisionEntityPlayer  :: Entity a => Player -> a -> Player

upCollisionBlockPlayer block player
    |    (py + psize <= by - bsize) -- playerBottom not above blockTop
      && (px + psize >= bx - bsize) -- playerRight not left to blockLeft
      && (px - psize <= bx + bsize) -- playerLeft not right to blockRight
      && (py - psize > by + bsize) -- playerTop not under blockTop
        = Player (px, by - bsize - psize - 5) (vx, 0) True
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) _ = player
    psize = playerSize / 2
    bsize = blockSize / 2

downCollisionBlockPlayer block player
    |    (py - psize >= by + bsize) -- playerTop not under blockBottom
      && (px + psize >= bx - bsize) -- playerRight not left to blockLeft
      && (px - psize <= bx + bsize) -- playerLeft not right to blockRight
      && (py + psize >= by + bsize) -- playerBottom not above blockBottom
        = Player (px, by + bsize + psize + 5) (vx, 0) False
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump = player
    psize = playerSize / 2
    bsize = blockSize / 2

leftCollisionBlockPlayer block player
    |    (px + psize >= bx - bsize) -- playerRight not left to blockLeft
      && (py + psize <= by - bsize) -- playerBottom not above blockTop
      && (py - psize >= by + bsize) -- playerTop not under blockBottom
        = Player (bx - bsize + psize, py) (0, vy) jump
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump = player
    psize = playerSize / 2
    bsize = blockSize / 2
rightCollisionBlockPlayer block player
    |    (px - psize <= bx + bsize) -- playerLeft not right to blockLeft
      && (py + psize <= by - bsize) -- playerBottom not above blockTop
      && (py - psize >= by + bsize) -- playerTop not under blockBottom
        = Player (bx + bsize + psize, py) (0, vy) jump
    | otherwise = player
  where
    Block _ (bx, by)  = block
    Player (px, py) (vx, vy) jump = player
    psize = playerSize / 2
    bsize = blockSize / 2

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
