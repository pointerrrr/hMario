module Entities where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Collision
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
