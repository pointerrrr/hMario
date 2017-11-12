module Entities where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Collision
import Definitions

instance Entity Player where
    location (Player pos _ _ _)  = pos
    direction (Player _ dir _ _) = dir
    move = movePlayer
    canJump (Player _ _ can _) = can
    size (Player _ _ _ pSize) = pSize
    upCollisionBlock        = upCollisionBlockPlayer
    downCollisionBlock      = downCollisionBlockPlayer
    leftCollisionBlock      = leftCollisionBlockPlayer
    rightCollisionBlock     = rightCollisionBlockPlayer
    upCollisionEntity       = upCollisionEntityPlayer
    downCollisionEntity     = downCollisionEntityPlayer
    leftCollisionEntity     = leftCollisionEntityPlayer
    rightCollisionEntity    = rightCollisionEntityPlayer

instance Entity Enemy where
    location (Enemy _ pos _ _) = pos
    direction (Enemy _ _ dir _) = dir
    move = moveEnemy
    canJump _ = False
    size (Enemy _ _ _ eSize) = eSize
    upCollisionBlock        = upCollisionBlockEnemy
    downCollisionBlock      = downCollisionBlockEnemy
    leftCollisionBlock      = leftCollisionBlockEnemy
    rightCollisionBlock     = rightCollisionBlockEnemy
    upCollisionEntity       = upCollisionEntityEnemy
    downCollisionEntity     = downCollisionEntityEnemy
    leftCollisionEntity     = leftCollisionEntityEnemy
    rightCollisionEntity    = rightCollisionEntityEnemy

movePlayer :: Float -> Player -> Player
movePlayer seconds (Player pos dir jump pSize) =
    Player (movePoint seconds pos dir) dir jump pSize

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy seconds (Enemy eType pos dir eSize) = Enemy eType (movePoint seconds pos dir) dir eSize

movePoint :: Float -> Point -> Vector -> Point
movePoint seconds (x,y) (vx, vy) = (x + vx * seconds, y + vy * seconds)
