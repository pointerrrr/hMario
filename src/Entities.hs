module Entities where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Definitions

class Show a => Entity a where
    location  :: a -> Point
    direction :: a -> Vector
    move :: Float -> a ->  a
    canJump :: a -> Bool
    upCollisionBlock :: Block -> a -> a
    downCollisionBlock :: Block -> a -> a
    leftCollisionBlock :: Block -> a -> a
    rightCollisionBlock :: Block -> a -> a
    upCollisionEntity :: a -> b -> a
    downCollisionEntity :: a -> b -> a
    leftCollisionEntity :: a -> b -> a
    rightCollisionEntity :: a -> b -> a

instance Entity Player where
    location (Player pos _ _)  = pos
    direction (Player _ dir _) = dir
    move = movePlayer
    canJump (Player _ _ can) = can
    upCollisionBlock block player = undefined
    downCollisionBlock block player = undefined
    leftCollisionBlock block player = undefined
    rightCollisionBlock block player = undefined
    upCollisionEntity player entity = undefined
    downCollisionEntity player entity = undefined
    leftCollisionEntity player entity = undefined
    rightCollisionEntity player entity = undefined

instance Entity Enemy where
    location (Enemy _ pos _) = pos
    direction (Enemy _ _ dir) = dir
    move = moveEnemy
    canJump _ = False
    upCollisionBlock block enemy = undefined
    downCollisionBlock block enemy = undefined
    leftCollisionBlock block enemy = undefined
    rightCollisionBlock block enemy = undefined
    upCollisionEntity enemy entity = undefined
    downCollisionEntity enemy entity = undefined
    leftCollisionEntity enemy entity = undefined
    rightCollisionEntity enemy entity = undefined

movePlayer :: Float -> Player -> Player
movePlayer seconds (Player pos dir jump) =
    Player (movePoint seconds pos dir) dir jump

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy a enemy = enemy

movePoint :: Float -> Point -> Vector -> Point
movePoint seconds (x,y) (vx, vy) = (x + vx * seconds, y + vy * seconds)
