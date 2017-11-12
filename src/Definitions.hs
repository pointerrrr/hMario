module Definitions where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

class Show a => Entity a where
    location  :: a -> Point
    direction :: a -> Vector
    move :: Float -> a ->  a
    canJump :: a -> Bool
    size :: a -> Float
    upCollisionBlock :: Block -> a -> a
    downCollisionBlock :: Block -> a -> a
    leftCollisionBlock :: Block -> a -> a
    rightCollisionBlock :: Block -> a -> a
    upCollisionEntity :: Entity b => a -> b -> a
    downCollisionEntity :: Entity b => a -> b -> a
    leftCollisionEntity :: Entity b => a -> b -> a
    rightCollisionEntity :: Entity b => a -> b -> a

-- | Data describing the state of the game.
data GameState = Game
    { player :: Player
    , enemies :: [Enemy]
    , blocks :: [Block]
    , pressedKeys :: PressedKeys
    } deriving Show

data PressedKeys = PressedKeys
    { upKey :: Bool
    , downKey :: Bool
    , leftKey :: Bool
    , rightKey :: Bool
    , xKey :: Bool
    , zKey :: Bool
    , pKey :: Bool
    } deriving Show

data Player = Player Point Vector Bool Float
    deriving Show

data Enemy = Enemy EnemyType Point Vector Float
    deriving Show

data EnemyType = Goomba | Koopa | PiranhaPlant | BulletBill deriving Show

data Block = Block BlockType Point deriving Show

data BlockType = Stone | Brick | Coin | PowerUp deriving Show
