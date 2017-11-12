module Definitions where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import System.Random

class Show a => Entity a where
    location  :: a -> Point
    direction :: a -> Vector
    move :: Float -> a ->  a
    canJump :: a -> Bool
    size :: a -> Float
    state :: a -> State
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
    , projectiles :: [Projectile]
    , blocks :: [Block]
    , generator :: StdGen
    , score :: Int
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

data Player = Player Point Vector Bool Float State
    deriving Show

data Enemy = Enemy EnemyType Point Vector Float State
    deriving Show

data State = Alive | Dying1 | Dying2 | Dying3 | Dying4 | Dying5 | Dying6 | Dying7 | Dying8 | Dying9 | Dying10 | Dying11 | Dying12 | Dying13 | Dying14 | Dying15 | Dying16 | Dying17 | Dying18 | Dying19 | Dead deriving (Show, Eq, Ord, Enum)
    
data EnemyType = Goomba | Koopa | PiranhaPlant | BulletBill deriving Show

data Projectile = Projectile Point Vector Float deriving Show

pLoc :: Projectile -> Point
pLoc (Projectile loc _ _) = loc

data Block = Block BlockType Point deriving Show

data BlockType = Stone | Brick | Coin | PowerUp deriving Show
