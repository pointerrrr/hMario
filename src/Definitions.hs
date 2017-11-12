module Definitions where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

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

data Player = Player Point Vector Bool
    deriving Show

data Enemy = Enemy EnemyType Point Vector
    deriving Show

data EnemyType = Goomba | Koopa | PiranhaPlant | BulletBill deriving Show

data Block = Block BlockType Point deriving Show

data BlockType = Stone | Brick | Coin | PowerUp deriving Show
