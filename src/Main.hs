module Main(main, GameState(..), initialState, render) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Radius = Float
type Position = (Float, Float)

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Data describing the state of the game.
data GameState = Game
    { ballLoc :: (Float, Float) -- ^ Pong ball (x, y) location.
    , ballVel :: (Float, Float) -- ^ Pong ball (x, y) velocity.
    , player1 :: Float          -- ^ Left player paddle height.
                                -- Zero is the middle of the screen.
    , player2 :: Float          -- ^ Right player paddle height.
    } deriving Show

-- | The starting state of the game.
initialState :: GameState
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (-10, -35)
    , player1 = 0
    , player2 = 0
    }

-- | Convert a game state into a picture.
render :: GameState -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
    pictures [ ball
             , walls
             , mkPaddle rose 120 $ player1 game
             , mkPaddle orange (-120) $ player2 game
             ]
  where
    -- The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $
             circleSolid 10
    ballColor = dark red

    -- The bootom and top walls.
    wall :: Float -> Picture
    wall offset = translate 0 offset $
                    color wallColor $
                      rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    -- Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

    paddleColor = light $ light blue

-- | Update the ball position using its curent velocity
moveBall :: Float       -- ^ The number of seconds since last update.
         -> GameState   -- ^ The initial game state.
         -> GameState   -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Number of frames to show per seconds
fps :: Int
fps = 60

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: ViewPort -> Float -> GameState -> GameState
update _ seconds = paddleBounce . wallBounce . moveBall seconds

-- | Detect a collision with a paddle. Upon collisisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: GameState -> GameState
paddleBounce game = game { ballVel = (vx', vy) }
  where
    -- Radius. Use same thing as in 'render'.
    radius = 10

    -- The old velocities
    (vx, vy) = ballVel game

    vx' = if paddleCollision radius game
            then
                -- Update velocity
                -vx
            else
                -- Do nothing. Return the old velocity
                vx


-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: GameState -> GameState
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in 'render'.
    radius = 10

    -- The old velocities
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
            then
                -- Update the velocity.
                -vy
            else
                -- Do nothing. Return the old velocity
                vy

-- | Given position and radius of the ball, return wether a collision occured.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral height / 2
    bottomCollision = y + radius >= fromIntegral height / 2

-- | Given position and radius of the ball, return wether a collision occured.
paddleCollision :: Radius -> GameState -> Bool
paddleCollision radius game@(Game {player1 = p1, player2 = p2})
    = leftCollision || rightCollision
      where
        (x, y) = ballLoc game
        leftCollision = (x - radius <= -110)
                      && (y <= p1 + 40) && (y >= p1 - 40)
        rightCollision = (x + radius >=  130)
                      && (y <= p2 + 40) && (y >= p2 - 40)


main :: IO ()
main = simulate window background fps initialState render update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState
