module KeyHandling where

import Graphics.Gloss.Interface.Pure.Game

import Definitions

-- | Respond to key events.
checkKeys :: Event -> GameState -> GameState
-- For an 's' keypress, reset the ball to the center.
-- right
checkKeys (EventKey (SpecialKey KeyRight) Down _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { rightKey = True }}
checkKeys (EventKey (SpecialKey KeyRight) Up _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { rightKey = False }}
-- left
checkKeys (EventKey (SpecialKey KeyLeft) Down _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { leftKey = True }}
checkKeys (EventKey (SpecialKey KeyLeft) Up _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { leftKey = False }}
-- up
checkKeys (EventKey (SpecialKey KeyUp) Down _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { upKey = True }}
checkKeys (EventKey (SpecialKey KeyUp) Up _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { upKey = False }}
-- down
checkKeys (EventKey (SpecialKey KeyDown) Down _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { downKey = True }}
checkKeys (EventKey (SpecialKey KeyDown) Up _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { downKey = False }}
-- z
checkKeys (EventKey (Char 'z') Down _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { zKey = True }}
checkKeys (EventKey (Char 'z') Up _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { zKey = False }}
-- x
checkKeys (EventKey (Char 'x') Down _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { xKey = True }}
checkKeys (EventKey (Char 'x') Up _ _)
    game@(Game _ _ _ _ _ _ keys) =
        game { pressedKeys = keys { xKey = False }}
-- p
checkKeys (EventKey (Char 'p') Down _ _) game@(Game _ _ _ _ _ _ keys) =
    game { pressedKeys = keys { pKey = not $ pKey keys }}
-- Do nothing for all other events.
checkKeys _ game = game
