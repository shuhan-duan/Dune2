module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K
import Common
import Unite

data GameState = GameState
    { commands :: [Commande]
    , envi :: Environnement
    , currentPlayer :: Int
    , turnNumber :: Int
    , playerCredits :: Int
     }
  deriving (Show)


initGameState :: Environnement -> GameState
initGameState ienv = GameState
  { commands = []
  , envi = ienv
  , currentPlayer = 1
  , turnNumber = 1
  , playerCredits = 0
  }

-- gameStep :: GameState -> Keyboard -> Float -> GameState
-- gameStep gameState kbd deltaTime =
--   let env' = tourDeJeu (env gameState)
--       cmds = handleCommands kbd (commands gameState) env'
--   in gameState { env = env', commands = cmds }

-- handleCommands :: Keyboard -> [Commande] -> Environnement -> [Commande]
-- handleCommands kbd cmds env =
  

