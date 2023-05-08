
module Keyboard where

import SDL

import Data.Set (Set)
import qualified Data.Set as S

type Keyboard = Set Keycode

data MouseState = MouseState
  { leftButton :: Bool
  , rightButton :: Bool
  , mouseX :: Int
  , mouseY :: Int
  , clickedInMenu :: Bool
  } deriving (Show, Eq)


-- | création de la structure d'état de clavier (vide)
createKeyboard :: Keyboard
createKeyboard = S.empty

createMouseState :: MouseState
createMouseState = MouseState False False 0 0 False


-- handle keyboard events for commands or selecting buildings/units with shortcut keys
handleKeyboardEvent :: Event -> Keyboard -> Keyboard
handleKeyboardEvent event kbd =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then S.insert (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
      else if keyboardEventKeyMotion keyboardEvent == Released
           then S.delete (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
           else kbd
    _ -> kbd

-- | quelques noms de *keycode*
keycodeName :: Keycode -> Char
keycodeName KeycodeA = 'a'
keycodeName KeycodeB = 'b'
keycodeName KeycodeC = 'c'
keycodeName KeycodeD = 'd'
keycodeName KeycodeE = 'e'
keycodeName KeycodeF = 'f'
keycodeName KeycodeG = 'g'
keycodeName KeycodeH = 'h'
keycodeName KeycodeI = 'i'
keycodeName KeycodeJ = 'j'
keycodeName KeycodeK = 'k'
keycodeName KeycodeL = 'l'
keycodeName KeycodeM = 'm'
keycodeName KeycodeN = 'n'
keycodeName KeycodeO = 'o'
keycodeName KeycodeP = 'p'
keycodeName KeycodeQ = 'q'
keycodeName KeycodeR = 'r'
keycodeName KeycodeS = 's'
keycodeName KeycodeT = 't'
keycodeName KeycodeU = 'u'
keycodeName KeycodeV = 'v'
keycodeName KeycodeW = 'w'
keycodeName KeycodeX = 'x'
keycodeName KeycodeY = 'y'
keycodeName KeycodeZ = 'z'
keycodeName _ = '-'

-- | Vérifies sir le *keycode* spécificé est actuellement
-- | actif sur le clavier.
keypressed :: Keycode -> Keyboard -> Bool
keypressed kc kbd = S.member kc kbd

leftButtonPressed :: MouseState -> MouseState -> Bool
leftButtonPressed oldState newState =
  (not $ leftButton oldState) && leftButton newState

getComponentX :: V2 Int -> Int
getComponentX (V2 x _) = x

getComponentY :: V2 Int -> Int
getComponentY (V2 _ y) = y

isClickInMenuArea :: MouseState -> Bool
isClickInMenuArea mouseState =
  let x = mouseX mouseState
      y = mouseY mouseState
  in x >= 640 && x <= 840 && y >= 200 && y <= 480

isPointInRect :: V2 Int -> SDL.Rectangle Int -> Bool
isPointInRect (V2 x y) (SDL.Rectangle (SDL.P (V2 rx ry)) (V2 rw rh)) =
  x >= rx && x <= rx + rw && y >= ry && y <= ry + rh
