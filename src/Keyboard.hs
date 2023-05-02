
module Keyboard where

import SDL

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

type Keyboard = Set Keycode

data MouseState = MouseState
  { leftButton :: Bool
  , rightButton :: Bool
  , mouseX :: Int
  , mouseY :: Int
  } deriving (Show, Eq)


-- | création de la structure d'état de clavier (vide)
createKeyboard :: Keyboard
createKeyboard = S.empty

createMouseState :: MouseState
createMouseState = MouseState False False 0 0


handleEvent :: Event -> Keyboard -> Keyboard
handleEvent event kbd =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then S.insert (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
      else if keyboardEventKeyMotion keyboardEvent == Released
           then S.delete (keysymKeycode (keyboardEventKeysym keyboardEvent)) kbd
           else kbd
    _ -> kbd

handleMouseEvent :: Event -> MouseState -> MouseState
handleMouseEvent event mouseState =
  case eventPayload event of
    MouseButtonEvent e ->
      let button = mouseButtonEventButton e
          motion = mouseButtonEventMotion e
          P (V2 x y) = mouseButtonEventPos e
          pos = V2 (fromIntegral x) (fromIntegral y) :: V2 Int
      in if button == ButtonLeft
         then mouseState { leftButton = motion == Pressed
                         , mouseX = getComponentX pos
                         , mouseY = getComponentY pos
                         }
         else mouseState
    _ -> mouseState

leftButtonPressed :: MouseState -> MouseState -> Bool
leftButtonPressed oldState newState =
  (not $ leftButton oldState) && leftButton newState


getComponentX :: V2 Int -> Int
getComponentX (V2 x _) = x

getComponentY :: V2 Int -> Int
getComponentY (V2 _ y) = y

-- | prise en compte des événements SDL2 pour mettre à jour l'état du clavier
--handleEvents :: [Event] -> Keyboard -> Keyboard
--handleEvents events kbd = foldl' (flip handleEvent) kbd events

handleEvents :: [Event] -> (Keyboard, MouseState) -> (Keyboard, MouseState)
handleEvents events (kbd, mouseState) =
  let kbd' = foldl' (flip handleEvent) kbd events
      mouseState' = foldl' (flip handleMouseEvent) mouseState events
  in (kbd', mouseState')

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
