{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (unless,when)
import Control.Concurrent (threadDelay)

import Data.Maybe (listToMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

--import Keyboard (Keyboard)
import Keyboard (Keyboard, MouseState)

import qualified Keyboard as K


import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
  -- initialisation de l'état du jeu
  let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  let mouseState = K.createMouseState

  -- lancement de la gameLoop
  --gameLoop 60 renderer tmap' smap' kbd gameState
  gameLoop 60 renderer tmap' smap' (kbd, mouseState) gameState


--gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> IO ()
--gameLoop frameRate renderer tmap smap kbd gameState = do
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> (Keyboard, MouseState) -> GameState -> IO ()
gameLoop frameRate renderer tmap smap (kbd, mouseState) gameState = do
  startTime <- time
  events <- pollEvents
  let payloads = map eventPayload events
  --let kbd' = K.handleEvents events kbd
  let (kbd', mouseState') = K.handleEvents events (kbd, mouseState)

  let quit = any (\case WindowClosedEvent{} -> True; _ -> False) payloads || K.keypressed KeycodeEscape kbd'
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState)))
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState kbd' deltaTime
  let sprite = S.moveTo (SM.fetchSprite (SpriteId "perso") smap) (fromIntegral (M.persoX gameState')) (fromIntegral (M.persoY gameState'))
  let V2 px py = S.position sprite
  let V2 w h = S.size sprite
  let inX = K.mouseX mouseState' >= fromIntegral px && K.mouseX mouseState' <= fromIntegral (px + w)
  let inY = K.mouseY mouseState' >= fromIntegral py && K.mouseY mouseState' <= fromIntegral (py + h)
  let hit = K.leftButtonPressed mouseState mouseState' && inX && inY

  when hit (putStrLn "Touché !")
  ---
  
  --unless quit (gameLoop frameRate renderer tmap smap kbd' gameState')
  unless quit (gameLoop frameRate renderer tmap smap (kbd', mouseState') gameState')
  

