{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Control.Monad (unless,forM_,foldM)

import Foreign.C.Types (CInt (..) )

import SDL

import qualified TextureMap as TM

import qualified Sprite as S

import qualified SpriteMap as SM

--import Keyboard (Keyboard)

import Carte
import Data.Word (Word8)
import qualified Data.Map as Map
import System.Random (newStdGen)
import Common
import TextureMap
import SpriteMap
import Batiment
import Model
import SDL.Font 
import qualified SDL.Font as Font
import qualified Data.Text as T


-- Define the colors for each terrain type.
herbeColor :: V4 Word8
herbeColor = V4 0 255 0 255  -- Green

ressourceColor :: V4 Word8
ressourceColor = V4 205 133 63 255  --Earth color

eauColor :: V4 Word8
eauColor = V4 0 191 255 255

-- Draw the terrain given its type and position.
drawTerrain :: SDL.Renderer -> Terrain -> SDL.Rectangle CInt -> IO ()
drawTerrain renderer terrain rect = do
  let color = case terrain of
        Herbe -> herbeColor
        Ressource _ -> ressourceColor
        Eau -> eauColor
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.fillRect renderer (Just rect)

drawMap :: SDL.Renderer -> Carte -> Int -> Int -> IO ()
drawMap renderer (Carte m) tileWidth tileHeight = do
  let coords = Map.keys m
  forM_ coords $ \coord@(C x y) -> do
    let terrain = m Map.! coord
    let rect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (x * tileWidth)) (fromIntegral (y * tileHeight)))) (SDL.V2 (fromIntegral tileWidth) (fromIntegral tileHeight))
    drawTerrain renderer terrain rect

loadEntite :: Renderer -> TextureMap -> SpriteMap -> Environnement -> IO (TextureMap, SpriteMap)
loadEntite renderer  tmap smap env = do
  let unitesList = Map.elems (unites env)
      batimentsList = Map.elems (batiments env)

  (tmap', smap') <- foldM (loadUnite renderer ) (tmap, smap) unitesList
  foldM (loadBatiment renderer ) (tmap', smap') batimentsList

loadUnite :: Renderer -> (TextureMap, SpriteMap) -> Unite -> IO (TextureMap, SpriteMap)
loadUnite renderer  (tmap, smap) unite = do
  let textureId = getTextureIdForUnite unite
      spriteId = getSpriteIdForUnite unite
      filePath = getFilePathForUnite (utype unite)

  tmap' <- TM.loadTexture renderer filePath textureId tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage textureId (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite spriteId sprite smap

  return (tmap', smap')

loadBatiment :: Renderer  -> (TextureMap, SpriteMap) -> Batiment -> IO (TextureMap, SpriteMap)
loadBatiment renderer  (tmap, smap) batiment = do
  let textureId = getTextureIdForBatiment batiment
      spriteId = getSpriteIdForBatiment batiment
      filePath = getFilePathForBatiment (btype batiment)

  tmap' <- TM.loadTexture renderer filePath textureId tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage textureId (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite spriteId sprite smap

  
  return (tmap', smap')


drawInfoAndMenu :: Renderer -> GameState -> Font -> IO ()
drawInfoAndMenu renderer gameState font = do
  -- Draw the info area background
  let infoArea = SDL.Rectangle (SDL.P (SDL.V2 640 0)) (SDL.V2 200 200)
  SDL.rendererDrawColor renderer SDL.$= V4 200 200 200 255
  SDL.fillRect renderer (Just infoArea)

  -- Draw the menu area background
  let menuArea = SDL.Rectangle (SDL.P (SDL.V2 640 200)) (SDL.V2 200 280)
  SDL.rendererDrawColor renderer SDL.$= V4 150 150 150 255
  SDL.fillRect renderer (Just menuArea)

  -- Draw the text for the active player and their credit balance
  let activePlayer = "Player: " ++ (show $ currentPlayer gameState)
  let creditBalance = "Credits: " ++ (show $ playerCredits gameState)

  let textColor = V4 0 0 0 255 :: V4 Word8
  textSurface1 <- Font.solid font textColor (T.pack activePlayer)
  textTexture1 <- SDL.createTextureFromSurface renderer textSurface1
  SDL.freeSurface textSurface1

  textSurface2 <- Font.solid font textColor (T.pack creditBalance)
  textTexture2 <- SDL.createTextureFromSurface renderer textSurface2
  SDL.freeSurface textSurface2

  let srcRect = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 200 20)
  let dstRect1 = SDL.Rectangle (SDL.P (SDL.V2 650 10)) (SDL.V2 200 20)
  let dstRect2 = SDL.Rectangle (SDL.P (SDL.V2 650 30)) (SDL.V2 200 20)

  SDL.copy renderer textTexture1 (Just srcRect) (Just dstRect1)
  SDL.copy renderer textTexture2 (Just srcRect) (Just dstRect2)

  SDL.destroyTexture textTexture1
  SDL.destroyTexture textTexture2

appLoop :: Renderer -> TextureMap -> SpriteMap -> GameState -> Font -> IO ()
appLoop renderer tmap smap gameState font = do
  let env = envi gameState
  events <- pollEvents
  let quit = any isQuitEvent events
  unless quit $ do
    clear renderer
    drawEnv renderer tmap smap gameState font
    present renderer
    appLoop renderer tmap smap gameState font
  where
    isQuitEvent e = case eventPayload e of
      WindowClosedEvent{} -> True
      _                   -> False

drawEnv :: Renderer -> TextureMap -> SpriteMap -> GameState -> Font -> IO ()
drawEnv renderer tmap smap gameState font = do
  let env = envi gameState 
  -- Draw the map
  drawMap renderer (ecarte env) 32 32

  -- Draw units
  forM_ (Map.elems (unites env)) $ \unite -> do
    let spriteId = getSpriteIdForUnite unite
    let (C x y) = ucoord unite
    let screenPosX = fromIntegral x * 32
    let screenPosY = fromIntegral y * 32
    let sprite = S.moveTo (SM.fetchSprite spriteId smap) screenPosX screenPosY
    S.displaySprite renderer tmap sprite

  -- Draw buildings
  forM_ (Map.elems (batiments env)) $ \batiment -> do
    
    let spriteId = getSpriteIdForBatiment batiment
    let (C x y) = bcoord batiment
    let screenPosX = fromIntegral x * 32
    let screenPosY = fromIntegral y * 32
    
    let sprite = S.moveTo (SM.fetchSprite spriteId smap) screenPosX screenPosY
    S.displaySprite renderer tmap sprite

  drawInfoAndMenu renderer gameState font



main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize
  font <- Font.load "assets/Freedom-10eM.ttf" 16
  gen <- newStdGen
  let rows = 20
  let cols = 20
  let tileSize = 32
  let infoMenuWidth = 200
  let myMap = generateRandomMap rows cols gen
  let windowSize = SDL.V2 (fromIntegral (cols * tileSize + infoMenuWidth)) (fromIntegral (rows * tileSize))
  let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
  window <- SDL.createWindow (T.pack "Duel 2") windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  
  -- Initialize empty TextureMap and SpriteMap
  let tmap = TM.createTextureMap 
  let smap = SM.createSpriteMap
  
  -- Generate initial player positions
  let numPlayers = 4
  let initialPositions = generateInitialPlayerPositions numPlayers myMap gen
  --let initialPositions = [(C 5 5), (C 10 10), (C 20 20), (C 25 25)]

  -- Create an initial Environnement using creerEnvironnement
  let env = creerEnvironnement myMap initialPositions
  let gameState = initGameState env
  
  -- Load entities into the TextureMap and SpriteMap
  (tmap1, smap2) <- loadEntite renderer tmap smap env
  
  -- Run the app loop with the updated TextureMap and SpriteMap
  appLoop renderer tmap1 smap2 gameState font

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

