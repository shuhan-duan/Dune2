{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Control.Monad (unless,forM_,foldM)

import Foreign.C.Types (CInt (..) )

import SDL

import qualified TextureMap as TM

import qualified Sprite as S

import qualified SpriteMap as SM

import Keyboard
import qualified Keyboard as K
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

loadEntites :: Renderer -> TextureMap -> SpriteMap -> Environnement -> IO (TextureMap, SpriteMap)
loadEntites renderer  tmap smap env = do
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
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage textureId (S.mkArea 0 0 32 32)
  let smap' = SM.addSprite spriteId sprite smap

  return (tmap', smap')

unloadEntites :: TextureMap -> SpriteMap -> Environnement -> Environnement -> IO (TextureMap, SpriteMap)
unloadEntites tmap smap oldEnv newEnv = do
  let oldUnitesList = Map.elems (unites oldEnv)
      newUnitesList = Map.elems (unites newEnv)
      oldBatimentsList = Map.elems (batiments oldEnv)
      newBatimentsList = Map.elems (batiments newEnv)

  let removedUnites = filter (`notElem` newUnitesList) oldUnitesList
  let removedBatiments = filter (`notElem` newBatimentsList) oldBatimentsList

  let smap' = foldr (SM.removeSprite . getSpriteIdForUnite) smap removedUnites
  let smap'' = foldr (SM.removeSprite . getSpriteIdForBatiment) smap' removedBatiments

  -- For now, we just return the original TextureMap without changes
  return (tmap, smap'')

loadEntite :: Renderer -> (TextureMap, SpriteMap) -> Entite -> IO (TextureMap, SpriteMap)
loadEntite renderer (tmap, smap) entity = do
  case entity of
    Left batiment -> loadBatiment renderer (tmap, smap) batiment
    Right unite -> loadUnite renderer (tmap, smap) unite

  

unloadEntite :: TextureMap -> SpriteMap -> Entite -> IO (TextureMap, SpriteMap)
unloadEntite tmap smap entity = do
  let spriteId = case entity of
        Left batiment -> getSpriteIdForBatiment batiment
        Right unite -> getSpriteIdForUnite unite

  -- For now, we just return the original TextureMap without changes
  let smap' = SM.removeSprite spriteId smap
  return (tmap, smap')


loadBatiment :: Renderer  -> (TextureMap, SpriteMap) -> Batiment -> IO (TextureMap, SpriteMap)
loadBatiment renderer  (tmap, smap) batiment = do
  let textureId = getTextureIdForBatiment batiment
      spriteId = getSpriteIdForBatiment batiment
      filePath = getFilePathForBatiment (btype batiment)

  tmap' <- TM.loadTexture renderer filePath textureId tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage textureId (S.mkArea 0 0 32 32)
  let smap' = SM.addSprite spriteId sprite smap

  
  return (tmap', smap')

getInfoAreaData :: GameState -> (T.Text, T.Text, T.Text)
getInfoAreaData gameState =
  let health = case selected gameState of
                 Just entityId -> getEntityHealth gameState entityId
                 Nothing       -> 0

      currentPlayer = getEntiteJoueur gameState <$> selected gameState
      playerCredits = maybe 0 (getJoueurCredit gameState) currentPlayer
      currentPlayerId = case currentPlayer of 
                          Just jid -> unJoueurId jid
                          Nothing  -> 0
      
      activePlayerText = T.concat [T.pack "Player: ", T.pack (show currentPlayerId)]
      creditBalanceText = T.concat [T.pack "Credits: ", T.pack (show playerCredits)]
      healthText = T.concat [T.pack "Points de vie: ", T.pack (show health)]

  in (activePlayerText, creditBalanceText, healthText)
  
drawText :: Renderer -> Font -> V4 Word8 -> T.Text -> SDL.Rectangle CInt -> IO ()
drawText renderer font textColor text rect = do
  textSurface <- Font.solid font textColor text
  textTexture <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  SDL.copy renderer textTexture Nothing (Just rect)
  SDL.destroyTexture textTexture

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

  let (activePlayer, creditBalance, pointDeVie) = getInfoAreaData gameState

  let textColor = V4 0 0 0 255 :: V4 Word8

  drawText renderer font textColor activePlayer (SDL.Rectangle (SDL.P (SDL.V2 650 30)) (SDL.V2 150 20))
  drawText renderer font textColor creditBalance (SDL.Rectangle (SDL.P (SDL.V2 650 70)) (SDL.V2 150 20))
  drawText renderer font textColor pointDeVie (SDL.Rectangle (SDL.P (SDL.V2 650 110)) (SDL.V2 150 20))


appLoop :: Renderer -> TextureMap -> SpriteMap -> GameState -> Font -> Keyboard -> MouseState -> IO ()
appLoop renderer tmap smap gameState font kbd mouseState = do
  events <- pollEvents
  startTime <- time
  let quit = any isQuitEvent events
  let (kbd', mouseState') = handleEvents events (kbd, mouseState)

  unless quit $ do
    clear renderer
    drawEnv renderer tmap smap gameState font
    present renderer
    endTime <- time
    -- let deltaTime = endTime - startTime
    let deltaTime = 2
    let gameState' = gameStep gameState kbd' mouseState' deltaTime
    -- get update entite

    appLoop renderer tmap smap gameState' font kbd' mouseState'
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
    let (screenPosX, screenPosY) = entiePos (Right unite) 
    let sprite = S.moveTo (SM.fetchSprite spriteId smap) (fromIntegral screenPosX) (fromIntegral screenPosY) -- Convert to CInt
    S.displaySprite renderer tmap sprite

  -- Draw buildings
  forM_ (Map.elems (batiments env)) $ \batiment -> do
    let spriteId = getSpriteIdForBatiment batiment
    let (screenPosX, screenPosY) = entiePos (Left batiment) 
    let sprite = S.moveTo (SM.fetchSprite spriteId smap) (fromIntegral screenPosX) (fromIntegral screenPosY) -- Convert to CInt
    S.displaySprite renderer tmap sprite

  drawInfoAndMenu renderer gameState font

  

main :: IO ()
main = do
  initializeAll
  SDL.Font.initialize
  font <- Font.load "assets/OpenSans-Regular.ttf" 40
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
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  let mouseState = K.createMouseState

  -- Load entities into the TextureMap and SpriteMap
  (tmap1, smap2) <- loadEntites renderer tmap smap env
  
  -- Run the app loop with the updated TextureMap and SpriteMap
  appLoop renderer tmap1 smap2 gameState font kbd mouseState

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Font.quit
  SDL.quit

