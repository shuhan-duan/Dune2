{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Control.Monad (unless,forM_,foldM,when)

import Foreign.C.Types (CInt (..) )

import SDL hiding (windowSize)

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
  case selected gameState of
    Just (Left entityId) ->
      let health = getEntityHealth gameState entityId

          currentPlayer = getEntiteJoueur gameState entityId
          playerCredits = getJoueurCredit gameState currentPlayer
          currentPlayerId = unJoueurId currentPlayer

          activePlayerText = T.concat [T.pack "Player: ", T.pack (show currentPlayerId)]
          creditBalanceText = T.concat [T.pack "Credits: ", T.pack (show playerCredits)]
          healthText = T.concat [T.pack "Points de vie: ", T.pack (show health)]

      in (activePlayerText, creditBalanceText, healthText)

    Just (Right terrain) ->
      let terrainType = T.pack $ show terrain
          resourceAmount = case terrain of
                            Ressource n -> T.pack (show n)
                            _           -> T.pack ""

          terrainText = T.concat [T.pack "Terrain: ", terrainType]
          resourceText = if not (T.null resourceAmount)
                         then T.concat [T.pack "Ressource: ", resourceAmount]
                         else T.pack ""

      in (terrainText, resourceText, T.pack "")

    Nothing -> (T.pack "", T.pack "", T.pack "")
  
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

  when (not $ T.null activePlayer) $
    drawText renderer font textColor activePlayer (SDL.Rectangle (SDL.P (SDL.V2 650 30)) (SDL.V2 150 20))

  when (not $ T.null creditBalance) $
    drawText renderer font textColor creditBalance (SDL.Rectangle (SDL.P (SDL.V2 650 70)) (SDL.V2 150 20))

  when (not $ T.null pointDeVie) $
    drawText renderer font textColor pointDeVie (SDL.Rectangle (SDL.P (SDL.V2 650 110)) (SDL.V2 150 20))

  case selected gameState of
    Just (Left entityId) -> drawMenuItems renderer font textColor gameState entityId
    Just (Right _) -> return ()  -- Ignore the Terrain case
    Nothing -> return ()

-- | Main application loop
appLoop :: Renderer -> TextureMap -> SpriteMap -> GameState -> Font -> Keyboard -> MouseState -> IO ()
appLoop renderer tmap smap gameState font kbd mouseState = do
  events <- pollEvents
  startTime <- time
  let quit = any isQuitEvent events
  let (gameState', kbd', mouseState') = handleEvents events gameState (kbd, mouseState)

  unless quit $ do
    clear renderer
    endTime <- time
    let deltaTime = endTime - startTime
    --let deltaTime = 2
    -- gameStep :: gameStep gstate kbd prevMouseState mouseState deltaTime
    let gameState2 = gameStep gameState' kbd' mouseState mouseState' deltaTime

    -- Update the TextureMap and SpriteMap if necessary
    (tmap', smap') <- updateTSMap renderer (tmap, smap) gameState gameState2

    drawEnv renderer tmap' smap' gameState2 font
    present renderer
    
    
    appLoop renderer tmap' smap' gameState2 font kbd' mouseState'
  where
    isQuitEvent e = case eventPayload e of
      WindowClosedEvent{} -> True
      _                   -> False



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
  let initialPositions = generateInitialPlayerPositions myMap
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

drawText :: Renderer -> Font -> V4 Word8 -> T.Text -> SDL.Rectangle CInt -> IO ()
drawText renderer font textColor text rect = do
  textSurface <- Font.solid font textColor text
  textTexture <- SDL.createTextureFromSurface renderer textSurface
  SDL.freeSurface textSurface
  SDL.copy renderer textTexture Nothing (Just rect)
  SDL.destroyTexture textTexture

drawMenuItems :: Renderer -> Font -> V4 Word8 -> GameState -> EntiteId -> IO ()
drawMenuItems renderer font textColor gameState entityId = do
  let menuItems = getMenuItemsForEntity gameState entityId
  let menuPositions = zipWith (\item index -> SDL.Rectangle (SDL.P (SDL.V2 650 (200 + 30 * index))) (SDL.V2 150 20)) menuItems [0..]
  forM_ (zip menuItems menuPositions) $ \(menuItem, menuPosition) -> do
    drawText renderer font textColor (menuItemToText menuItem) menuPosition

menuItemToText :: MenuItem -> T.Text
menuItemToText menuItem = case menuItem of
  BuildBuilding batType -> T.pack "Build " <> T.pack (show batType)
  ProduceUnit unitType -> T.pack "Produce " <> T.pack (show unitType)
  CollectResources -> T.pack "Collect Resources"
  Attack -> T.pack "Attack"
  Patrol -> T.pack "Patrol"
  Moveto -> T.pack "Moveto"

drawEnv :: Renderer -> TextureMap -> SpriteMap -> GameState -> Font -> IO ()
drawEnv renderer tmap smap gameState font = do
  let env = envi gameState 
  -- Draw the map
  drawMap renderer (ecarte env) 32 32

  -- Draw units
  forM_ (Map.elems (unites env)) $ \unite -> do
    let spriteId = getSpriteIdForUnite unite
    when (Map.member spriteId smap) $ do
      let (screenPosX, screenPosY) = entiePos (Right unite) 
      let sprite = S.moveTo (SM.fetchSprite spriteId smap) (fromIntegral screenPosX) (fromIntegral screenPosY) -- Convert to CInt
      S.displaySprite renderer tmap sprite

  -- Draw buildings
  forM_ (Map.elems (batiments env)) $ \batiment -> do
    let spriteId = getSpriteIdForBatiment batiment
    when (Map.member spriteId smap) $ do
      let (screenPosX, screenPosY) = entiePos (Left batiment) 
      let sprite = S.moveTo (SM.fetchSprite spriteId smap) (fromIntegral screenPosX) (fromIntegral screenPosY) -- Convert to CInt
      S.displaySprite renderer tmap sprite

  drawInfoAndMenu renderer gameState font

updateTSMap :: Renderer -> (TextureMap, SpriteMap) -> GameState -> GameState -> IO (TextureMap, SpriteMap)
updateTSMap renderer (tmap, smap) oldState newState = do
  let oldEnv = envi oldState
  let newEnv = envi newState
  let oldBuildings = batiments oldEnv
  let newBuildings = batiments newEnv
  let oldUnits = unites oldEnv
  let newUnits = unites newEnv

  -- Check if a new building has been added
  let addedBuildings = Map.difference newBuildings oldBuildings
  unless (Map.null addedBuildings) $ putStrLn "Loading new buildings..."
  (tmap', smap') <- foldM (\acc e -> loadBatiment renderer acc e) (tmap, smap) (Map.elems addedBuildings)

  -- Check if a new unit has been added
  let addedUnits = Map.difference newUnits oldUnits
  unless (Map.null addedUnits) $ putStrLn "Loading new units..."
  (tmap2, smap2) <- foldM (\acc e -> loadUnite renderer acc e) (tmap', smap') (Map.elems addedUnits)

  -- Check if a building has been removed
  let removedBuildings = Map.difference oldBuildings newBuildings
  unless (Map.null removedBuildings) $ putStrLn "Removing buildings..."
  tmap3 <- foldM (\acc e -> TM.destroyTexture (getTextureIdForBatiment e) acc) tmap2 (Map.elems removedBuildings)
  let smap3 = foldl (\acc e -> removeSprite (getSpriteIdForBatiment e) acc) smap2 (Map.elems removedBuildings)

  -- Check if a unit has been removed
  let removedUnits = Map.difference oldUnits newUnits
  unless (Map.null removedUnits) $ putStrLn "Removing units..."
  tmap4 <- foldM (\acc e -> TM.destroyTexture (getTextureIdForUnite e) acc) tmap3 (Map.elems removedUnits)
  let smap4 = foldl (\acc e -> removeSprite (getSpriteIdForUnite e) acc) smap3 (Map.elems removedUnits)

  putStrLn "New sprite ids:"
  mapM_ (putStrLn . show) (Map.keys smap4)
  return (tmap4, smap4) 
