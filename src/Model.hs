module Model where

import SDL

import Keyboard
import qualified Keyboard as K
import Common
import Unite
import Prelude
import qualified Data.Map as M
import Environnement
import Joueur
import Carte
import Data.Maybe
import Data.List (find,foldl')
import Batiment
import qualified Debug.Trace as Debug



data MenuItem = BuildBuilding BatimentType
              | ProduceUnit UniteType
              | CollectResources
              | Attack
              | Patrol
              | Moveto
              deriving (Show, Eq)

type Selected = Either EntiteId Terrain

data GameState = GameState
    { envi :: Environnement
    , currentPlayer :: Maybe Joueur
    , pendingCommand :: Maybe Commande
    , selected :: Maybe Selected
    , menuItems :: [MenuItem]
    , pendingAction :: Maybe MenuItem
    , executor :: Maybe EntiteId
    ,remainingTurnTime :: Float
    }
  deriving (Show)

initGameState :: Environnement -> GameState
initGameState ienv = GameState
  { envi = ienv
  , currentPlayer = Nothing
  , pendingCommand = Nothing
  , selected = Nothing
  , menuItems = []
  , pendingAction = Nothing
  , executor = Nothing
  , remainingTurnTime = 20
  }

gameStep :: GameState -> Keyboard -> MouseState -> MouseState -> Float -> GameState
gameStep gstate kbd prevMouseState mouseState deltaTime =
  let remainingTurnTime' = remainingTurnTime gstate - deltaTime
      isNewTurn = remainingTurnTime' <= 0
      turnDuration = 30 -- Set the turn duration in seconds
      remainingTurnTime'' = if isNewTurn then turnDuration else remainingTurnTime'

      selected' = if leftButton mouseState && not (leftButton prevMouseState)
                     then selectedCase (mouseX mouseState) (mouseY mouseState) gstate
                     else selected gstate
      menuItems' = case selected' of
                Just (Left entityId) -> getMenuItemsForEntity gstate entityId
                _                    -> []
      gstate' = gstate { selected = selected', menuItems = menuItems'}

      -- Execute the pending action if the left button was just pressed and not in the menu area
      gstate2 = if leftButton mouseState && not (leftButton prevMouseState) && not (clickedInMenu mouseState)
                 then
                   case pendingAction gstate' of
                     Nothing -> gstate'
                     Just menuItem ->
                       let coord = screenCoordToMapCoord (mouseX mouseState, mouseY mouseState)
                       in Debug.trace "executeMenuItemAction will called in gameStep" $ executeMenuItemAction gstate' menuItem coord
                 else gstate'

      -- Add more logic here based on keyboard input
      -- Update the remainingTurnTime and call debutTour if it's a new turn
      gstate3 = if isNewTurn
                then gstate2 { envi = debutTour (envi gstate2), remainingTurnTime = remainingTurnTime'' }
                else gstate2 { remainingTurnTime = remainingTurnTime'' }
                
      -- Call terminerProductionForALL
      env' = terminerProductionForALL (envi gstate3)

      -- Call tourDeJeu for unites
      env'' = tourDeJeu env'

      env2 = convertResourcesForALL env''
      -- Update the environment
      gstate4 = gstate3 { envi = env2 }
  in Debug.trace("show batiments (envi gstate4" ++ show (batiments (envi gstate4))) $ gstate4



selectedCase :: Int -> Int -> GameState -> Maybe Selected
selectedCase x y gameState =
  if x < 640 && y < 640 -- Check if the coordinates are within the game map area
    then
      case selectEntity x y gameState of
        Just entId -> Just (Left entId)
        Nothing -> Just (Right (fromMaybe Herbe (selectedTerrain x y gameState)))
    else
      Nothing 


selectedTerrain :: Int -> Int -> GameState -> Maybe Terrain
selectedTerrain x y gameState =
  let env = envi gameState
      coord = C (x `div` 32) (y `div` 32)  -- Convert click coordinates to map coordinates
  in getTerrain (ecarte env) coord

selectEntity :: Int -> Int -> GameState -> Maybe EntiteId
selectEntity x y gameState =
  let env = envi gameState
      allEntities = [(Left batId, Left bat) | (batId, bat) <- M.assocs (batiments env)] ++
                    [(Right unitId, Right unit) | (unitId, unit) <- M.assocs (unites env)]
      clickedEntities = filter (\(_, ent) -> isEntityClicked x y ent) allEntities
  in case clickedEntities of
       [] -> Nothing
       ((entId, _):_) -> Just entId
  
isEntityClicked :: Int -> Int -> Entite -> Bool
isEntityClicked x y ent =
  let (entX, entY) = entiePos ent
      entityWidth = 32
      entityHeight = 32
  in x >= entX && x < entX + entityWidth && y >= entY && y < entY + entityHeight


-- Get the health of the entity with the given EntiteId
getEntityHealth :: GameState -> EntiteId -> Int
getEntityHealth gameState entityId =
  case entityId of
    Left batId -> bpointsVie (batiments (envi gameState) M.! batId)
    Right unitId -> upointsVie (unites (envi gameState) M.! unitId)

-- Get the player ID of the entity with the given EntiteId
getEntiteJoueur :: GameState -> EntiteId -> JoueurId
getEntiteJoueur gameState entityId =
  case entityId of
    Left batId -> bproprio (batiments (envi gameState) M.! batId)
    Right unitId -> uproprio (unites (envi gameState) M.! unitId)

-- Get the credit of the player with the given JoueurId
getJoueurCredit :: GameState -> JoueurId -> Int
getJoueurCredit gameState playerId =
  case findJoueur playerId (joueurs (envi gameState)) of
    Just joueur -> jcredits joueur
    Nothing -> 0

getMenuItemsForEntity :: GameState -> EntiteId -> [MenuItem]
getMenuItemsForEntity gameState entityId =
  let env = envi gameState
      maybeEntity = case entityId of
                      Left batId -> Left <$> M.lookup batId (batiments env)
                      Right unitId -> Right <$> M.lookup unitId (unites env)
  in case maybeEntity of
       Just (Left bat) ->
         case btype bat of
           Usine -> [BuildBuilding batType | batType <- [ Raffinerie, Usine, Centrale]] ++ [ProduceUnit unitType | unitType <- [Collecteur, Combatant]]
           _ -> [BuildBuilding batType | batType <- [ Raffinerie, Usine, Centrale]]
       Just (Right unit) ->
         case utype unit of
           Collecteur  -> [CollectResources,Moveto]
           Combatant -> [Attack, Patrol,Moveto]
       Nothing -> []

handleMenuClick :: GameState -> MouseState -> GameState
handleMenuClick gameState mouseState = 
  Debug.trace "handleMenuClick is called" $
  case selected gameState of
    Just (Left entityId) -> 
      let clickedMenuItem = getMenuItemAtClick gameState mouseState
          currentPlayerId = getEntiteJoueur gameState entityId
          currentPlayer = findJoueur currentPlayerId (joueurs (envi gameState))         
      in case clickedMenuItem of
        Just menuItem -> gameState { pendingAction = Just menuItem ,currentPlayer = currentPlayer }
        Nothing -> gameState
    _ -> gameState

handleMapClick :: GameState -> MouseState -> GameState
handleMapClick gameState mouseState =
  Debug.trace "handleMapClick is called" $
  case pendingAction gameState of
    Nothing -> -- Handle the normal map click, e.g., selecting units or buildings
      let newSelected = selectedCase (mouseX mouseState) (mouseY mouseState) gameState
          newExecutor = selectEntity (mouseX mouseState) (mouseY mouseState) gameState
      in gameState { selected = newSelected , executor = newExecutor }
    Just _ -> gameState -- Keep the pending action for the next game step


getMenuItemAtClick :: GameState -> MouseState -> Maybe MenuItem
getMenuItemAtClick gameState mouseState =
  case selected gameState of
    Just (Left entityId) -> 
      let menuItems = getMenuItemsForEntity gameState entityId
          menuPositions = zipWith (\item index -> SDL.Rectangle (SDL.P (SDL.V2 650 (200 + 30 * index))) (SDL.V2 150 20)) menuItems [0..]
          clickedPoint = V2 (mouseX mouseState) (mouseY mouseState)
          menuItemInRect = find (\(item, rect) -> isPointInRect clickedPoint rect) (zip menuItems menuPositions)
      in case menuItemInRect of
        Just (item, _) -> Just item
        Nothing -> Nothing
    _ -> Nothing

-- handle mouse events for selecting buildings, units or clicking commands in the menu
handleMouseEvent :: Event -> GameState -> MouseState -> (GameState, MouseState)
handleMouseEvent event gameState mouseState =
  let mouseState' = case eventPayload event of
        MouseButtonEvent e ->
          let button = mouseButtonEventButton e
              motion = mouseButtonEventMotion e
              P (V2 x y) = mouseButtonEventPos e
              pos = V2 (fromIntegral x) (fromIntegral y) :: V2 Int
          in if button == ButtonLeft
             then mouseState { leftButton = motion == Pressed
                             , mouseX = getComponentX pos
                             , mouseY = getComponentY pos
                             , clickedInMenu = isClickInMenuArea mouseState
                             }
             else mouseState
        MouseMotionEvent e ->
          let P (V2 x y) = mouseMotionEventPos e
              pos = V2 (fromIntegral x) (fromIntegral y) :: V2 Int
          in mouseState { mouseX = getComponentX pos
                        , mouseY = getComponentY pos
                        }
        _ -> mouseState

      gameState' = if leftButton mouseState' && not (leftButton mouseState)
                   then
                     if clickedInMenu mouseState'
                     then handleMenuClick gameState mouseState'
                     else handleMapClick gameState mouseState'
                   else gameState

  in (gameState', mouseState')

handleEvents :: [Event] -> GameState -> (Keyboard, MouseState) -> (GameState, Keyboard, MouseState)
handleEvents events gameState (kbd, mouseState) =
  let kbd' = foldl' (flip handleKeyboardEvent) kbd events
      (gameState', mouseState') = foldl' (\(gs, ms) e -> handleMouseEvent e gs ms) (gameState, mouseState) events
  in (gameState', kbd', mouseState')



screenCoordToMapCoord :: (Int, Int) -> Coord
screenCoordToMapCoord (x, y) =
  let mapX = x `div` 32
      mapY = y `div` 32
  in (C mapX mapY)

executeMenuItemAction :: GameState -> MenuItem -> Coord -> GameState
executeMenuItemAction gstate menuItem coord =
  Debug.trace "executeMenuItemAction is called" $
  case menuItem of
    BuildBuilding btype ->
      let newEnv = case currentPlayer gstate of
                     Just player -> construireBatiment player (envi gstate) btype coord
                     Nothing -> envi gstate
      in gstate { envi = newEnv, pendingAction = Nothing }
    ProduceUnit utype ->
      case selected gstate of
        Just (Left entId) ->
          case entId of 
            Left batId -> case M.lookup batId (batiments (envi gstate)) of
                            Just bat -> let newEnv = produireUnite (envi gstate) utype bat
                                        in gstate { envi = newEnv, pendingAction = Nothing }
                            Nothing -> gstate -- Selected entity is not a valid building
            _ -> gstate -- Selected entity is not a building
        _ -> gstate -- No building selected, do nothing
    CollectResources ->
      case selected gstate of
        Just (Right terrain) ->
          case terrain of 
            Ressource _ ->
              Debug.trace "executeMenuItemAction CollectResources is called" $
              case executor gstate of
                Just (Right uniteId) -> 
                  Debug.trace "executeMenuItemAction CollectResources is called" $
                  case M.lookup uniteId (unites (envi gstate)) of
                                  Just unite -> let newEnv = executeOrdreCollecte unite (envi gstate) coord
                                                in gstate { envi = newEnv, pendingAction = Nothing }
                                  Nothing -> gstate -- Selected entity is not a valid unite
                _ -> gstate -- executor entity is not a unite
            _ -> gstate -- No ressource selected, do nothing
        _ -> gstate -- No terrain selected, do nothing
    Moveto ->
      Debug.trace "executeMenuItemAction Moveto is called" $
      case executor gstate of
        Just (Right uniteId) -> 
          case M.lookup uniteId (unites (envi gstate)) of
            Just unite -> 
              let newEnv = calculatePath coord unite (envi gstate)
              in gstate { envi = newEnv, pendingAction = Nothing }
            Nothing -> gstate -- Selected entity is not a valid unite
        _ -> gstate -- executor entity is not a unite
    Attack ->
      Debug.trace "executeMenuItemAction Attack is called" $
      let maybeAttacker = do
            (Right uniteId) <- executor gstate
            findUniteById uniteId (envi gstate)
          maybeTarget = do
            (Left entId) <- selected gstate
            findEntiteByID entId (envi gstate)
          newEnv = attaque <$> maybeAttacker <*> maybeTarget <*> Just (envi gstate)
      in case newEnv of
           Just env' -> Debug.trace "Attack "gstate { envi = env', pendingAction = Nothing }
           Nothing -> Debug.trace "Invalid Attack: Check if both executor and selected entity are valid." gstate
    Patrol -> gstate { pendingAction = Just menuItem } -- Not implemented


