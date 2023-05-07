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

type Selected = Either EntiteId Terrain

data MenuItem = BuildBuilding BatimentType
              | ProduceUnit UniteType
              | CollectResources
              | Attack
              | Patrol
              deriving (Show, Eq)

data GameState = GameState
    { commands :: [Commande]
    , envi :: Environnement
    , pendingCommand :: Maybe Commande
    , selected :: Maybe Selected
    , menuItems :: [MenuItem]
    }
  deriving (Show)

initGameState :: Environnement -> GameState
initGameState ienv = GameState
  { commands = []
  , envi = ienv
  , pendingCommand = Nothing
  , selected = Nothing
  }

gameStep :: GameState -> Keyboard -> MouseState -> Float -> GameState
gameStep gstate kbd mouseState deltaTime =
  let selected' = if leftButton mouseState
                     then selectCase (mouseX mouseState) (mouseY mouseState) gstate
                     else selected gstate
      menuItems' = case selected' of
                Just (Left entityId) -> getMenuItemsForEntity gstate entityId
                _                    -> []               
      -- pendingCommand' = if leftButton mouseState
      --                   then selectCommand (mouseX mouseState) (mouseY mouseState)
      --                   else pendingCommand gstate

      -- slectedCase : show the ressource 

      -- add more logic here based on keyboard input
      
      -- update env executerCommande :: Commande -> Environnement -> Environnement


      -- Handle building-specific interactions

      -- moves all the units
      gstate2 = moveUnits gstate

      -- if unit is on the raffinerie , convertResourcesToCredits
      -- todo : check unit is on the raffinerie ,
  in gstate2 { selected = selected' , menuItems = menuItems'}

selectCase :: Int -> Int -> GameState -> Maybe Selected
selectCase x y gameState =
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

moveUnits :: GameState -> GameState
moveUnits gstate =
  let env = envi gstate
      env' = M.foldr moveUnit env (unites env)
  in gstate { envi = env' }

moveUnit :: Unite -> Environnement -> Environnement
moveUnit unite env =
  case upath unite of
    [] -> env
    (nextCoord : remainingPath) ->
      let newUnite = unite { ucoord = nextCoord, upath = remainingPath }
      in updateUnite newUnite env

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
           Usine -> [BuildBuilding batType | batType <- [QuartierGeneral, Raffinerie, Usine, Centrale]] ++ [ProduceUnit unitType | unitType <- [Collecteur, Combatant]]
           _ -> [BuildBuilding batType | batType <- [QuartierGeneral, Raffinerie, Usine, Centrale]]
       Just (Right unit) ->
         case utype unit of
           Collecteur  -> [CollectResources]
           Combatant -> [Attack, Patrol]
       Nothing -> []

