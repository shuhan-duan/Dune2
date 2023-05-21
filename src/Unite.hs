{-# LANGUAGE BlockArguments #-}
module Unite where

import Common
import Carte
import Joueur
import Environnement
import qualified Data.Map as M
import Data.Maybe
import Path
import qualified Debug.Trace as Debug
import Data.List (find,foldl')


prop_cuveInvariant :: Maybe Tank -> Bool
prop_cuveInvariant Nothing = False
prop_cuveInvariant (Just (EmptyTank cap)) = cap >= 0
prop_cuveInvariant (Just (FullTank cap)) = cap >= 0
prop_cuveInvariant (Just (Tank cap cour)) = cap > 0 && cour > 0 && cour <= cap

initCuve :: Int -> Maybe Tank
initCuve cap
    | cap > 0 = Just $ EmptyTank cap
    | otherwise = Nothing

quantite ::Tank -> Int
quantite (Tank _ q) = q
quantite (EmptyTank _) = 0
quantite (FullTank cap) = cap

capacite ::Tank -> Int
capacite (Tank c _) = c
capacite (EmptyTank c) = c
capacite (FullTank c) = c

changeCuve :: Tank -> Int -> Tank
changeCuve cu q
        | q == 0 = EmptyTank (capacite cu)
        | q == capacite cu = FullTank (capacite cu)
        | otherwise  = Tank (capacite cu) q

-- v: volume
remplirCuve ::Tank -> Int -> Maybe Tank
remplirCuve _ v | v <= 0 = Nothing
remplirCuve cu v =
    let q = v + quantite cu in
        if q <= capacite cu then
        Just $ changeCuve cu q
                    else Nothing

isCollecteur :: Unite -> Bool
isCollecteur (Unite {utype = Collecteur}) = True
isCollecteur _ = False

isCombattant :: Unite -> Bool
isCombattant (Unite { utype = Combatant }) = True
isCombattant _ = False

-- check the cuve is full
isFull :: Tank -> Bool
isFull (FullTank _) = True
isFull _ = False

-- Returns the capacity of the cuve of a given unite type.
uniteTypeCapaciteCuve :: UniteType -> Int
uniteTypeCapaciteCuve Collecteur = 100
uniteTypeCapaciteCuve Combatant = 0 -- Combatants do not have tanks

-- Returns the cost (in credits) of a given unite type.
uniteTypeCost :: UniteType -> Int
uniteTypeCost Collecteur  = 10
uniteTypeCost Combatant = 10

-- Returns the product time of a given unite type.
uniteTypeTempsProd :: UniteType -> Int
uniteTypeTempsProd Collecteur  = 6
uniteTypeTempsProd Combatant = 4

-- Returns the point of life  of a given unite type.
uniteTypePointsVie :: UniteType -> Int
uniteTypePointsVie Collecteur  = 50
uniteTypePointsVie Combatant = 30

-- create new unite 
creerUnite :: UniteType -> Joueur -> Coord -> Environnement -> Unite
creerUnite utype joueur coord env =
  let allUnits = M.elems (unites env)
      newId = if null allUnits
              then UniteId 1
              else UniteId (1 + unUniteId (Prelude.maximum (map uid allUnits)))
  in  Unite {
    uid = newId,
    utype = utype,
    uproprio = jid joueur,
    ucoord = coord,
    upath = [],
    upointsVie = uniteTypePointsVie utype,
    ucuve = initCuve (uniteTypeCapaciteCuve utype),
    uordres = [],
    ubut = Nothing
} 
    


-- Defirnir la porter d'attaque
porter :: Coord -> Coord -> Bool
porter (C x1 y1) (C x2 y2)  = ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2) <= 4

-- Attaquer une entite 
attaque :: Unite -> Entite -> Environnement -> Environnement
attaque attaquant cible env =
  let
    attaquantProprio = uproprio attaquant
    cibleProprio = either bproprio uproprio cible
  in
    if attaquantProprio == cibleProprio
      then env  -- if the attaquant and the cible belong to the same player, do nothing
      else
        case cible of
          Left batiment ->Debug.trace ("attaqueBatiment is called, batiment: " ++ show batiment)  attaqueBatiment attaquant batiment env
          Right unite -> Debug.trace ("attaqueUnite is called, unite: " ++ show unite) attaqueUnite attaquant unite env

attaqueBatiment :: Unite -> Batiment -> Environnement -> Environnement
attaqueBatiment attaquant batiment env =
  let degats = case utype attaquant of
                 Combatant -> 10
                 _ -> 0
      newPointsVie = max 0 (bpointsVie batiment - degats)
      updatedBatiment = batiment { bpointsVie = newPointsVie }
  in if newPointsVie == 0
       then detruireBatiment env updatedBatiment
       else updateBatiment updatedBatiment env

attaqueUnite :: Unite -> Unite -> Environnement -> Environnement
attaqueUnite attaquant cible env =
  let degats = case utype attaquant of
                 Combatant -> 10
                 _ -> 0
      newPointsVie = max 0 (upointsVie cible - degats)
      updatedCible = cible { upointsVie = newPointsVie }
  in if newPointsVie == 0
       then removeUnite updatedCible env
       else updateUnite updatedCible env

-- Pres condition attaque
prop_preAttaque :: Unite-> Bool
prop_preAttaque unite=
  case (utype unite) of
    Combatant-> True
    otherwise->False
-- Post Condition attaque
prop_postAttaque :: Unite -> Entite -> Environnement -> Bool
prop_postAttaque unite cible env =
  let result = attaque unite cible env 
      degats = 10
  in case cible of
      Left batiment -> 
        let newPointsVie= max 0 (bpointsVie batiment - degats)
        in if newPointsVie == 0
           then isMissingBat batiment result
           else 
            let batupdate = getBatiment (bid batiment) result
                pointdeVie = bpointsVie batupdate
            in pointdeVie == newPointsVie
      Right unite ->
        let newPointsVie= max 0 (upointsVie unite - degats)
        in if newPointsVie == 0
           then isMissingUnit unite result
           else 
            let uniteUpdate = getUnite (uid unite) result
                pointdeVie = upointsVie uniteUpdate
            in pointdeVie == newPointsVie


-- Deplacer une unite
modifCoordEO :: Unite -> Direction -> Unite
modifCoordEO u dir =
  let x = cx (ucoord u) + if dir == Est then 1 else -1
      y = cy (ucoord u)
      z = u {ucoord = creeCoord x y}
  in z

modifCoordNS :: Unite -> Direction -> Unite
modifCoordNS u dir =
  let x = cx (ucoord u)
      y = cy (ucoord u) + if dir == Nord then 1 else -1
      z = u {ucoord = creeCoord x y}
  in z

-- deplace l'unité à la coordonnée suivante
deplacer :: Unite -> Environnement -> Environnement
deplacer unite env =
  case upath unite of
    [] -> env
    (nextCoord:remainingPath) ->
      let nextBut = if null remainingPath 
                    then case uordres unite of
                          [] -> Nothing 
                          (nextOrdre:_) -> Just nextOrdre
                    else Just (Deplacer nextCoord)
          newUnite = unite { ucoord = nextCoord, upath = remainingPath, ubut = nextBut}
      in updateUnite newUnite env

--Post condition deplacer 
prop_postdeplacer:: Coord -> Unite -> Environnement -> Bool
prop_postdeplacer coord unite env =
  let env'= deplacer unite env 
  in 
    let path = bfs env (ucoord unite) coord
        coordInpath = head path
        newUnite = M.lookup (uid unite) (unites env')
        newCoord = head (upath (fromJust(newUnite)))
    in coordInpath == newCoord



calculatePath :: Coord -> Unite -> Environnement -> Environnement
calculatePath coord unite env =
  let path = bfs env (ucoord unite) coord
      updatedEnv = case path of
        [] -> env -- No valid path found, do not update the environment
        (nextCoord:restOfPath) ->
          let newUnite = unite { ucoord = nextCoord, upath = restOfPath , ubut = Just (Deplacer nextCoord) }
          in Debug.trace ("calculatePath is called, newUnite: " ++ show newUnite) updateUnite newUnite env
  in Debug.trace ("calculatePath is called, path: " ++ show path) updatedEnv

modifBut :: Ordre -> Unite -> Environnement -> Environnement
modifBut o u env =
  let uniteUpdate = u {ubut = Just o}
  in updateUnite uniteUpdate env

-- Collecter de la ressource
collecterRessource :: Unite -> Environnement -> Environnement
collecterRessource unite env =
  case utype unite of
    Collecteur -> -- The unit is a collector
      let cuve = fromJust(ucuve unite)in
      case getTerrain (ecarte env) (ucoord unite) of
        Just (Ressource _) -> -- There is a resource at the unit's position
          let capaciteDisponible = capacite cuve - quantite cuve
              (q, carte') = Debug.trace "collecting Ressource " $ collecteCase (ucoord unite) capaciteDisponible (ecarte env)
          in case remplirCuve cuve q of
               Just cuve' -> -- Update the unit's cuve and the map
                 let unite' = unite {ucuve = Just cuve' ,uordres = []}
                     env' = env {ecarte = carte'}
                 in if isFull cuve' then -- The cuve is full
                      let raffinerie = findRaffinerie env' (uproprio unite') -- Find a raffinerie owned by the unit's owner
                      in case raffinerie of
                        Just raff -> 
                          Debug.trace( "Move the collector"++show unite' ++ " to the raffinerie : "++ show raff )$ calculatePath (bcoord raff) unite' env' -- Move the collector to the raffinerie
                        -- todo : if is arrived , the raffinerie will change the resoure to credits
                        Nothing -> env' -- No raffinerie found
                    else
                      Debug.trace "collected Ressource " $ updateUnite unite' env'
               Nothing -> env
        _ -> env -- No resource at the unit's position
    _ -> env -- The unit is not a collector

--Precondition collecter ressources
prop_preCollecte::Unite->Carte->Environnement->Bool
prop_preCollecte unite carte env =
  let isCollecteur= (utype unite)==Collecteur
      hasRessource= (case getTerrain carte (ucoord unite) of
                        Just (Ressource _) -> True
                        _ -> False)
      isnFull = not (isFull (fromJust(ucuve unite)))
  in isCollecteur && hasRessource && isnFull

-- Post condition collecter ressources
prop_postCollecte:: Unite->Carte->Environnement->Bool
prop_postCollecte unite carte env =
  let env'= collecterRessource unite env
  in case utype unite of
      Collecteur ->
        let cuve = fromJust (ucuve unite) in
          case getTerrain carte (ucoord unite) of
            Just (Ressource _) ->
              let capaciteDisponible = capacite cuve - quantite cuve
                  (q, carte') = collecteCase (ucoord unite) capaciteDisponible carte
              in case (remplirCuve cuve q) of 
                  Just cuve' ->
                    let newUnite= M.lookup (uid unite) (unites env')
                        newCUve = ucuve (fromJust(newUnite))
                        isTRueCuve = fromJust(newCUve)== cuve'
                        isTRueCarte = prop_postCollecteCase (ucoord unite) capaciteDisponible carte
                    in isTRueCuve && isTRueCarte
                  Nothing -> False
            _ -> False
      _ -> False 

executeOrdreCollecte :: Unite -> Environnement -> Coord -> Environnement
executeOrdreCollecte unite env targetCoord
  | not (isCollecteur unite) = env -- The unit is not a collector, so no action is taken
  | otherwise = 
    Debug.trace "executeOrdreCollecte is called " $
    case ucuve unite of
      Just cuve ->
        if isFull cuve then
          let raffinerie = findRaffinerie env (uproprio unite) -- Find a raffinerie owned by the unit's owner
          in case raffinerie of
            Just raff -> 
              calculatePath (bcoord raff) unite env -- Move the collector to the raffinerie
            -- todo : if is arrived , the raffinerie will change the resoure to credits
            Nothing -> env -- No raffinerie found
        else  
          let env' = Debug.trace "will call deplacer  " $ calculatePath targetCoord unite env -- Move the collector to the target location
              newUnite = case findUniteById (uid unite) env' of  -- update the uordres
                              Just updatedUnite -> updatedUnite { uordres = (Collecter targetCoord) : (uordres updatedUnite) }
                              Nothing -> unite
          in updateUnite newUnite env'
      Nothing -> env -- The collector has no cuve, so no action is taken


executeOrdrePatrouille :: Unite -> Environnement -> Coord -> Coord -> Environnement
executeOrdrePatrouille unite env coord1 coord2
  | not (isCombattant unite) = env -- The unit is not a combatant, so no action is taken
  | otherwise =
      case ubut unite of
        Just(Attaquer _ )->
          let ennemi = findEnnemiInRange unite env
          in case ennemi of
            Just entite -> attaque unite entite env
            Nothing -> calculatePath coord1 unite env
        Just (Deplacer _) ->
          if ucoord unite == coord1 then
            let updatedUnite = unite { ubut = Just (Deplacer coord1) }
            in calculatePath coord2 updatedUnite env
          else if ucoord unite == coord2 then
            let updatedUnite = unite { ubut = Just (Deplacer coord2) }
            in calculatePath coord1 updatedUnite env
          else env
        _ -> env

findEnnemiInRange :: Unite -> Environnement -> Maybe Entite
findEnnemiInRange unite env =
  let currentPlayerId = uproprio unite
      ennemiUnites = filter (\u -> uproprio u /= currentPlayerId) (M.elems (unites env))
      ennemiBatiments = filter (\b -> bproprio b /= currentPlayerId) (M.elems (batiments env))
      ennemiEntites = map Right ennemiUnites ++ map Left ennemiBatiments
      ennemiInRange = filter (porter (ucoord unite) . entiteCoord) ennemiEntites
  in listToMaybe ennemiInRange

etape :: Unite -> Environnement -> Environnement
etape  unite env =
  case ubut unite of
    Just (Attaquer entite) -> attaque unite entite env
    Just (Deplacer _)  -> deplacer unite env
    Just (Collecter _)  -> collecterRessource unite env
    Just (Patrouiller coord1 coord2) -> executeOrdrePatrouille unite env coord1 coord2
    Nothing -> 
      case upath unite of
        [] -> env
        (nextCoord:remainingPath) ->
          let updatedUnit = unite { ucoord = nextCoord, upath = remainingPath }
          in updateUnite updatedUnit env

tourDeJeu :: Environnement -> Environnement
tourDeJeu env = M.foldr etape env (unites env)

findUniteAtCoord :: Coord -> Environnement -> Maybe Unite
findUniteAtCoord coord env =
  let allUnites = M.elems (unites env)
  in find (\u -> ucoord u == coord) allUnites
