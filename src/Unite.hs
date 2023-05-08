module Unite where

import Common
import Carte
import Joueur
import Environnement
import qualified Data.Map as M
import Data.Maybe
import Path

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
uniteTypeCapaciteCuve Collecteur = 30
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
uniteTypePointsVie Collecteur  = 100
uniteTypePointsVie Combatant = 50

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
    uordres = []
} 
    


-- Defirnir la porter d'attaque
porter :: Coord -> Coord -> Bool
porter (C x1 y1) (C x2 y2)  = ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2) <= 4

-- Attaquer une entite 
attaque :: Unite -> Entite -> Environnement -> Environnement
attaque attaquant cible env =
  case cible of
    Left batiment -> attaqueBatiment attaquant batiment env
    Right unite -> attaqueUnite attaquant unite env

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

deplacer :: Coord -> Unite -> Environnement -> Environnement
deplacer coord unite env =
  let path = aStar env (ucoord unite) coord
      newUnite = unite { upath = path }
  in updateUnite newUnite env



modifBut :: Ordre -> Unite -> Environnement -> Environnement
modifBut o u env =
  let uniteUpdate = u {ubut = o}
  in updateUnite uniteUpdate env

-- Collecter de la ressource
collecterRessource :: Unite -> Carte -> Environnement -> Environnement
collecterRessource unite carte env =
  case utype unite of
    Collecteur -> -- The unit is a collector
      let cuve = fromJust(ucuve unite)in
      case getTerrain carte (ucoord unite) of
        Just (Ressource _) -> -- There is a resource at the unit's position
          let capaciteDisponible = capacite cuve - quantite cuve
              (q, carte') = collecteCase (ucoord unite) capaciteDisponible carte
          in case remplirCuve cuve q of
               Just cuve' -> -- Update the unit's cuve and the map
                 let unite' = unite {ucuve = Just cuve'}
                     env' = env {ecarte = carte'}
                 in updateUnite unite' env'
               Nothing -> env
        _ -> env -- No resource at the unit's position
    _ -> env -- The unit is not a collector

executeOrdreCollecte :: Unite -> Environnement -> Coord -> Environnement
executeOrdreCollecte unite env targetCoord
  | not (isCollecteur unite) = env -- The unit is not a collector, so no action is taken
  | otherwise = case ucuve unite of
      Just cuve ->
        if isFull cuve then
          let raffinerie = findRaffinerie env (uproprio unite) -- Find a raffinerie owned by the unit's owner
          in case raffinerie of
            Just raff -> deplacer (bcoord raff) unite env -- Move the collector to the raffinerie
            -- todo : if is arrived , the raffinerie will change the resoure to credits
            Nothing -> env -- No raffinerie found
        else
          let carte = ecarte env
              terrain = getTerrain carte (ucoord unite)
          in case terrain of
            Just (Ressource _) -> collecterRessource unite carte env -- Collect resources
            _ -> deplacer targetCoord unite env -- Move the collector to the target location
      Nothing -> env -- The collector has no cuve, so no action is taken

executeOrdrePatrouille :: Unite -> Environnement -> Coord -> Coord -> Environnement
executeOrdrePatrouille unite env coord1 coord2
  | not (isCombattant unite) = env -- The unit is not a combatant, so no action is taken
  | otherwise =
      case ubut unite of
        Attaquer _ ->
          let ennemi = findEnnemiInRange unite env
          in case ennemi of
            Just entite -> attaque unite entite env
            Nothing -> deplacer coord1 unite env
        Deplacer _ ->
          if ucoord unite == coord1 then
            let updatedUnite = unite { ubut = Deplacer coord2 }
            in deplacer coord2 updatedUnite env
          else if ucoord unite == coord2 then
            let updatedUnite = unite { ubut = Deplacer coord1 }
            in deplacer coord1 updatedUnite env
          else env
        _ -> env

headCoord :: [Ordre] -> Maybe Coord
headCoord (Patrouiller pt1 _:_) = Just pt1
headCoord _ = Nothing

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
    Attaquer entite -> attaque unite entite env
    Deplacer coord -> deplacer coord unite env
    Collecter coord -> executeOrdreCollecte unite env coord
    Patrouiller coord1 coord2 -> executeOrdrePatrouille unite env coord1 coord2


tourDeJeu :: Environnement -> Environnement
tourDeJeu env = M.foldr etape env (unites env)
