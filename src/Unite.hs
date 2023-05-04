{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Unite where

import Common
import Carte
import Joueur
import qualified Data.Map as M
import Environnement

    
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

capaciteCuve :: UniteType -> Int
capaciteCuve (Collecteur t) = case t of
                                EmptyTank c -> c
                                FullTank c -> c
                                Tank c _ -> c
capaciteCuve Combatant = 0 -- Les combattants ne peuvent pas collecter de ressources

-- Returns the cost (in credits) of a given unite type.
uniteTypeCost :: UniteType -> Int
uniteTypeCost (Collecteur _) = 10 
uniteTypeCost Combatant = 10 

-- Returns the product time of a given unite type.
uniteTypeTempsProd :: UniteType -> Int
uniteTypeTempsProd (Collecteur _)  = 20 
uniteTypeTempsProd Combatant = 15

-- Returns the point of life  of a given unite type.
uniteTypePointsVie :: UniteType -> Int
uniteTypePointsVie (Collecteur _) = 100
uniteTypePointsVie Combatant = 50

-- Returns the capacity of the cuve of a given unite type.
uniteTypeCapaciteCuve :: UniteType -> Int
uniteTypeCapaciteCuve (Collecteur tank) = capacite tank
uniteTypeCapaciteCuve Combatant = 0 -- Combatants do not have tanks

-- create new unite 
creerUnite :: UniteType -> Joueur -> Coord -> Unite
creerUnite utype joueur coord =
  Unite {
    uid = getNextUniteId joueur,
    utype = utype,
    uproprio = jid joueur,
    ucoord = coord,
    udirection = Nord,
    upointsVie = uniteTypePointsVie utype,
    ucuve = initCuve (uniteTypeCapaciteCuve utype),
    uordres = [],
    ubut = Deplacement coord
  }

-- update the environment with new unite
updateUnite :: Unite -> Environnement -> Environnement
updateUnite updatedUnite env =
  let currentPlayer = head $ filter (\j -> jid j == uproprio updatedUnite) (joueurs env)
      updatedUnites = M.insert (uid updatedUnite) updatedUnite (junites currentPlayer)
      updatedPlayer = currentPlayer { junites = updatedUnites }
      updatedGlobalUnites = M.insert (uid updatedUnite) updatedUnite (unites env)
  in updateJoueur updatedPlayer (env { unites = updatedGlobalUnites })

-- Defirnir la porter d'attaque
porter :: Coord -> Coord -> Bool
porter (C x1 y1) (C x2 y2) = ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2) <= 4

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

removeUnite :: Unite -> Environnement -> Environnement
removeUnite unite env =
  let currentPlayer = head $ filter (\j -> jid j == uproprio unite) (joueurs env)
      updatedUnites = M.delete (uid unite) (junites currentPlayer)
      updatedPlayer = currentPlayer { junites = updatedUnites }
      updatedGlobalUnites = M.delete (uid unite) (unites env)
  in updateJoueur updatedPlayer (env { unites = updatedGlobalUnites })


-- Deplcer Unite
-- deplacer::Coord->Unite->Unite


