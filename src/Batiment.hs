{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
module Batiment where

import Common
import Carte

import qualified Data.Map as M
import Unite
import Environnement
import Joueur
import Data.Map
import Data.Maybe
import qualified Debug.Trace as Debug

-- Returns the cost (in credits) of a given building type.
batimentTypeCost :: BatimentType -> Int
batimentTypeCost QuartierGeneral = 100
batimentTypeCost Raffinerie = 50
batimentTypeCost Usine = 75
batimentTypeCost Centrale = 100

-- Returns the maximum number of hit points for a given building type.
batimentTypePointsVie :: BatimentType -> Int
batimentTypePointsVie QuartierGeneral = 100
batimentTypePointsVie Raffinerie = 20
batimentTypePointsVie Usine = 80
batimentTypePointsVie Centrale = 60

-- Returns the energy production or consumption (in units) of a given building type.
batimentTypeEnergie :: BatimentType -> Int
batimentTypeEnergie QuartierGeneral = 10 -- produces energy
batimentTypeEnergie Raffinerie = -5 -- consumes energy
batimentTypeEnergie Usine = -10 -- consumes energy
batimentTypeEnergie Centrale = 20 -- produces energy

-- | Builds a new building at the given coordinates if possible, updates the player's credits, and returns the updated environment
construireBatiment :: Joueur -> Environnement -> BatimentType -> Coord -> Environnement
construireBatiment joueur env btype coord
  | not (isValidCoord (ecarte env) coord) || not (isConstructible coord (ecarte env)) = env
  | jcredits joueur < batimentTypeCost btype = env
  | otherwise = let
             newBatId = BatId (M.size (batiments env) + 1)
             newBatiment = Batiment { bid = newBatId
                                    , btype = btype
                                    , bproprio = jid joueur
                                    , bcoord = coord
                                    , benergie = batimentTypeEnergie btype
                                    , bpointsVie = batimentTypePointsVie btype
                                    , btempsProd = Nothing
                                    }
             newJoueur = joueur { jcredits = jcredits joueur - batimentTypeCost btype}
             env' = updateJoueur newJoueur env
             updatedEnv = updateBatiment newBatiment env'
             in updatedEnv


--PostConditionConstuire batiment
prop_postConstruireBatiment::Joueur->Environnement->BatimentType->Coord->Bool
prop_postConstruireBatiment j env btype coord =
  let env'=construireBatiment j env btype coord
      batId = BatId (M.size (batiments env) + 1)
      credits = jcredits j - batimentTypeCost btype
      newJoueur = findJoueur (jid j) (joueurs env')
      newCredit = jcredits (fromJust(newJoueur))
      newBat= M.lookup batId (batiments env')
      isTrueBat = (case newBat of 
                    Just _ -> True
                    Nothing -> False)
  in isTrueBat && (credits==newCredit)

-- Modifie les points de vie d'un bâtiment en ajoutant la valeur de deltaPoints.
-- Si le résultat est inférieur à 0, les points de vie sont fixés à 0.    
modifierPointsVie :: Int -> Batiment -> Batiment
modifierPointsVie deltaPoints batiment = batiment { bpointsVie = max 0 (bpointsVie batiment - deltaPoints) }

produireUnite :: Environnement -> UniteType -> Batiment -> Environnement
produireUnite env utype bat  =
    Debug.trace "Call Producing unit in factory" 
    case btype bat of
         Usine -> case btempsProd bat of
            Just _ -> env  -- L'usine est déjà en train de produire une unité
            Nothing ->  case j of
                            Nothing -> env -- Le joueur propriétaire du bâtiment n'a pas été trouvé dans l'environnement
                            Just joueur ->
                                if jcredits joueur < uniteTypeCost utype
                                    then env -- Le joueur n'a pas suffisamment de points pour payer les coûts de production
                                    else let newBat = bat { btempsProd = Just (uniteTypeTempsProd utype, utype)}
                                             newJoueur = joueur { jcredits = jcredits joueur - uniteTypeCost utype }
                                             env' = updateJoueur newJoueur env
                                             env'' = updateBatiment newBat env'
                                        in Debug.trace "Producing unit in factory" $ env''
            where j = joueurProprioBatiment env bat
         _ -> env -- Le bâtiment n'est pas de type "Usine", on ne peut pas effectuer de production d'unité
-- Pre-condition produireUnite
prop_pre_produireUnite::Environnement->UniteType->Batiment->Bool
prop_pre_produireUnite env utype bat =
  let batType = (btype bat) == Usine
      batTmprod = (case btempsProd bat of
                    Just _ -> False
                    Nothing -> True)
      joueurcredit = (case j of
                       Just joueur -> (jcredits joueur) >= uniteTypeTempsProd utype
                       Nothing -> False)
  in batType && batTmprod && joueurcredit 
  where j = joueurProprioBatiment env bat

-- Post condition produireUnite
prop_post_produireUnite :: Environnement -> UniteType -> Batiment -> Bool
prop_post_produireUnite env utype bat =
  let newEnv = produireUnite env utype bat
      newJoueur = joueurProprioBatiment newEnv bat
      newBat = getBatiment (bid bat) newEnv
      newCredjoueur = jcredits (fromJust newJoueur)
      (newTemps, newUnite) = fromJust (btempsProd newBat)
      temps = uniteTypeTempsProd utype
      oldJoueur = joueurProprioBatiment env bat
      credits = jcredits (fromJust oldJoueur) - uniteTypeCost utype
  in newCredjoueur == credits && newTemps == temps && newUnite == utype


-- termine la production de unite 
terminerProduction :: Joueur -> Environnement -> Batiment -> Environnement
terminerProduction j env bat =
  case btype bat of
    Usine ->
      case btempsProd bat of
        Just (t, utype) ->
          if t == 1
            then  -- creerUnite :: UniteType -> Joueur -> Coord -> Environnement -> Unite
                  -- findNearestGrass :: Coord -> Carte -> Maybe Coord
              let newCoord = fromJust(findNearestGrass  (bcoord bat) (ecarte env)) -- nearby the bat 
                  newUnit = creerUnite utype j newCoord env
                  newBatiment = bat {btempsProd = Nothing}
                  env' = updateUnite newUnit env
                  env'' = updateBatiment newBatiment env'
              in Debug.trace "termined Production" $ env''
            else
              let newBatiment = bat {btempsProd = Just (t-1, utype)}
                  env' = updateBatiment newBatiment env
              in Debug.trace "terminerProduction" $ terminerProduction j env' newBatiment
        Nothing -> env
    _ -> env

--Pres condition terminer production
prop_preTerminerProd:: Joueur -> Batiment -> Bool
prop_preTerminerProd j bat =
  let batType = (btype bat) == Usine
      batTempsProd = (btempsProd bat) /= Nothing
      proprio = (case (M.lookup (bid bat) (jbatiments j)) of
                    Just b -> True
                    Nothing -> False)
  in batType && batTempsProd && proprio 


-- Post condition terminer production
prop_postTerminerProd::Joueur->Environnement->Batiment->Bool
prop_postTerminerProd j env bat =
  let env'= terminerProduction j env bat
  in case btype bat of
      Usine -> 
        let (t,utype) = fromJust(btempsProd bat)
            constUnit= creerUnite utype j (bcoord bat) env
            newUnit = M.lookup (uid constUnit) (unites env')
            newBat = M.lookup (bid bat) (batiments env')
        in ((uid constUnit) == (uid (fromJust(newUnit)))) && (btempsProd (fromJust(newBat))== Nothing)

creerEnvironnement :: Carte -> [Coord] -> Environnement
creerEnvironnement carte qgCoords =
  let joueurIds = [1..length qgCoords]
      initialCredits = 1000
      initialJoueurs = [ Joueur { jid = JoueurId jid
                                , jcredits = initialCredits
                                , jbatiments = M.empty
                                , junites = M.empty
                                , jenergieConsume = 0
                                , jenergieProduit = 0
                                } | jid <- joueurIds ]
      initialEnv = Environnement { joueurs = initialJoueurs
                                 , ecarte = carte
                                 , unites = M.empty
                                 , batiments = M.empty
                                 }
      updatedEnv = Prelude.foldl (\env (j, qgCoord) -> construireBatiment j env QuartierGeneral qgCoord)
           initialEnv (zip initialJoueurs qgCoords)
  in Debug.trace ("Initial batiments: " ++ show (batiments updatedEnv)) updatedEnv

-- Calculate the total energy production and consumption of the player's building
energieTotal :: Joueur -> Int
energieTotal joueur = sum (Prelude.map benergie (M.elems (jbatiments joueur)))

-- Update player's building status 
updateEtatBatiments :: Joueur -> Joueur
updateEtatBatiments joueur =
  let totalEnergie = energieTotal joueur
      closeBatiment bat
        | btype bat `elem` [Raffinerie, Usine] =
            if totalEnergie < 0
               then bat { btempsProd = Nothing }
               else bat
        | otherwise = bat
      batimentsMisAJour = M.map closeBatiment (jbatiments joueur)
  in joueur { jbatiments = batimentsMisAJour }

-- Update building status for all players
updateEtatBatimentsForALL :: Environnement -> Environnement
updateEtatBatimentsForALL env =
  let joueursMisAJour = Prelude.map updateEtatBatiments (joueurs env)
  in env { joueurs = joueursMisAJour }

-- Update a single raffinerie
convertResourcesAtRaffinerie :: Batiment -> Environnement -> Environnement
convertResourcesAtRaffinerie raffinerie env
  | btype raffinerie /= Raffinerie = env -- If the building is not a raffinerie, do nothing
  | otherwise =
    let maybeCollector = findUniteAtCoord (bcoord raffinerie) env
    in case maybeCollector of
      Just unite
        | utype unite == Collecteur -> -- If there is a collector at the raffinerie's coordinates
          let cuve = fromJust (ucuve unite)
              credits = quantite cuve -- Convert the resources to credits
              newCollector = unite { ucuve = Just (Tank (capacite cuve) 0), ubut = Nothing } -- Empty the cuve and move the collector
              env' = moveCollectorToAdjacentCell newCollector env (bcoord raffinerie)
              joueur = fromJust (findJoueur (bproprio raffinerie) (joueurs env'))
              newJoueur = joueur { jcredits = jcredits joueur + credits }
          in Debug.trace "convertResourcesAtRaffinerie" $ updateJoueur newJoueur env'
        | otherwise -> env -- If the unit at the raffinerie's coordinates is not a collector, do nothing
      Nothing -> env -- If there is no unit at the raffinerie's coordinates, do nothing

-- Move a collector unit to an adjacent cell
moveCollectorToAdjacentCell :: Unite -> Environnement -> Coord -> Environnement
moveCollectorToAdjacentCell unite env coord =
  let newCoord = fromJust (findNearestGrass coord (ecarte env)) -- Find the nearest grass cell to the building
      newUnit = unite { ucoord = newCoord }
      env' = updateUnite newUnit env
  in Debug.trace "moveCollectorToAdjacentCell" env'


-- Convert the resources in the collectors present at the raffineries' coordinates
convertResourcesForALL :: Environnement -> Environnement
convertResourcesForALL env =
  let raffineries = M.elems $ M.filter (\b -> btype b == Raffinerie) (batiments env)
  in Prelude.foldl (\accEnv raffinerie -> convertResourcesAtRaffinerie raffinerie accEnv) env raffineries

-- Update the player's building status at the start of each turn
debutTour :: Environnement -> Environnement
debutTour env =
  let env' = updateEtatBatimentsForALL env
      env'' = terminerProductionForALL env'
      env2 = convertResourcesForALL env''
  in Debug.trace "debutTour" $ env2

-- End production of units for all players
terminerProductionForALL :: Environnement -> Environnement
terminerProductionForALL env =
  Prelude.foldl (\accEnv joueur ->
            Prelude.foldl (terminerProduction joueur
                  ) accEnv (M.elems (jbatiments joueur))
        ) env (joueurs env)
