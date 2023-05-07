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
import Debug.Trace

-- Returns the cost (in credits) of a given building type.
batimentTypeCost :: BatimentType -> Int
batimentTypeCost QuartierGeneral = 100
batimentTypeCost Raffinerie = 50
batimentTypeCost Usine = 75
batimentTypeCost Centrale = 100

-- Returns the maximum number of hit points for a given building type.
batimentTypePointsVie :: BatimentType -> Int
batimentTypePointsVie QuartierGeneral = 500
batimentTypePointsVie Raffinerie = 100
batimentTypePointsVie Usine = 200
batimentTypePointsVie Centrale = 150

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
             env'' = updateBatiment newBatiment env'
             in env''

-- Modifie les points de vie d'un bâtiment en ajoutant la valeur de deltaPoints.
-- Si le résultat est inférieur à 0, les points de vie sont fixés à 0.    
modifierPointsVie :: Int -> Batiment -> Batiment
modifierPointsVie deltaPoints batiment = batiment { bpointsVie = max 0 (bpointsVie batiment - deltaPoints) }

-- produce unite si le batiment est "Usine"
produireUnite :: Environnement -> UniteType -> Batiment -> Environnement
produireUnite env utype bat =
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
                                        in env''
            where j = joueurProprioBatiment env bat
         _ -> env -- Le bâtiment n'est pas de type "Usine", on ne peut pas effectuer de production d'unité


-- termine la production de unite 
terminerProduction :: Joueur -> Environnement -> Batiment -> Environnement
terminerProduction j env bat =
  case btype bat of
    Usine ->
      case btempsProd bat of
        Just (t, utype) ->
          if t == 1
            then
              let newUnit = creerUnite utype j (bcoord bat)
                  newBatiment = bat {btempsProd = Nothing}
                  env' = updateUnite newUnit env
                  env'' = updateBatiment newBatiment env'
              in env''
            else
              let newBatiment = bat {btempsProd = Just (t-1, utype)}
                  env' = updateBatiment newBatiment env
              in terminerProduction j env' newBatiment
        Nothing -> env
    _ -> env

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
  in trace ("Initial batiments: " ++ show (batiments updatedEnv)) updatedEnv


executerCommande :: Commande -> Environnement -> Environnement
executerCommande cmd env =
  case cmd of
    DonnerOrdre uid ordre -> -- Update the unit with the new order
      let unite = M.lookup uid (unites env)
      in case unite of
        Just u -> updateUnite (u {uordres = ordre : uordres u}) env
        Nothing -> env
    Construire jid btype coord -> -- Build a new building at the specified coordinates
      let joueur = findJoueur jid (joueurs env)
      in case joueur of
        Just j -> construireBatiment j env btype coord
        Nothing -> env
    Produire jid utype -> -- Produce a new unit
      let joueur = findJoueur jid (joueurs env)
          usine = findUsineForJoueur joueur (batiments env)
      in case usine of
        Just u -> produireUnite env utype u
        Nothing -> env

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

-- Update the player's building status at the start of each turn
debutTour :: Environnement -> Environnement
debutTour env =
  let env' = updateEtatBatimentsForALL env
      env'' = terminerProductionForALL env'
  in env''

-- End production of units for all players
terminerProductionForALL :: Environnement -> Environnement
terminerProductionForALL env =
  Prelude.foldl (\accEnv joueur ->
            Prelude.foldl (terminerProduction joueur
                  ) accEnv (M.elems (jbatiments joueur))
        ) env (joueurs env)

findUsineForJoueur :: Maybe Joueur -> Map BatId Batiment -> Maybe Batiment
findUsineForJoueur Nothing _ = Nothing
findUsineForJoueur (Just joueur) batiments =
  let usines = M.filter (\b -> btype b == Usine && bproprio b == jid joueur && isNothing (btempsProd b)) batiments
  in if M.null usines then Nothing else Just (snd (M.findMin usines))       

findRaffinerieForJoueur :: Maybe Joueur -> Map BatId Batiment -> Maybe Batiment
findRaffinerieForJoueur Nothing _ = Nothing
findRaffinerieForJoueur (Just joueur) batiments =
  let raffs = M.filter (\b -> btype b == Raffinerie && bproprio b == jid joueur && isNothing (btempsProd b)) batiments
  in if M.null raffs then Nothing else Just (snd (M.findMin raffs))       