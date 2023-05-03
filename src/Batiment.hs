{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
module Batiment where

import Common
import Carte
import Joueur
import qualified Data.Map as M
import Data.List
import Unite

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


construireBatiment :: Joueur -> Environnement -> BatimentType -> Coord -> Environnement
construireBatiment joueur env btype coord
  | not (isValidCoord (ecarte env) coord) || not (isConstructible coord (ecarte env)) = env
  | jcredits joueur < batimentTypeCost btype = env
  | otherwise = let
             newBatId = BatId (M.size (jbatiments joueur) + 1)
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

-- get the proprio joueur of batiment
joueurProprioBatiment :: Environnement -> Batiment -> Maybe Joueur
joueurProprioBatiment env bat =
  let myjid = bproprio bat
      js = joueurs env
  in Data.List.find (\j -> myjid == jid j) js


updateBatiment :: Batiment -> Environnement -> Environnement
updateBatiment updatedBatiment env =
  let currentPlayer = Data.List.head $ Data.List.filter (\j -> jid j == bproprio updatedBatiment) (joueurs env)
      updatedBatiments = M.insert (bid updatedBatiment) updatedBatiment (jbatiments currentPlayer)
      updatedPlayer = currentPlayer { jbatiments = updatedBatiments }
      updatedGlobalBatiments = M.insert (bid updatedBatiment) updatedBatiment (batiments env)
  in updateJoueur updatedPlayer (env { batiments = updatedGlobalBatiments })

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


--  détruit un bâtiment et supprime ses coordonnées de la carte
--detruireBatiment :: Environnement -> Batiment -> Environnement
