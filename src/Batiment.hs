{-# LANGUAGE BlockArguments #-}
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


-- Construit un bâtiment sur une case donn ́ee, si possible.
construireBatiment :: Joueur -> Environnement -> BatimentType -> Coord -> Maybe (Joueur, Batiment)
construireBatiment j env bt c
    | bTypeCost > credits = Nothing
    | not $ isConstructible c (ecarte env) = Nothing
    | otherwise = Just (newJoueur, Batiment bid bt (jid j) c bEnergie bPointsVie Nothing)
  where
    credits = jcredits j
    bid = getNextBatimentId j
    bTypeCost = batimentTypeCost bt
    bPointsVie = batimentTypePointsVie bt
    bEnergie = batimentTypeEnergie bt
    newJoueur = j { jcredits = credits - bTypeCost
                  , jbatiments = insert bid (Batiment bid bt (jid j) c bEnergie bPointsVie Nothing) (jbatiments j)
                  , jenergieProduit = jenergieProduit j + bEnergie
                  , jenergieConsume = jenergieConsume j + (if bEnergie < 0 then -bEnergie else 0)
                  }

                        

-- Modifie les points de vie d'un bâtiment en ajoutant la valeur de deltaPoints.
-- Si le résultat est inférieur à 0, les points de vie sont fixés à 0.    
modifierPointsVie :: Int -> Batiment -> Batiment
modifierPointsVie deltaPoints batiment = batiment { bpointsVie = max 0 (bpointsVie batiment - deltaPoints) }

-- get the proprio joueur of batiment
joueurProprioBatiment :: Environnement -> Batiment -> Maybe Joueur
joueurProprioBatiment env bat =
  let myjid = bproprio bat
      js = joueurs env
  in find (\j -> myjid == jid j) js


-- produce unite si le batiment est "Usine"
produireUnite :: Environnement -> UniteType -> Batiment -> (Environnement, Batiment)
produireUnite env utype bat = 
    case btype bat of 
         Usine -> case btempsProd bat of
            Just _ -> (env, bat)  -- L'usine est déjà en train de produire une unité
            Nothing ->  if jcredits j < uniteTypeCost utype
                            then (env, bat) -- Le joueur n'a pas suffisamment de points pour payer les coûts de production
                            else let newBat = bat { btempsProd = Just (uniteTypeTempsProd utype, utype)}
                                     newJoueur = j { jcredits = jcredits j - uniteTypeCost utype }
                                     env' = env { joueurs = insert (jid j) newJoueur (joueurs env) }
                                in (env', newBat)
            where j = joueurProprioBatiment env bat
         _ -> (env, bat) -- Le bâtiment n'est pas de type "Usine", on ne peut pas effectuer de production d'unité 

-- termine la production de unite 
terminerProduction :: Joueur -> Batiment -> (Joueur, Batiment)
terminerProduction j  bat =
  case btype bat of
    Usine ->
      case btempsProd bat of
        Just (t, utype) ->
          if t == 1 
            then
              let newUnit = creerUnite utype j (bcoord bat)
                  newJoueur = j {junites = insert (uid newUnit) newUnit (junites j)} -- mettre a jour le joueur
              in (newJoueur, bat {btempsProd = Nothing, bproprio = jid newJoueur})
            else terminerProduction j (bat {btempsProd = Just (t-1, utype)})
        Nothing -> (j, bat)
    _ -> (j, bat)
