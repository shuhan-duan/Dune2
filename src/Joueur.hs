{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Joueur where

import Common
import qualified Data.Map as Map
import Data.Map (keys)

jcentrales :: Joueur -> [BatId]
jcentrales joueur = [bid | (bid, batiment) <- Map.toList (jbatiments joueur), btype batiment == Centrale]

jraffineries :: Joueur -> [BatId]
jraffineries joueur = [bid | (bid, batiment) <- Map.toList (jbatiments joueur), btype batiment == Raffinerie]

jusines :: Joueur -> [BatId]
jusines joueur = [bid | (bid, batiment) <- Map.toList (jbatiments joueur), btype batiment == Usine]


getNextBatimentId :: Joueur -> BatId
getNextBatimentId j = BatId $ maximum (map unBatId $ keys (jbatiments j)) + 1

getNextUniteId :: Joueur -> UniteId
getNextUniteId j = UniteId $ maximum (map unUniteId $ keys $ junites j) + 1

joueurExiste :: JoueurId -> Environnement -> Bool
joueurExiste joueurId env = joueurId `elem` fmap jid (joueurs env)

updateJoueur :: Joueur -> Environnement -> Environnement
updateJoueur newj env =
  let newjs = map (\j -> if jid j == jid newj then newj else j) (joueurs env)
  in env { joueurs = newjs }

