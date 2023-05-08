module Joueur where

import Common
import qualified Data.Map as Map
import Data.Map (keys)
import qualified Data.Map as M
import qualified Data.List
import Data.List (find)

jcentrales :: Joueur -> [BatId]
jcentrales joueur = [bid | (bid, batiment) <- Map.toList (jbatiments joueur), btype batiment == Centrale]

jraffineries :: Joueur -> [BatId]
jraffineries joueur = [bid | (bid, batiment) <- Map.toList (jbatiments joueur), btype batiment == Raffinerie]

jusines :: Joueur -> [BatId]
jusines joueur = [bid | (bid, batiment) <- Map.toList (jbatiments joueur), btype batiment == Usine]


joueurExiste :: JoueurId -> Environnement -> Bool
joueurExiste joueurId env = joueurId `elem` fmap jid (joueurs env)

findRaffinerie :: Environnement -> JoueurId -> Maybe Batiment
findRaffinerie env jid = M.foldr checkBatiment Nothing (batiments env)
  where
    checkBatiment b acc =
      if bproprio b == jid && btype b == Raffinerie
      then Just b
      else acc

-- get the proprio joueur of batiment
joueurProprioBatiment :: Environnement -> Batiment -> Maybe Joueur
joueurProprioBatiment env bat =
  let myjid = bproprio bat
      js = joueurs env
  in Data.List.find (\j -> myjid == jid j) js



