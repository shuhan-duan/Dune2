{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Environnement
where
import Common
import Carte
import qualified Data.Map as M
import Joueur
import qualified Data.List

prop_Entites_correctes :: Environnement -> Bool
prop_Entites_correctes env =
    all (`prop_Unite_correcte` env) (unites env) &&
    all (`prop_Batiment_correct` env) (batiments env)

prop_Unite_correcte :: Unite -> Environnement -> Bool
prop_Unite_correcte unite env =
    let coord = ucoord unite
        joueurId = uproprio unite
    in isValidCoord (ecarte env) coord && joueurExiste joueurId env && upointsVie unite > 0


prop_Batiment_correct :: Batiment -> Environnement -> Bool
prop_Batiment_correct bat env =
    let coord = bcoord bat
        joueurId = bproprio bat
    in isValidCoord (ecarte env) coord &&
       not (isEau (getTerrain (ecarte env) coord)) &&
       joueurExiste joueurId env &&
       bpointsVie bat >= 0

prop_EntitesAppartenanceCorrecte :: Environnement -> Bool
prop_EntitesAppartenanceCorrecte env =
  all (\u -> uproprio u `joueurExiste` env) (unites env) &&
  all (\b -> bproprio b `joueurExiste` env) (M.elems $ batiments env)


prop_EauVide :: Environnement -> Bool
prop_EauVide env = not (any (isEau . getTerrain (ecarte env)) (M.keys $ carte $ ecarte env))



prop_HerbeUniteOuBatiment :: Environnement -> Bool
prop_HerbeUniteOuBatiment env =
    all (\coord -> case getTerrain (ecarte env) coord of
                      Just Herbe -> M.size (M.filter (\u -> ucoord u == coord) (unites env)) +
                                    M.size (M.filter (\b -> bcoord b == coord) (batiments env)) <= 1
                      _ -> True) (M.keys (carte (ecarte env)))

prop_RessourceUnite :: Environnement -> Bool
prop_RessourceUnite env =
    all (\coord -> case getTerrain (ecarte env) coord of
                      Just (Ressource _) -> M.size (M.filter (\u -> ucoord u == coord) (unites env)) <= 1
                      _ -> True) (M.keys (carte (ecarte env)))


envInvariant :: Environnement -> Bool
envInvariant env = prop_Entites_correctes env
                   && prop_EntitesAppartenanceCorrecte env
                   && prop_EauVide env
                   && prop_HerbeUniteOuBatiment env
                   && prop_RessourceUnite env

updateJoueur :: Joueur -> Environnement -> Environnement
updateJoueur newj env =
  let newjs = map (\j -> if jid j == jid newj then newj else j) (joueurs env)
  in env { joueurs = newjs } 

-- update the environment with new batiment
updateBatiment :: Batiment -> Environnement -> Environnement
updateBatiment updatedBatiment env =
  let currentPlayer = Data.List.head $ Data.List.filter (\j -> jid j == bproprio updatedBatiment) (joueurs env)
      updatedBatiments = M.insert (bid updatedBatiment) updatedBatiment (jbatiments currentPlayer)
      updatedPlayer = currentPlayer { jbatiments = updatedBatiments }
      updatedGlobalBatiments = M.insert (bid updatedBatiment) updatedBatiment (batiments env)
  in updateJoueur updatedPlayer (env { batiments = updatedGlobalBatiments })

-- détruit un bâtiment et supprime ses coordonnées de la carte
detruireBatiment :: Environnement -> Batiment -> Environnement
detruireBatiment env bat =
    let updatedBatiments = M.delete (bid bat) (batiments env)
        updatedCarte = setCaseVide (bcoord bat) (ecarte env)
        currentPlayer = Data.List.head $ Data.List.filter (\j -> jid j == bproprio bat) (joueurs env)
        updatedPlayerBatiments = M.delete (bid bat) (jbatiments currentPlayer)
        updatedPlayer = currentPlayer { jbatiments = updatedPlayerBatiments }
        updatedEnv = env { batiments = updatedBatiments, ecarte = updatedCarte }
    in updateJoueur updatedPlayer updatedEnv

-- update the environment with new unite
updateUnite :: Unite -> Environnement -> Environnement
updateUnite updatedUnite env =
  let currentPlayer = head $ filter (\j -> jid j == uproprio updatedUnite) (joueurs env)
      updatedUnites = M.insert (uid updatedUnite) updatedUnite (junites currentPlayer)
      updatedPlayer = currentPlayer { junites = updatedUnites }
      updatedGlobalUnites = M.insert (uid updatedUnite) updatedUnite (unites env)
  in updateJoueur updatedPlayer (env { unites = updatedGlobalUnites })


removeUnite :: Unite -> Environnement -> Environnement
removeUnite unite env =
  let currentPlayer = head $ filter (\j -> jid j == uproprio unite) (joueurs env)
      updatedUnites = M.delete (uid unite) (junites currentPlayer)
      updatedPlayer = currentPlayer { junites = updatedUnites }
      updatedGlobalUnites = M.delete (uid unite) (unites env)
  in updateJoueur updatedPlayer (env { unites = updatedGlobalUnites })

getUnite::UniteId->Environnement->Unite
getUnite u env =
    let unite = M.lookup u (unites env)
    in case unite of
        Just unite'-> unite'
        Nothing -> error "undifined"

getBatiment:: BatId->Environnement->Batiment
getBatiment b env=
    let batiment = M.lookup b (batiments env)
    in case batiment of
        Just batiment'-> unite'
        Nothing -> error "undifined"

isMissingBat::Batiment->Environnement->Bool
isMissingBat batiment env =
  let verif = M.lookup (bid batiment) (batiments env)
  in case verif of 
      Just _ -> True
      Nothing -> False

isMissingUnit::Unite->Environnement->Bool
isMissingUnit unite env =
   let verif = M.lookup (uid batiment) (unites env)
   in case verif of 
      Just _ -> True
      Nothing -> False






