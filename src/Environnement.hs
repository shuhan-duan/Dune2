{-# OPTIONS_GHC -Wno-missing-export-lists #-}
import Common
import Carte
import qualified Data.Map as M
import Joueur

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
    

