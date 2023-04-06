module Environment where

import Carte
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Common



 -- |Rechercher un element dans une liste
etsDans::JoueurId->[Joueur]->Bool
etsDans a l=foldl (\acc elem-> ((jid elem)== a) || acc) False l

-- |verifier qu'un batiment appartient a un joueur de environnement
prop_bat_appartient::Environnement->Bool
prop_bat_appartient (Environnement j _ _ b)= M.foldr (\elem acc-> etsDans (bproprio elem)  j || acc) False b

-- |verifier qu'une uninte appartient a un joueur de environnement
prop_unit_appartient::Environnement->Bool
prop_unit_appartient (Environnement j _ u _)= M.foldr (\elem acc-> etsDans (uproprio elem) j || acc) False u


prop1_env::Environnement->Bool
prop1_env e= prop_bat_appartient e &&  (prop_unit_appartient e)

-- |verifier que les cases au de la carte sont vide
coordVide1::Coord->Carte->Bool
coordVide1 e  c  = M.foldrWithKey ($ prim elem acc) True c where
                            prim k elem acc= case elem of
                                                Eau-> (k/=e)&& acc
                                                otherwase-> True

prop_caseEauVide::Environnement->Bool
prop_caseEauVide (Environnement _ c u b) = (M.foldr (prim1 elem1 acc) True u) && (M.foldr (prim2 elem2 acc) True b) where
                                            prim1 x a = (coordVide1 (ucoord x) c) && a
                                            prim2 y b = (coordVide1 (bcoord y) c) && b

-- |verifier qu'une case herbe contient au plus un batiment ou une entite(mais pas les deux a la fois)
coordVide2::Coord->Carte->Bool
coordVide2 e  c  = M.foldrWithKey (prim k elem acc) true c where
                            prim k elem acc= case elem of
                                                Herbe-> (k/=e) && acc
                                                otherwase-> True


coordNonVide::Coord->Carte->Bool
coordNonVide e  c  = M.foldrWithKey (prim k elem acc) false c where
                            prim k elem acc= case elem of
                                                Herbe-> (k==e) || acc
                                                otherwase-> False

contient_bat_OU_unit::Environnement->Bool
contient_bat_OU_unit  (Environnement _ c u b) = M.foldr (prim elem acc) false b where
                                                    prim e a = (coordNonVide (bcoord e) c) || a || (M.foldr (prim1 elem1 acc1) false u) where
                                                            prim1 x b = ((x\=e) && (coordNonVide (ucoord x) c)) || b

prop_caseHerbe::Environnement->Bool
prop_caseHerbe (Environnement _ c u b) '@' e= (contient_bat_OU_unit e) || (prim e) where
                                            prim (Environnement _ c' u' b')=  (M.foldr (prim1 elem1 acc) True u') && (M.foldr (prim2 elem2 acc) True b')
                                            prim1 x a = (coordVide2 (ucoord x) c) && a
                                            prim2 y b = (coordVide2 (bcoord y) c) && b

-- |verifier que chaque case de ressource contient au plus une entite
coordVide3::Coord->Carte->Bool
coordVide3 e  c = M.foldrWithKey (prim k elem acc) True c where
                            prim k elem acc= case elem of
                                                Ressource _-> (k/=e) && acc
                                                otherwase-> True


coordNonVide1::Coord->Carte->Bool
coordNonVide1 e (Carte c) = M.foldrWithKey (prim k elem acc) False c where
                            prim k elem acc= case elem of
                                                Ressource _-> (k==e) || acc
                                                otherwase-> False

verifRessource::Environnement->Bool
verifRessource (Environnement _ c u b) = M.foldr (prim elem acc) True u && M.foldr (prim1 elem acc1) True b where
                            prim x a = (coordNonVide1 (ucoord x) c) && a
                            prim1 y b' = (coordVide3 (bcoord y) c) && b'

prop_caseRessource::Environnement->Bool
prop_caseRessource (Environnement _ c u b) '@' e= (verifRessource e) ||(prim e) where
                                            prim (Environnement _ c' u' b')=  (M.foldr (prim1 elem1 acc) true u') && (M.foldr (prim2 elem2 acc) true b')
                                            prim1 x a = (coordVide3 (ucoord x) c) && a
                                            prim2 y b = coordVide3 (bcoord y) c && b

-- |invariant complete pour l'environnement 
prop_inv_env::Environnement->Bool
prop_inv_env =  prop1_env &&  prop_caseEauVide &&  prop_caseHerbe && prop_caseRessourc

