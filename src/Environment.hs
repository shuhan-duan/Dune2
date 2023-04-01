module Environment where

    import Carte
    import qualified Data.Map.Strict as M
    import Data.Map (Map)

    newtype JoueurId = JoueurId Int

    data Joueur = Joueur { jid :: JoueurId
                        , jcredits :: Int
                        , jcentrales :: [BatId]
                        , jraffineries :: [BatId]
                        , jusines :: [BatId]
                        }

    newtype BatId = BatId Int

    data BatimentType = QuartierGeneral | Raffinerie | Usine | Centrale

    data Batiment = Batiment { bid :: BatId
                            , btype :: BatimentType
                            , bproprio :: JoueurId
                            , bcoord :: Coord
                            , benergie :: Int
                            , bpointsVie :: Int
                            , btempsProd :: Maybe (Int, UniteType)
                            }
     bcoord :: Batiment -> Coord
    ucoord (Batiment _ _ _ c _ _ _ _) = c

    newtype UniteId = UniteId Int

    data UniteType = Tank | Soldat | Trike

    data Unite = Unite { uid :: UniteId
                    , utype :: UniteType
                    , uproducteur :: BatId
                    , uproprio :: JoueurId
                    , ucoord :: Coord
                    , udirection :: Int  -- angle en degrés, modulo 360
                    , upointsVie :: Int
                    }
    ucoord :: Unite -> Coord
    ucoord (Unite _ _ _ _ _ c _ _) = c


data Environnement = Environnement{
                joueurs :: [Joueur],
                ecarte :: Carte,
                unites :: Map UniteId Unite ,
                batiments :: Map BatId Batiment
                }
 -- |Rechercher un element dans une liste
    etsDans::a->[a]->Bool
        etsDans a l=foldl (\acc elem-> ((jid elem)==a) || acc) false l

    -- |verifier qu'un batiment appartient a un joueur de environnement
    prop_bat_appartient::Environnement->Bool
        prop_bat_appartient (Env j _ _ b)= M.foldr (\elem acc-> etsDans (bproprio elem) j || acc) false b

    -- |verifier qu'une uninte appartient a un joueur de environnement
    prop_unit_appartient::Environnement->Bool
        prop_unit_appartient (Env j _ u _)= M.foldr (\x acc-> etsDans (uproprio elem) j || acc) false unites


    prop1_env::Environnement->Bool
        prop1_env= prop_bat_appartient && prop_unit_appartient

    -- |verifier que les cases au de la carte sont vide
    coordVide1::Coord->Carte->Bool
        coordVide1 e (Carte c)  = M.foldrWithKey (prim k elem acc) true c where
                            prim k elem acc= case elem of
                                                Eau-> (k\=e)&& acc
                                                otherwase-> true
   
    prop_caseEauVide::Environnement->Bool
        prop_caseEauVide (Env _ c u b) = (M.foldr (prim1 elem1 acc) true u) && (M.foldr (prim2 elem2 acc) true b) where
                                            prim1 x a = (coordVide1 (ucoord x) c) && a
                                            prim2 y b = (coordVide1 (bcoord y) c) && b
                                                            
    -- |verifier qu'une case herbe contient au plus un batiment ou une entite(mais pas les deux a la fois)
     coordVide2::Coord->Carte->Bool
         coordVide2 e (Carte c)  = M.foldrWithKey (prim k elem acc) true c where
                            prim k elem acc= case elem of
                                                Herbe-> (k\=e) && acc
                                                otherwase-> true
   

     coordNonVide::Coord->Carte->Bool
         coordNonVide e (Carte c)  = M.foldrWithKey (prim k elem acc) false c where
                            prim k elem acc= case elem of
                                                Herbe-> (k==e) || acc
                                                otherwase-> false
    
    contient_bat_OU_unit::Environnement->Bool
        contient_bat_OU_unit  (Env _ c u b) = M.foldr (prim elem acc) false b where 
                                                    prim e a = (coordNonVide (bcoord e) c) || a || (M.foldr (prim1 elem1 acc1) false u) where
                                                            prim1 x b = ((x\=e) && (coordNonVide (ucoord x) c)) || b

    prop_caseHerbe::Environnement->Bool
        prop_caseHerbe (E _ c u b) @ e= (contient_bat_OU_unit e) || (prim e) where
                                            prim (E _ c' u' b')=  (M.foldr (prim1 elem1 acc) true u') && (M.foldr (prim2 elem2 acc) true b')
                                            prim1 x a = (coordVide2 (ucoord x) c) && a
                                            prim2 y b = (coordVide2 (bcoord y) c) && b

    -- |verifier que chaque case de ressource contient au plus une entite
    coordVide3::Coord->Carte->Bool
         coordVide3 e (Carte c) = M.foldrWithKey (prim k elem acc) true c where
                            prim k elem acc= case elem of
                                                Ressource _-> (k\=e) && acc
                                                otherwase-> true
   

    coordNonVide1::Coord->Carte->Bool
         coordNonVide e (Carte c) = M.foldrWithKey (prim k elem acc) false c where
                            prim k elem acc= case elem of
                                                Ressource _-> (k==e) || acc
                                                otherwase-> false
                                    
    verifRessource::Environnement->Bool
        (Env _ c u b) = M.foldr (prim elem acc) true u && M.foldr (prim1 elem acc1) true b where
                            prim x a = (coordNonVide1 (ucoord x) c) && a 
                            prim1 y b' = (coordVide3 (bcoord y) c) && b'
    
    prop_caseRessource::Environnement->Bool
        prop_caseRessource (E _ c u b) @ e= (verifRessource e) ||(prim e) where
                                            prim (E _ c' u' b')=  (M.foldr (prim1 elem1 acc) true u') && (M.foldr (prim2 elem2 acc) true b')
                                            prim1 x a = (coordVide3 (ucoord x) c) && a
                                            prim2 y b = (coordVide3 (bcoord y) c) && b

    -- |invariant pour l'environnement 
    prop_inv_env::Environnement->Bool
        prop_inv_env =  prop1_env &&  prop_caseEauVide &&  prop_caseHerbe && prop_caseRessourc

