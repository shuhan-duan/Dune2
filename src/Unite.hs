module Unite where
    
import Carte as C
import Data.List as L
import qualified Data.Map as M
import Common
    
prop_cuveInvariant :: Maybe Tank -> Bool
prop_cuveInvariant (Just (EmptyTank cap)) = cap >= 0
prop_cuveInvariant (Just (FullTank cap)) = cap >= 0
prop_cuveInvariant (Just (Tank cap cour)) = cap > 0 && cour > 0 && cour <= cap

initCuve :: Int -> Maybe Tank
initCuve cap 
    | cap > 0 = Just $ EmptyTank cap
    | otherwise = Nothing 

quantite ::Tank -> Int
quantite (Tank _ q) = q
quantite (EmptyTank cap) = 0
quantite (FullTank cap) = cap

capacite ::Tank -> Int
capacite (Tank c _) = c
capacite (EmptyTank c) = c
capacite (FullTank c) = c

changeCuve :: Tank -> Int -> Tank
changeCuve cu q
        | q == 0 = EmptyTank (capacite cu) 
        | q == capacite cu = FullTank (capacite cu)
        | otherwise  = Tank (capacite cu) q

-- v: volume
remplirCuve ::Tank -> Int -> Maybe Tank
remplirCuve _ v | v <= 0 = Nothing 
remplirCuve cu v = 
    let q = v + quantite cu in 
        if q <= capacite cu then 
        Just $ changeCuve cu q
                    else Nothing



-- Returns the cost (in credits) of a given unite type.
uniteTypeCost :: UniteType -> Int
uniteTypeCost Collecteur = 10 
uniteTypeCost Combatant = 10 

-- Returns the product time of a given unite type.
uniteTypeTempsProd :: UniteType -> Int
uniteTypeTempsProd Collecteur = 20 
uniteTypeTempsProd Combatnt = 15


prop1_inv_Unite:: Unite -> Bool
prop1_inv_Unite u = L.elem (ubut u) (uTache u)


prop_Ordre_Collecteur::Unite->Bool
prop_Ordre_Collecteur u = case (uid u) of 
                        Collecteur t-> foldr(\elem acc -> (aux elem) && acc ) True (uTache u) where
                                aux elem= case elem of
                                        Collecter a -> True
                                        Deplacer a -> True
                                        otherwise -> False
                        otherwase -> False
prop_Ordre_Combattant::Unite->Bool
prop_Ordre_Combattant u = case (uid u) of 
                        Collecteur t-> foldr(\elem acc -> (aux elem) && acc ) True (uTache u) where
                                aux elem= case elem of
                                        Patrouiller a b -> True
                                        Deplacer a -> True
                                        otherwise -> False
                        otherwase -> False

modifCoordEO::Unite->Unite
modifCoordEO u = 
    let x= (cx (ucoord u))+1  
        y=(cy (ucoord u))
    in u{ucoord=C.creeCoord x y} 

modifCoordNS::Unite->Unite
modifCoordNS u= 
    let x= (cx (ucoord u))  
        y=(cy (ucoord u)) +1 
    in u{ucoord=C.creeCoord x y} 

deplacer:: Coord -> Unite -> Unite
deplacer c u =
  let x = abs (cx c - cx (ucoord u))
      y = abs (cy c - cy (ucoord u))
  in case x > y of
       True -> modifCoordEO u
       False -> modifCoordNS u

-- | Fonction pour modifier le but d'une Unite
modif_But :: Ordre->Unite->Unite
modif_But o u = u{ubut=o}
 
collecterRessource::Coord->Unite->Environnement->Unite
collecterRessource coord (Unite id j b c d v t but)@ u e = case  (M.lookup coord (ecarte e)) of
                            Nothing->Nothing
                            Just a-> case a of
                                    Ressource n-> case id of
                                                 Collecteur t-> Unite (Collecteur (remplirCuve t)) j b c d v t but 
                                                otherwase-> u
                                    otherwase->u 

-- |Verifier qu'une unite collecteur est pleine 
collecteurPlein::UniteId-> Maybe Bool
collecteurPlein u = case u of
                    Collecteur t -> case t of   
                                    FullTank -> Just True
                                    otherwase -> Just False
                    otherwase->Nothing

-- | 
getCoordCollecte::[Ordre]->Maybe Coord
getCoordCollecte []=Nothing
getCoordCollecte x:xs = case x of
                        

changeButCollecteur::Unite->Environment->Unite
changeButCollecte (Unite id j b c d v t but) @ u e = case (collecteurPlein (uid u)) of 
                        Nothing-> u
                        Just False -> case (M.lookup (ucoord u) (ecarte e)) of
                                      Nothing-> u
                                      Just a-> case a of
                                               Ressource n-> Unite id j b c d v t (Collecter (ucoord u))
                        Just True -> -- | probleme avec la raffinerie 

-- | definir la porter 
porter::Coord->Coord->Bool
porter (C x y) (C x' y')= ((x-x')**2)+ ((y-y')**2) <= 4

-- | je fixe le nombre de vie a retirer a chaque attaque a 2

-- | Attaquer une unite
attaquerU::Unite->Unite->(Unite,Unite)
attaqueU (Unite id j b c d v t but)  (Unite id1 j1 b1 c1 d1 v1 t1 but1) = if (v1) < 2 then 
                 let x = Unite id j b c d (v+v1) t but 
                    y= Unite id1 j1 b1 c1 d1 0 t1 bu1t in (x,y)
                else   let x = Unite id j b c d (v+2) t but 
                           y= Unite id j b c d (v1-2) t but in (x,y)
-- | Attaquer un batiment
attaqueB::Unite->Batiment->(Unite,Batiment)
attaqueU (Unite id j b c d v t but)  (Batiment id1 type j1  c1 e v1 t1) @ = if (v1) < 2 then 
                 let x = Unite id j b c d (v+v1) t but 
                    y= Batiment id type j1 c1 0 t1 in (x,y)
                else   let x = Unite id j b c d (v+2) t but 
                           y= Batiment  id type j1 c1 (v1-2) t1 in (x,y)
-- |
attaque::Unite->Entite->(Unite,Entite)
attaque u e= case e of
            Batiment -> attaqueB u e
            Unite ->attaquerU u e

-- | verifier si une entite est ennemie
ennemie::Unite->Entite->Bool
ennemie u e = case e of 
            Batiment -> (uproprio u) \= (bproprio e)
            Unite -> (uproprio u) \= (uproprio e)

patrouillerA::Unite->Environnement->Unite
patrouillerA u e= let ordre= (head (uTache u)) in 
                 case ordre of
                    Patrouiller a b -> 



