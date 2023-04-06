module Unite where
    
import Carte as C
import Data.List as L
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
uniteTypeCost Combatnt = 10 

-- Returns the product time of a given unite type.
uniteTypeTempsProd :: UniteType -> Int
uniteTypeTempsProd Collecteur = 20 
uniteTypeTempsProd Combatnt = 15


prop1_inv_Unite:: Unite -> Bool
prop1_inv_Unite u = L.elem (ubut u) (uTache u)

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
 
collecterRessource::Coord->Unite->Unite
collecterRessource c u =   