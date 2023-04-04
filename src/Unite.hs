module Unite where
    
    import Carte
    import Data.List as L

    newtype UniteId = UniteId Int

    data Tank = Tank Int Int
                |EmptyTank Int
                |FullTank Int
                deriving Show
    
    prop_cuveInvariant :: Maybe Tank -> Bool
        prop_cuveInvariant (Just (EmptyTank cap)) = cap >= 0
        prop_cuveInvariant (Just (FullTank cap)) = cap >= 0
        prop_cuveInvariant (Just (Tank cap cour)) = cap > 0 && cour > 0 && cour <= cap

    initCuve :: Integer -> Maybe Tank
        initCuve c 
        | c > 0 = Just $ EmptyTank c
        | otherwise = Nothing 
    quantite ::Tank -> Integer
        quantite (Cuve _ q) = q
        quantite (CuveVide c) = 0
        quantite (CuvePleine c) = c

    capacite ::Tank -> Integer
        capacite (Cuve c _) = c
        capacite (CuveVide c) = c
        capacite (CuvePleine c) = c

    changeCuve :: Tank -> Integer -> Tank
        changeCuve cu q
            | q == 0 = EmptyTank (capacite cu) 
            | q == capacite cu = FullTank (capacite cu)
            | otherwise  = Tank (capacite cu) q

    -- v: volume
    remplirCuve ::Tank -> Integer -> Maybe Tank
        remplirCuve _ v | v <= 0 = Nothing 
        remplirCuve cu v = 
            let q = v + quantite cu in 
                if q <= capacite cu
                    then Just $ changeCuve cu q
                    else Nothing




    data UniteType = Collecteur Tank 
                    | Combatnt
    data Ordre = CR 
                |D 
                |P
                |A 
                deriving Eq

    
    data Unite = Unite { uid :: UniteId
                    , utype :: UniteType
                    , uproducteur :: BatId
                    , ucoord :: Coord
                    , udirection :: Int  -- angle en degrés, modulo 360
                    , upointsVie :: Int
                    ,uTache :: [Ordre]
                    ,ubut :: Ordre
                    }



prop1_inv_Unite:: Unite -> Bool
prop1_inv_Unite (Unite _ _ _ _ _ _ _ t b) = L.elem t b

modifCoordEO::Unite->Unite
modifCoord u= let c= Coord (((cx (ucoord u)+1),cy (ucoord u))) in u{ucoord=c} 

modifCoordEO::Unite->Unite
modifCoord u= let c= Coord ((cx (ucoord u),(cy (ucoord u))+1)) in u{ucoord=c} 

deplacer:: Coord->Unite->Unite
deplacer c u = let x= abs ((cx c) - (cx (cx (ucoord u)))) ,y= abs ((cy c) - (cy (cy (ucoord u)))) in
                                        case (x > y) of
                                            True -> modifCoordEO u
                                            otherwase -> modifCoordNS u 
