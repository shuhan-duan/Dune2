{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Unite where

import Common
import Carte
import Joueur
import qualified Data.Map as M
import Environnement

    
prop_cuveInvariant :: Maybe Tank -> Bool
prop_cuveInvariant Nothing = False
prop_cuveInvariant (Just (EmptyTank cap)) = cap >= 0
prop_cuveInvariant (Just (FullTank cap)) = cap >= 0
prop_cuveInvariant (Just (Tank cap cour)) = cap > 0 && cour > 0 && cour <= cap

initCuve :: Int -> Maybe Tank
initCuve cap 
    | cap > 0 = Just $ EmptyTank cap
    | otherwise = Nothing 

quantite ::Tank -> Int
quantite (Tank _ q) = q
quantite (EmptyTank _) = 0
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

capaciteCuve :: UniteType -> Int
capaciteCuve (Collecteur t) = case t of
                                EmptyTank c -> c
                                FullTank c -> c
                                Tank c _ -> c
capaciteCuve Combatant = 0 -- Les combattants ne peuvent pas collecter de ressources

-- Returns the cost (in credits) of a given unite type.
uniteTypeCost :: UniteType -> Int
uniteTypeCost (Collecteur _) = 10 
uniteTypeCost Combatant = 10 

-- Returns the product time of a given unite type.
uniteTypeTempsProd :: UniteType -> Int
uniteTypeTempsProd (Collecteur _)  = 20 
uniteTypeTempsProd Combatant = 15

-- Returns the point of life  of a given unite type.
uniteTypePointsVie :: UniteType -> Int
uniteTypePointsVie (Collecteur _) = 100
uniteTypePointsVie Combatant = 50

-- Returns the capacity of the cuve of a given unite type.
uniteTypeCapaciteCuve :: UniteType -> Int
uniteTypeCapaciteCuve (Collecteur tank) = capacite tank
uniteTypeCapaciteCuve Combatant = 0 -- Combatants do not have tanks

-- create new unite 
creerUnite :: UniteType -> Joueur -> Coord -> Unite
creerUnite utype joueur coord =
  Unite {
    uid = getNextUniteId joueur,
    utype = utype,
    uproprio = jid joueur,
    ucoord = coord,
    udirection = Nord,
    upointsVie = uniteTypePointsVie utype,
    ucuve = initCuve (uniteTypeCapaciteCuve utype),
    uordres = [],
    ubut = Deplacement coord
  }


-- Defirnir la porter d'attaque
porter :: Coord -> Coord -> Bool
porter (C x1 y1) (C x2 y2)  = ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2) <= 4

-- Attaquer une entite 
attaque :: Unite -> Entite -> Environnement -> Environnement
attaque attaquant cible env =
  case cible of
    Left batiment -> attaqueBatiment attaquant batiment env
    Right unite -> attaqueUnite attaquant unite env

attaqueBatiment :: Unite -> Batiment -> Environnement -> Environnement
attaqueBatiment attaquant batiment env =
  let degats = case utype attaquant of
                 Combatant -> 10
                 _ -> 0
      newPointsVie = max 0 (bpointsVie batiment - degats)
      updatedBatiment = batiment { bpointsVie = newPointsVie }
  in if newPointsVie == 0
       then detruireBatiment env updatedBatiment
       else updateBatiment updatedBatiment env

attaqueUnite :: Unite -> Unite -> Environnement -> Environnement
attaqueUnite attaquant cible env =
  let degats = case utype attaquant of
                 Combatant -> 10
                 _ -> 0
      newPointsVie = max 0 (upointsVie cible - degats)
      updatedCible = cible { upointsVie = newPointsVie }
  in if newPointsVie == 0
       then removeUnite updatedCible env
       else updateUnite updatedCible env

-- Deplacer une unite
updateCoordEO::Unite->Unite
modifCoordEO u = 
    let x= (cx (ucoord u))+1  
        y=(cy (ucoord u))
        z= u{ucoord = creeCoord x y} 
    in z

updateCoordNS::Unite->Unite
modifCoordNS u= 
  let x= (cx (ucoord u))  
      y=(cy (ucoord u)) +1 
      z= u {ucoord = creeCoord x y} 
  in z

verifPosition::Coord->Coord->Bool
verifPosition c1 c2 = 
  let x = abs (cx c1 - cx c2)
      y = abs (cy c1 - cy c2)
  in x > y



deplacer::Coord->Unite->Environnement->Environnement
deplacer coord unite env = case (verifPosition coord (ucoord u)) of
                            True -> let uniteUpdate = updateCoordEO unite
                                    in updateUnite uniteUpdate env 
                            False -> let uniteUpdate = updateCoordEO unite
                                     in updateUnite uniteUpdate env 

-- Collecter de la ressource

modif_But :: Ordre->Unite->Environnement->Environment
modif_But o u env = 
  let uniteUpdate = u{ubut=o} 
  in updateUnite uniteUpdate env 

collecte::Coord->Unite->Environnement->Environnement
collecte c u e= 
  case (uid u) of
    Collecteur t -> let (r,carte)=collecteCase coord 10 (ecarte e)
                        uniteUpdate= unite{uid= Collecteur (remplirCuve t)}
                        env= e{ecarte=carte}
                    in   updateUnite uniteUpdate env
    _ -> e


collecterRessource::Coord->Unite->Environnement->Environnement
collecterRessource coord unite env = case  (M.lookup coord (ecarte e)) of
                            Nothing->env
                            Just a-> case a of
                                      Ressource n-> collecte coord unite env
                                      _ -> env

collecteurPlein::UniteId-> Maybe Bool
collecteurPlein u = case u of
                    Collecteur t -> (case t of   
                                    FullTank c -> Just True
                                    _-> Just False)
                    otherwase->Nothing

changeButCollecteur::Unite->Environnement->Environment
changeButCollecte unite env = case (collecteurPlein (uid u)) of 
                        Nothing-> u
                        Just False -> (case (M.lookup (ucoord u) (ecarte e)) of
                                        Nothing-> u
                                        Just a -> (case a of
                                                    Ressource n-> modif_But (Collecter (ucoord u)) u env
                                                    _-> env
                                                    )
                                                  )
                        Just True -> -- | probleme avec la raffinerie 

-- Patrouille 
ennemie::Unite->Entite->Bool
ennemie u e = case e of 
            Left batiment -> (uproprio u) \= (bproprio batiment)
            Right unite -> (uproprio u) \= (uproprio unite)

fillHeadPat::Unite->Bool
fillHeadPat u = case (head (uordres u)) of
                  Patrouiller a b -> True
                  _ -> False




patrouille::Unite->Environnement->Environnement
patrouille unite env =
  if fillHeadPat u then
         
   