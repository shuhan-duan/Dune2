{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Carte where

import qualified Data.Map.Strict as M

data Coord = C {cx :: Int, cy :: Int}
    deriving (Show, Eq)

instance Ord Coord where
    compare (C x1 y1) (C x2 y2)
        | y1 < y2 = LT
        | y1 > y2 = GT
        | x1 < x2 = LT
        | x1 > x2 = GT
        | otherwise = EQ


creeCoord :: Int -> Int -> Coord
creeCoord x y = C {cx = x, cy = y}

prop_Coord_unique :: Coord -> Coord -> Bool
prop_Coord_unique c1 c2 = c1 /= c2 

prop_Coord_positive :: Coord -> Bool
prop_Coord_positive c = cx c >= 0 && cy c >= 0


data Terrain = Herbe | Ressource Int | Eau
  deriving (Show, Eq)

-- Invariant pour le type Terrain
prop_Terrain :: Terrain -> Bool
prop_Terrain (Ressource n) = n > 0
prop_Terrain _ = True




newtype Carte = Carte {carte :: M.Map Coord Terrain} deriving (Show, Eq)

-- Invariant pour le type Carte
prop_Carte :: Carte -> Bool 
prop_Carte (Carte m) = all coordValide (M.keys m)
                        && all terrainCorrect (M.elems m)
                        && all segConvexe (segments $ M.keys m)
  where
    coordValide (C x y) = x >= 0 && y >= 0
    terrainCorrect t = case t of
                         Ressource n -> n > 0
                         _ -> True
    segments cs = [(a, b) | a <- cs, b <- cs, a < b]
    segConvexe (c1, c2)
        | cx c1 == cx c2 = all (\y -> M.member (C (cx c1) y) m) [min (cy c1) (cy c2) .. max (cy c1) (cy c2)]
        | cy c1 == cy c2 = all (\x -> M.member (C x (cy c1)) m) [min (cx c1) (cx c2) .. max (cx c1) (cx c2)]
        | otherwise = error "segments non alignés"

-- Fonction de collecte de ressources
collecteCase :: Coord -> Int -> Carte -> (Int, Carte)
collecteCase coord r (Carte m) =
  case M.lookup coord m of
    Just (Ressource n) ->
      let r' = min r n
          m' = if r' == n then M.insert coord Herbe m else M.insert coord (Ressource (n - r')) m
      in (r', Carte m')
    Just _ -> (0, Carte m)  -- Case occupée par un terrain autre qu'un gisement de ressources
    Nothing -> (0, Carte m) -- Case inexistante sur la carte


--Postconditon de collecteCase
prop_postCollecteCase :: Coord -> Int -> Carte -> Bool
prop_postCollecteCase coord r c@(Carte m) =
  let (v, Carte m') = collecteCase coord r c
  in case M.lookup coord m of
       Just (Ressource n) -> v == min r n
                              && (if v == n then not (M.member coord m')
                                  else case M.lookup coord m' of
                                         Just (Ressource n') -> n' == n - v
                                         _ -> False)
       _ -> v == 0 && m' == m

--Precondition de collecteCase
prop_preCollecteCase :: Coord -> Int -> Carte -> Bool
prop_preCollecteCase (C x y) r (Carte m) =
  case M.lookup (C x y) m of
    Just (Ressource n) -> n >= r
    _ -> False


getTerrain :: Carte -> Coord -> Maybe Terrain
getTerrain (Carte c) coord = M.lookup coord c

-- verifier si la case est constructible
isConstructible :: Coord -> Carte -> Bool
isConstructible c carte = case getTerrain carte c of
                           Just Herbe -> True
                           Just (Ressource 0) -> True
                           _ -> False

isValidCoord :: Carte -> Coord -> Bool
isValidCoord (Carte m) coord = M.member coord m

isEau :: Maybe Terrain -> Bool
isEau (Just Eau) = True
isEau _ = False


