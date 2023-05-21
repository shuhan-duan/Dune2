module Carte where

import qualified Data.Map.Strict as M
import Data.List (nubBy,minimumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.Random (StdGen, randomRs, randomR, split)
import Control.Monad (replicateM)

data Coord = C {cx :: Int, cy :: Int}
    deriving (Show, Eq)

instance Ord Coord where
    compare (C x1 y1) (C x2 y2)
        | y1 < y2 = LT
        | y1 > y2 = GT
        | x1 < x2 = LT
        | x1 > x2 = GT
        | otherwise = EQ

-- Creates a Coord data type with x and y coordinates
creeCoord :: Int -> Int -> Coord
creeCoord x y = C {cx = x, cy = y}

-- Property to check if two coordinates are unique
prop_Coord_unique :: Coord -> Coord -> Bool
prop_Coord_unique c1 c2 = c1 /= c2 

-- Property to check if a coordinate has positive x and y values
prop_Coord_positive :: Coord -> Bool
prop_Coord_positive c = cx c >= 0 && cy c >= 0



data Terrain = Herbe | Ressource Int | Eau
  deriving (Show, Eq)

-- Property to check if a Terrain value is valid
prop_Terrain :: Terrain -> Bool
prop_Terrain (Ressource n) = n > 0
prop_Terrain _ = True

newtype Carte = Carte {carte :: M.Map Coord Terrain} deriving (Show, Eq)

--- Property to check if a Carte data type is valid
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

-- Collects resources from a given Coord key in the Carte
collecteCase :: Coord -> Int -> Carte -> (Int, Carte)
collecteCase coord r (Carte m) =
  case M.lookup coord m of
    Just (Ressource n) ->
      let r' = min r n
          m' = if r' == n then M.insert coord Herbe m else M.insert coord (Ressource (n - r')) m
      in (r', Carte m')
    Just _ -> (0, Carte m)  -- Case occupée par un terrain autre qu'un gisement de ressources
    Nothing -> (0, Carte m) -- Case inexistante sur la carte


-- Postcondition for the collecteCase function
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

--Precondition for the collecteCase function
prop_preCollecteCase :: Coord -> Int -> Carte -> Bool
prop_preCollecteCase (C x y) r (Carte m) =
  case M.lookup (C x y) m of
    Just (Ressource n) -> n >= r
    _ -> False

-- Retrieves the terrain type at the specified coordinate from a given map
getTerrain :: Carte -> Coord -> Maybe Terrain
getTerrain (Carte c) coord = M.lookup coord c

-- Checks if a given coordinate on a map is constructible 
isConstructible :: Coord -> Carte -> Bool
isConstructible c carte = case getTerrain carte c of
                           Just Herbe -> True
                           Just (Ressource 0) -> True
                           _ -> False

-- Checks if a given coordinate is a valid coordinate on a map
isValidCoord :: Carte -> Coord -> Bool
isValidCoord (Carte m) coord = M.member coord m

-- Determines if a given terrain type is water
isEau :: Maybe Terrain -> Bool
isEau (Just Eau) = True
isEau _ = False

-- Sets the terrain type at the specified coordinate to grass (i.e., an empty space)
setCaseVide :: Coord -> Carte -> Carte
setCaseVide coord (Carte c) = Carte $ M.insert coord Herbe c

dimension :: Carte -> (Int, Int)
dimension (Carte m) = let coords = Map.keys m
                          xs = map (\(C x _) -> x) coords
                          ys = map (\(C _ y) -> y) coords
                      in (maximum xs + 1, maximum ys + 1)

generateRandomMap :: Int -> Int -> StdGen -> Carte
generateRandomMap width height gen =
  let coords = [C x y | x <- [0..width - 1], y <- [0..height - 1]]
      initialMap = M.fromList [(coord, Herbe) | coord <- coords]

      numberOfModifiedCoords = (width * height * 4) `div` 10

      terrainFromInt n = case n of
        0 -> Herbe
        1 -> Ressource 50
        _ -> Eau

      genTerrain coord = let (terrainType, newGen) = randomR (0 :: Int, 2 :: Int) gen
                         in (coord, terrainFromInt terrainType, newGen)

      updateMapWithRandomTerrain m (coord, terrain, newGen) = (M.insert coord terrain m, newGen)

      generateMapWithRandomTerrains m _ 0 _ = m
      generateMapWithRandomTerrains m g remainingCoords usedCoords =
        let (x, g1) = randomR (0, width - 1) g
            (y, g2) = randomR (0, height - 1) g1
            coord = C x y
            (terrain, g3) = randomR (0 :: Int, 2 :: Int) g2
        in if coord `elem` usedCoords
             then generateMapWithRandomTerrains m g3 remainingCoords usedCoords
             else generateMapWithRandomTerrains (M.insert coord (terrainFromInt terrain) m) g3 (remainingCoords - 1) (coord:usedCoords)

      updatedMap = generateMapWithRandomTerrains initialMap gen numberOfModifiedCoords []

      -- Ensure grass at the bottom-left and top-right corners
      finalMap = M.insert (C 0 0) Herbe . M.insert (C (width - 1) (height - 1)) Herbe $ updatedMap
  in Carte finalMap


generateInitialPlayerPositions :: Carte -> [Coord]
generateInitialPlayerPositions carte = [C 0 0, C (cols - 1) (rows - 1)]
  where (cols, rows) = dimension carte

euclideanDistance :: Coord -> Coord -> Float
euclideanDistance (C x1 y1) (C x2 y2) =
  let dx = fromIntegral (x1 - x2)
      dy = fromIntegral (y1 - y2)
  in sqrt (dx * dx + dy * dy)

findNearestGrass :: Coord -> Carte -> Maybe Coord
findNearestGrass coord (Carte m) =
  let grassCoords = [c | (c, t) <- M.toList m, t == Herbe, c /= coord]
  in if null grassCoords
       then Nothing
       else Just $ minimumBy (comparing (euclideanDistance coord)) grassCoords