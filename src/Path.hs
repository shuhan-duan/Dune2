module Path
where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sortBy)
import Common
import Carte


type Cost = Int
type OpenSet = M.Map Coord Cost

aStar :: Environnement -> Coord -> Coord -> [Coord]
aStar env start goal =
  let openSet = M.singleton start 0
      cameFrom = M.empty
      costSoFar = M.singleton start 0
  in aStar' env openSet cameFrom costSoFar goal

aStar' :: Environnement -> OpenSet -> M.Map Coord Coord -> M.Map Coord Cost -> Coord -> [Coord]
aStar' env openSet cameFrom costSoFar goal =
  case lowestCost openSet of
    Nothing -> []
    Just current ->
      if current == goal
      then reconstructPath cameFrom goal
      else let (openSet', cameFrom', costSoFar') = updateNeighbors current
           in aStar' env openSet' cameFrom' costSoFar' goal
  where
    lowestCost os = fst <$> listToMaybe (sortBy (\(_, a) (_, b) -> compare a b) (M.toList os))

    updateNeighbors current =
      let neighbors = filter (canPass env) (adjacentCoords current)
          currentCost = fromMaybe maxBound (M.lookup current costSoFar) + 1
          (newOpenSet, newCameFrom, newCostSoFar) = foldr (update currentCost current) (openSet, cameFrom, costSoFar) neighbors
      in (newOpenSet, newCameFrom, newCostSoFar)

    update currentCost current neighbor (os, cf, csf) =
      let oldCost = fromMaybe maxBound (M.lookup neighbor csf)
      in if currentCost < oldCost
        then (M.insert neighbor (currentCost + heuristic goal neighbor) os,
              M.insert neighbor current cf,
              M.insert neighbor currentCost csf)
        else (os, cf, csf)

reconstructPath :: M.Map Coord Coord -> Coord -> [Coord]
reconstructPath cameFrom goal =
  case M.lookup goal cameFrom of
    Nothing -> []
    Just prev -> goal : reconstructPath cameFrom prev

canPass :: Environnement -> Coord -> Bool
canPass env coord =
  let terrain = M.lookup coord (carte (ecarte env))
      hasUnit = M.foldr (\u acc -> acc || ucoord u == coord) False (unites env)
      hasBuilding = M.foldr (\b acc -> acc || bcoord b == coord) False (batiments env)
  in not (isEau terrain) && not hasUnit && not hasBuilding

adjacentCoords :: Coord -> [Coord]
adjacentCoords (C x y) = [C (x-1) y, C (x+1) y, C x (y-1), C x (y+1)]

heuristic :: Coord -> Coord -> Int
heuristic (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)
