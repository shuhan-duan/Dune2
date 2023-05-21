module Path
where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Common
import Carte
import Debug.Trace (traceShow)

type Visited = M.Map Coord Bool

bfs :: Environnement -> Coord -> Coord -> [Coord]
bfs env start goal =
  bfs' env [(start, [start])] M.empty goal

bfs' :: Environnement -> [(Coord, [Coord])] -> Visited -> Coord -> [Coord]
bfs' _ [] _ _ = []
bfs' env ((coord, path):rest) visited goal
  | coord == goal = path
  | fromMaybe False (M.lookup coord visited) = bfs' env rest visited goal
  | otherwise =
      let newVisited = M.insert coord True visited
          neighbors = adjacentCoords coord goal env
          newQueue = rest ++ [(neighbor, path ++ [neighbor]) | neighbor <- neighbors]
      in bfs' env newQueue newVisited goal

canPass :: Environnement -> Coord -> Coord -> Bool
canPass env  goal coord=
  let terrain = M.lookup coord (carte (ecarte env))
      hasUnit = M.foldr (\u acc -> acc || ucoord u == coord) False (unites env)
      hasBuilding = M.foldr (\b acc -> acc || bcoord b == coord) False (batiments env)
      result = not (isEau terrain) && not hasUnit && (not hasBuilding || coord == goal)
  in  result

adjacentCoords :: Coord -> Coord -> Environnement -> [Coord]
adjacentCoords (C x y) goal env =
  let maxX = 20
      maxY = 20
  in filter (canPass env goal) [C x' y' | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], x' >= 0, x' <= maxX, y' >= 0, y' <= maxY]
