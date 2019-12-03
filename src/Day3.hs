module Day3 (day3) where

import Data.List (scanl, sortBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)
import Data.Array.ST
import Data.Maybe (mapMaybe)
import Control.Monad.ST
import Debug.Trace

import qualified Data.Map.Strict as Map

type Pos = (Int, Int)
data Dir = DUp | DDown | DLeft | DRight deriving (Show)
type Movement = (Dir, Int)
data WireType = Wire1 | Wire2 deriving (Eq, Show)
data Cell = One (WireType, Int) | Both (Int, Int) deriving (Show)
type Grid = Map.Map Pos Cell
data Intersection = Intersection (Pos, (Int, Int))

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

manhattanDistance :: Intersection -> Int
manhattanDistance (Intersection ((x, y), _)) = abs x + abs y

combinedDistance :: Intersection -> Int
combinedDistance (Intersection (_, (a, b))) = a + b

asIntersection :: (Pos, Cell) -> Maybe Intersection
asIntersection (pos, (Both t)) = Just $ Intersection (pos, t) 
asIntersection _               = Nothing

applyMovements :: WireType -> [Movement] -> Grid -> Grid
applyMovements wireType movements grid = fst3 $ foldl (applyMovement wireType) (grid, (0, 0), 0) movements
  where 
    applyDir :: Pos -> Dir -> Pos
    applyDir (x, y) DUp    = (x, y + 1)
    applyDir (x, y) DDown  = (x, y - 1)
    applyDir (x, y) DLeft  = (x - 1, y)
    applyDir (x, y) DRight = (x + 1, y)

    applyCell :: WireType -> Int -> Maybe Cell -> Maybe Cell
    applyCell w dist Nothing                             = Just $ One (w, dist)
    applyCell w _ (Just (One (w', dist)))      | w == w' = Just $ One (w, dist)
    applyCell w dist1 (Just (One (w', dist2))) | w /= w' = Just $ Both (dist1, dist2)
    applyCell a b c                                      = error $ show (a, b, c)

    applyWire :: WireType -> Int -> Pos -> Grid -> Grid
    applyWire wireType dist pos = Map.alter (applyCell wireType dist) pos 

    applyMovement :: WireType -> (Grid, Pos, Int) -> Movement -> (Grid, Pos, Int)
    applyMovement _        (grid, pos, dist) (dir, 0)   = (grid, pos, dist)
    applyMovement wireType (grid, pos, dist) (dir, len) =
        let newDist = dist + 1 in
        let newPos = applyDir pos dir in
        let newGrid = applyWire wireType newDist newPos grid in
        applyMovement wireType (newGrid, newPos, newDist) (dir, len - 1)

findIntersections :: [Movement] -> [Movement] -> [Intersection]
findIntersections a b =
    let filledGrid = applyMovements Wire2 b $ applyMovements Wire1 a Map.empty in
    mapMaybe asIntersection (Map.assocs filledGrid)

day3a :: [Movement] -> [Movement] -> Int
day3a a b =
    let intersections = findIntersections a b in
    let closest = head $ sortBy (comparing manhattanDistance) intersections in
    manhattanDistance closest

day3b :: [Movement] -> [Movement] -> Int
day3b a b = 
    let intersections = findIntersections a b in
    let closest = head $ sortBy (comparing combinedDistance) intersections in
    combinedDistance closest

day3 :: IO ()
day3 = do
    inputText <- readFile "./day3.txt"
    let [wireA, wireB] = fmap (fmap readMovement . splitOn ",") $ splitOn "\n" inputText
    putStrLn $ show $ day3a wireA wireB
    putStrLn $ show $ day3b wireA wireB
    return ()
    where
        readMovement :: String -> Movement
        readMovement ('U' : tail) = (DUp, read tail)
        readMovement ('D' : tail) = (DDown, read tail)
        readMovement ('L' : tail) = (DLeft, read tail)
        readMovement ('R' : tail) = (DRight, read tail)
