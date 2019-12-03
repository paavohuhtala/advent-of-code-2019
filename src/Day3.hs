module Day3
  ( day3
  ) where

import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import           Data.Ord        (comparing)

import qualified Data.Map.Strict as Map

type Pos = (Int, Int)

data Dir
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Show)

type Movement = (Dir, Int)

data WireType
  = Wire1
  | Wire2
  deriving (Eq, Show)

data Cell
  = One (WireType, Int)
  | Both (Int, Int)
  deriving (Show)

type Grid = Map.Map Pos Cell

data Intersection =
  Intersection (Pos, (Int, Int))

readMovement :: String -> Movement
readMovement ('U':dist) = (DUp, read dist)
readMovement ('D':dist) = (DDown, read dist)
readMovement ('L':dist) = (DLeft, read dist)
readMovement ('R':dist) = (DRight, read dist)
readMovement command    = error $ "Unknown command: " ++ command

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

manhattanDistance :: Intersection -> Int
manhattanDistance (Intersection ((x, y), _)) = abs x + abs y

combinedDistance :: Intersection -> Int
combinedDistance (Intersection (_, (a, b))) = a + b

asIntersection :: (Pos, Cell) -> Maybe Intersection
asIntersection (pos, (Both t)) = Just $ Intersection (pos, t)
asIntersection _               = Nothing

applyMovement :: WireType -> (Grid, Pos, Int) -> Movement -> (Grid, Pos, Int)
applyMovement _ (grid, pos, dist) (_, 0) = (grid, pos, dist)
applyMovement wireType (grid, pos, dist) (dir, len) =
  let newDist = dist + 1
   in let newPos = applyDir pos dir
       in let newGrid = applyWire wireType newDist newPos grid
           in applyMovement wireType (newGrid, newPos, newDist) (dir, len - 1)
  where
    applyDir :: Pos -> Dir -> Pos
    applyDir (x, y) DUp    = (x, y + 1)
    applyDir (x, y) DDown  = (x, y - 1)
    applyDir (x, y) DLeft  = (x - 1, y)
    applyDir (x, y) DRight = (x + 1, y)
    applyCell :: WireType -> Int -> Maybe Cell -> Maybe Cell
    applyCell w d Nothing = Just $ One (w, d)
    applyCell w _ (Just (One (w', d)))
      | w == w' = Just $ One (w, d)
    applyCell w d1 (Just (One (w', d2)))
      | w /= w' = Just $ Both (d1, d2)
    applyCell a b c = error $ show (a, b, c)
    applyWire :: WireType -> Int -> Pos -> Grid -> Grid
    applyWire w d p = Map.alter (applyCell w d) p

applyMovements :: WireType -> [Movement] -> Grid -> Grid
applyMovements wireType movements grid =
  fst3 $ foldl (applyMovement wireType) (grid, (0, 0), 0) movements

findIntersections :: [Movement] -> [Movement] -> [Intersection]
findIntersections a b =
  let filledGrid = applyMovements Wire2 b $ applyMovements Wire1 a Map.empty
   in mapMaybe asIntersection (Map.assocs filledGrid)

day3a :: [Movement] -> [Movement] -> Int
day3a a b =
  let intersections = findIntersections a b
   in let closest = head $ sortBy (comparing manhattanDistance) intersections
       in manhattanDistance closest

day3b :: [Movement] -> [Movement] -> Int
day3b a b =
  let intersections = findIntersections a b
   in let closest = head $ sortBy (comparing combinedDistance) intersections
       in combinedDistance closest

day3 :: IO ()
day3 = do
  inputText <- readFile "./day3.txt"
  let [wireA, wireB] =
        fmap (fmap readMovement . splitOn ",") $ splitOn "\n" inputText
  putStr "Day3a: "
  putStrLn $ show $ day3a wireA wireB
  putStr "Day3b: "
  putStrLn $ show $ day3b wireA wireB
