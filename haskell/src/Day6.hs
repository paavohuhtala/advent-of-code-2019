module Day6
  ( day6
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Algorithm.Search (bfs)

type Parents = Map String String
type Edges = Map String (Set String)
type Nodes = Set String

day6a :: (Nodes, Parents) -> Int
day6a (nodes, edges) =
  sum $ fmap (countOrbiting) $ Set.elems nodes
  where
    countOrbiting :: String -> Int
    countOrbiting x = case Map.lookup x edges of
      Just parent -> 1 + countOrbiting parent
      Nothing -> 0

day6b :: Edges -> Maybe Int
day6b edges = fmap (subtract 2 . length) $ bfs (\k -> edges Map.! k) ((==) "SAN") "YOU"

loadMap :: String -> (Nodes, Parents, Edges)
loadMap input =
  let pairs = fmap (toTuple . splitOn ")") $ lines input in
  foldl
    (\(nodes, parents, edges) (a, b) -> (
      Set.insert b $ Set.insert a nodes,
      Map.insert b a parents,
      addEdge b a $ addEdge a b edges))
    (Set.empty, Map.empty, Map.empty) pairs
  where
    toTuple :: [a] -> (a, a)
    toTuple [a, b] = (a, b)
    tuTuple _      = undefined

    addEdge :: String -> String -> Edges -> Edges
    addEdge a b edges =
      Map.alter (\x -> case x of
        Nothing -> Just (Set.fromList [b])
        Just s  -> Just (Set.insert b s)) a  edges

day6 :: IO ()
day6 = do
  inputText <- readFile "./day6.txt"
  let (nodes, parents, edges) = loadMap inputText
  putStr "Day6a: "
  putStrLn $ show $ day6a (nodes, parents)
  putStr "Day6b: "
  putStrLn $ show $ day6b edges
