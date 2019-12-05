module Day4
  ( day4
  ) where

import           Data.Char (digitToInt)
import           Data.List (group, or)

noDecrease :: Int -> Bool
noDecrease x =
  let (a : rest) = fmap digitToInt $ show x in
  noDecreaseHelper a rest
  where
    noDecreaseHelper :: Int -> [Int] -> Bool
    noDecreaseHelper max [] = True
    noDecreaseHelper max (a:rest)
      | max > a = False
    noDecreaseHelper max (a:rest) = noDecreaseHelper a rest

day4a :: [Int] -> Int
day4a range =
  length $
  [ x
  | x <- range
  , hasTwoNeighbours (show x)
  , noDecrease x
  ]
  where
    hasTwoNeighbours :: Eq a => [a] -> Bool
    hasTwoNeighbours [] = False
    hasTwoNeighbours (a:b:rest)
      | a == b = True
    hasTwoNeighbours (_:rest) = hasTwoNeighbours rest

day4b :: [Int] -> Int
day4b range =
  length $
  [ x
  | x <- range
  , hasTwoNeighbours (show x)
  , noDecrease x
  ]
  where
    hasTwoNeighbours :: Eq a => [a] -> Bool
    hasTwoNeighbours ls = length (filter ((== 2) . length) $ group ls) >= 1

day4 :: IO ()
day4 = do
  let input = [134792 .. 675810]
  putStr "Day4a: "
  putStrLn $ show $ day4a input
  putStr "Day4b: "
  putStrLn $ show $ day4b input
