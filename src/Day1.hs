module Day1 (day1) where

readNumbersFromLines :: String -> [Integer]
readNumbersFromLines = fmap read . lines

day1a :: [Integer] -> Integer
day1a = sum . fmap (subtract 2 . (flip div) 3)

day1b :: [Integer] -> Integer
day1b = sum . fmap calculateFuel
    where
    fuel n = max 0 $ (div n 3) - 2
    calculateFuel n
        | n <= 0    = 0
        | otherwise = (fuel n) + calculateFuel (fuel n)

day1 :: IO ()
day1 = do
    inputText <- readFile "./day1.txt"
    let inputNumbers = readNumbersFromLines inputText
    putStr "Day1a: "
    putStrLn $ show $ day1a inputNumbers
    putStr "Day1b: "
    putStrLn $ show $ day1b inputNumbers
