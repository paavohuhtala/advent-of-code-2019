module Day2
  ( day2
  ) where

import           Control.Monad.ST
import           Data.Array.ST    (STArray, newListArray, readArray, writeArray)
import           Data.List.Split  (splitOn)

data ProgramState
  = Halt Int
  | Error String
  deriving (Eq, Show)

runProgram :: STArray s Int Int -> ST s ProgramState
runProgram program = do
  runCycle 0 program
  where
    runCycle :: Int -> STArray s Int Int -> ST s ProgramState
    runCycle ip state = do
      op <- readArray state ip
      runOp op ip state
    readBinaryOp :: Int -> STArray s Int Int -> ST s (Int, Int, Int)
    readBinaryOp ip state = do
      aPos <- readArray state (ip + 1)
      bPos <- readArray state (ip + 2)
      a <- readArray state aPos
      b <- readArray state bPos
      dest <- readArray state (ip + 3)
      return (a, b, dest)
    runOp :: Int -> Int -> STArray s Int Int -> ST s ProgramState
    runOp 1 ip state = do
      (a, b, dest) <- readBinaryOp ip state
      writeArray state dest (a + b)
      runCycle (ip + 4) state
    runOp 2 ip state = do
      (a, b, dest) <- readBinaryOp ip state
      writeArray state dest (a * b)
      runCycle (ip + 4) state
    runOp 99 _ state = fmap Halt (readArray state 0)
    runOp other _ _ = return $ Error $ "Undefined op: " ++ show other

createState :: [Int] -> (Int, Int) -> ST s (STArray s Int Int)
createState initial (noun, verb) = do
  program <-
    newListArray (0, length initial) initial :: ST s (STArray s Int Int)
  writeArray program 1 noun
  writeArray program 2 verb
  return program

day2a :: [Int] -> IO ()
day2a initial = do
  putStr "Day2a: "
  putStrLn $
    show $
    runST $ do
      program <- createState initial (12, 2)
      runProgram program

day2b :: [Int] -> IO ()
day2b initial = do
  let range = [0 .. 99]
  let target = 19690720
  let [(noun, verb)] =
        [ (n, v)
        | n <- range
        , v <- range
        , (Halt target) ==
            (runST $ (createState initial (n, v)) >>= runProgram)
        ]
  putStr "Day2b: "
  putStrLn $ show $ 100 * noun + verb

day2 :: IO ()
day2 = do
  inputText <- readFile "./day2.txt"
  let initialState = fmap read $ splitOn "," inputText
  day2a initialState
  day2b initialState
