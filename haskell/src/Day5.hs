module Day5
  ( day5, parseOp
  ) where

import           Control.Monad.ST
import           Data.Array.ST    (STArray, newListArray, readArray, writeArray)
import           Data.List.Split  (splitOn)
import           Data.STRef
import           Data.Char (digitToInt)
import qualified Data.Sequence as Seq
import           Debug.Trace (traceShow, traceShowId)

data ProgramResult
  = Halt Int
  | Error String
  deriving (Eq, Show)

data ParameterMode = Position | Immediate deriving (Eq, Show)
data Parameter = Parameter (Int, ParameterMode) deriving (Eq, Show)
data Op = Unary (Parameter, Int) | Trinary (Parameter, Int) (Parameter, Int) (Parameter, Int) deriving (Eq, Show)

data ProgramState s = ProgramState {
  memory :: STArray s Int Int,
  ip :: STRef s Int,
  inputBuffer :: STRef s (Seq.Seq Int),
  outputBuffer :: STRef s (Seq.Seq Int)
}

parseOp :: Int -> (Int, [ParameterMode])
parseOp x =
  let xString = show x in
  let (modes, op) = splitAt (length xString - 2) xString in
  (read op, parseModes $ reverse modes)
  where
    parseModes :: String -> [ParameterMode]
    parseModes ('0' : rest) = Position : parseModes rest
    parseModes ('1' : rest) = Immediate : parseModes rest
    parseModes [] = [Position, Position, Position]

runProgram :: ProgramState s -> ST s ProgramResult
runProgram program = do
  runCycle program
  where
    readWord :: ProgramState s -> Int -> ST s Int
    readWord state = readArray (memory state)

    readNextWord :: ProgramState s -> ST s Int
    readNextWord state = do
      ip' <- readSTRef (ip state)
      word <- readWord state ip'
      modifySTRef' (ip state) (\x -> x + 1)
      return word
    
    writeWord :: ProgramState s -> Int -> Int -> ST s ()
    writeWord state = writeArray (memory state)

    readInput :: ProgramState s -> ST s Int
    readInput state = do
      inputBuffer' <- readSTRef (inputBuffer state)
      let input = Seq.index inputBuffer' 0
      modifySTRef (inputBuffer state) (Seq.drop 1)
      return input

    writeOutput :: ProgramState s -> Int -> ST s ()
    writeOutput state x = modifySTRef (outputBuffer state) (\buffer -> buffer Seq.|> x)

    runCycle :: ProgramState s -> ST s ProgramResult
    runCycle state = do
      ip' <- readSTRef (ip state)
      op <- readNextWord state
      ip'' <- readSTRef (ip state)
      output <- readSTRef (outputBuffer state)
      runOp state $ traceShow ("runCycle", ip', op, output) (parseOp op)
    
    readParam :: ProgramState s -> ParameterMode -> Int -> ST s Int
    readParam state Position = readWord state
    readParam _ Immediate = return
    
    readBinaryOp :: ProgramState s -> (ParameterMode, ParameterMode) -> ST s (Int, Int, Int)
    readBinaryOp state (aMode, bMode) = do
      ip' <- readSTRef (ip state)
      a <- readNextWord state
      b <- readNextWord state
      dest <- readNextWord state
      a' <- readParam state aMode a
      b' <- readParam state bMode b
      ip'' <- readSTRef (ip state)
      return $ traceShow (ip', (a, aMode), (b, bMode), dest, ip'') (a', b', dest)

    runOp :: ProgramState s -> (Int, [ParameterMode]) -> ST s ProgramResult
    runOp state (1, aMode : bMode: _) = do
      (a, b, dest) <- readBinaryOp state (aMode, bMode)
      writeWord state dest (a + b)
      runCycle state
    runOp state (2, aMode : bMode : _) = do
      (a, b, dest) <- readBinaryOp state (aMode, bMode)
      writeWord state dest (a * b)
      runCycle state
    runOp state (3, _) = do
      dest <- readNextWord state
      input <- readInput state
      writeWord state dest input
      runCycle state
    runOp state (4, srcMode : _) = do
      src <- readNextWord state
      src' <- readParam state srcMode src
      x <- readWord state src'
      writeOutput state x
      runCycle state
    runOp state (99, _) = do
      result <- readWord state 0
      return $ Halt result
    runOp _ other = return $ Error $ "Undefined op: " ++ show other

createState :: [Int] -> ST s (ProgramState s)
createState initial = do
  memory <- newListArray (0, length initial) initial :: ST s (STArray s Int Int)
  ip <- newSTRef 0
  inputBuffer <- newSTRef $ Seq.singleton 1
  outputBuffer <- newSTRef Seq.empty
  return ProgramState { memory = memory, ip = ip, inputBuffer = inputBuffer, outputBuffer = outputBuffer }

day5a :: [Int] -> IO ()
day5a initial = do
  putStr "Day5a: "
  putStrLn $
    show $
    runST $ do
      program <- createState initial
      runProgram program

day5 :: IO ()
day5 = do
  inputText <- readFile "./day5.txt"
  let initialState = fmap read $ splitOn "," inputText

  putStrLn $ show (parseOp 1002)
  day5a initialState
