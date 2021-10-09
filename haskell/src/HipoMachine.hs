module HipoMachine where

import Codegen (Operador (..))
import qualified Codegen as Gen
import Data.Bool (Bool)
import Data.List.Split (splitOn)
import Utils (replaceNth)

data MachineData = MachineData
  { instructions :: [(Operador, Double)],
    memory :: [Double],
    stack :: [Double],
    iPointer :: Int
  }
  deriving (Show)

run :: String -> IO ()
run code = do
  let md = emptyMd {instructions = stringToInstructions code}
  executeMachineData md
  return ()

emptyMd :: MachineData
emptyMd = MachineData [] [] [] 0

stringToInstructions :: String -> [(Operador, Double)]
stringToInstructions code =
  map f commands
  where
    commands = lines code
    f a =
      let splitL = splitOn " " a
          op = read (head splitL) :: Gen.Operador
          arg = read (last splitL) :: Double
       in (op, arg)

nextMachineData :: MachineData -> MachineData
nextMachineData md = md {iPointer = 1 + iPointer md}

executeMachineData :: MachineData -> IO MachineData
executeMachineData md =
  case currentIns of
    (INPP, _) ->
      if p == 0
        then executeMachineData $ nextMachineData md
        else error "Invalid call INPP "
    (ALME, _) -> executeMachineData $ nextMachineData md {memory = 0 : memory md}
    (CRVL, i) ->
      executeMachineData $ nextMachineData md {stack = m !! round i : s}
    (ARMZ, i) -> executeMachineData $ nextMachineData md {stack = tail s, memory = replaceNth (round i) (head s) m}
    (SOMA, i) -> executeMachineData $ nextMachineData md {stack = sum (take 2 s) : drop 2 s}
    (SUBT, i) -> executeMachineData $ nextMachineData md {stack = foldr1 (-) (take 2 s) : drop 2 s}
    (DIVI, i) -> executeMachineData $ nextMachineData md {stack = foldr1 (/) (take 2 s) : drop 2 s}
    (MULT, i) -> executeMachineData $ nextMachineData md {stack = product (take 2 s) : drop 2 s}
    (INVE, i) -> executeMachineData $ nextMachineData md {stack = -1 * head s : drop 1 s}
    (CRCT, i) -> executeMachineData $ nextMachineData md {stack = i : s}
    -- <
    (CPME, i) ->
      executeMachineData $
        nextMachineData md {stack = compareFunFirstTwoElements (<) s : drop 2 s}
    -- >
    (CPMA, i) ->
      executeMachineData $
        nextMachineData md {stack = compareFunFirstTwoElements (>) s : drop 2 s}
    -- =
    (CPIG, i) ->
      executeMachineData $
        nextMachineData md {stack = compareFunFirstTwoElements (==) s : drop 2 s}
    -- <>
    (CDES, i) ->
      executeMachineData $
        nextMachineData md {stack = compareFunFirstTwoElements (/=) s : drop 2 s}
    -- <=
    (CPMI, i) ->
      executeMachineData $
        nextMachineData md {stack = compareFunFirstTwoElements (<=) s : drop 2 s}
    -- >=
    (CMAI, i) ->
      executeMachineData $
        nextMachineData md {stack = compareFunFirstTwoElements (>=) s : drop 2 s}
    (DSVF, i) ->
      let nextPointer =
            if 1 == head s
              then round i
              else p + 1
       in do
            executeMachineData
            $ nextMachineData md {iPointer = nextPointer, stack = drop 1 s}
    (DSVI, i) -> executeMachineData md {iPointer = round i - 1}
    (LEIT, _) -> do
      input <- getLine
      executeMachineData $ nextMachineData md {stack = read input : s}
    (IMPR, i) -> do
      print $ m !! round i
      executeMachineData $ nextMachineData md {stack = tail s}
    (PARA, _) -> return md
    (c, _) -> error $ "Invalid Command: " ++ show c
  where
    MachineData {iPointer = p, stack = s, instructions = ins, memory = m} = md
    currentIns = ins !! p

compareFunFirstTwoElements :: (Double -> Double -> Bool) -> [Double] -> Double
compareFunFirstTwoElements f list =
  compareFun f (head list) $ list !! 1

compareFun :: (Double -> Double -> Bool) -> Double -> Double -> Double
compareFun f v1 v2 = fromIntegral (fromEnum $ f v1 v2) :: Double