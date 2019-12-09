{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Data.List
import Control.Monad (join)

import Debug.Trace

import qualified Data.ByteString.Char8 as B8
getInput :: IO [Int]
getInput = map (read @Int . B8.unpack). B8.split ',' <$> B8.readFile "input"

testInputs :: [[Int]]
testInputs = [ [1,0,0,0,99]
             , [2,3,0,3,99]
             , [2,4,4,5,99,0]
             , [1,1,1,4,99,5,6,0,99]
             , [1101,100,-1,4,0]
             , [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]

data Mode = Pos | Imm deriving (Show)
data MonoOp = Input | Output deriving (Show)
data BinOp  = JNZ | JZ deriving (Show)
data TriOp  = Mul | Sum | LessT | Equal  deriving (Show)
data Op = Exit | Mono MonoOp | Bin BinOp | Tri TriOp deriving (Show)


readOp :: Int -> (Op, Mode, Mode, Mode)
readOp i = (op (i `mod` 100), m1, m2, m3)
  where op  1 = Tri Sum
        op  2 = Tri Mul
        op  3 = Mono Input
        op  4 = Mono Output
        op  5 = Bin JNZ
        op  6 = Bin JZ
        op  7 = Tri LessT
        op  8 = Tri Equal
        op 99 = Exit
        op v = error ("Undefined op: " ++ show v ++ " from " ++ show i)
        m1 = toMode $ (i `div` 100) `mod` 10
        m2 = toMode $ (i `div` 1000) `mod` 10
        m3 = toMode $ (i `div` 10000) `mod` 10
        toMode 0 = Pos
        toMode 1 = Imm
        toMode _ = error "Invalid Mode"

data State = State { done :: Bool,
                     pos :: Int,
                     prog :: [Int],
                     inps :: [Int]} deriving (Show)


initState :: [Int] -> [Int] -> State
initState prog inps = State False 0 prog inps

run :: State -> (State, [Int])
run s@(State done ipos iprog iinps) =
  if done then (s, [])
  else runST $
  do { sta <- (newListArray (0, (length iprog) - 1) iprog) :: ST s (STArray s Int Int)
     ; inp <- newSTRef iinps
     ; pos <- newSTRef ipos
     ; out <- newSTRef []
     ; let readInput Imm p = readArray sta p
           readInput Pos p = readArray sta p >>= readArray sta
     ; let loop =
             do { p <- readSTRef pos
                ; val <- readArray sta p
                ; let (op, m1, m2, _) = readOp val
                ; case op of
                    Exit -> return True
                    Mono mop -> do { case mop of
                                     Input -> do { i <- readInput Imm (p + 1)
                                                 ; ival <- readSTRef inp
                                                 ; case ival of
                                                     (ci:r) -> do { writeArray sta i ci
                                                                  ; writeSTRef inp r
                                                                  ; writeSTRef pos (p + 2)
                                                                  ; loop}
                                                     [] -> return False }
                                     Output -> do { o <- readInput m1 (p + 1)
                                                  ; modifySTRef out (o:)
                                                  ; writeSTRef pos (p + 2)
                                                  ; loop}
                                  }
                    Bin bop -> do { i1 <- readInput m1 (p + 1)
                                  ; i2 <- readInput m2 (p + 2)
                                  ; case bop of
                                      JNZ -> writeSTRef pos (if (i1 /= 0)
                                                             then i2
                                                             else (p + 3))
                                      JZ  -> writeSTRef pos (if i1 == 0
                                                             then i2
                                                             else (p + 3))
                                  ; loop }

                    Tri top -> do { i1 <- readInput m1 (p + 1)
                                  ; i2 <- readInput m2 (p + 2)
                                  ; o <- readArray sta (p + 3)
                                  ; case top of
                                      Sum -> writeArray sta o (i1 + i2)
                                      Mul -> writeArray sta o (i1 * i2)
                                      LessT -> writeArray sta o (if i1 < i2
                                                                 then 1 else 0)
                                      Equal -> writeArray sta o (if i1 == i2
                                                                 then 1 else 0)
                                  ; writeSTRef pos (p + 4)
                                  ; loop }}
     ; finished <- loop
     ; p <- readSTRef pos
     ; els <- getElems sta
     ; o <-  reverse <$> readSTRef out
     ; i <- readSTRef inp
     ; return $ (State finished p els i, o) }


addInps :: [Int] -> State -> State
addInps newInps st@(State {inps = inps}) = st {inps = (inps ++ newInps)}


chain :: [Int] -> [State] -> ([State], [Int])
chain inps sts = chain' inps [] sts
 where chain' inps states [] = (reverse states, inps)
       chain' inps done (s:sts) = chain' out (ns:done) sts
         where  (ns, out) = run (addInps inps s)


loopUntilDone :: [Int] -> [State] -> Int
loopUntilDone inps sts = chain' inps [] sts
 where chain' inps (s:_)  [] | done s = last inps
       chain' inps states [] = chain' inps [] $ reverse states
       chain' inps dones (s:sts) = chain' out (ns:dones) sts
         where  (ns, out) = run (addInps inps s)



genSettings :: [Int] ->[[Int]]
genSettings [] = [[]]
genSettings avail = join $ map f2 cs
 where combs i = (i, delete i avail)
       cs = map combs avail
       f2 (i,r) =  map (i:) rs
         where rs = genSettings r
      

t1,t2,t3,t4,t5 :: [Int]
t1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
t2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0]
t3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,
      31,1,32,31,31,4,31,99,0,0,0]

--
t4 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
      27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
t5 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,
      1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,
      1008,54,0,55,1001,55,1,55,2,53,55,53,4,
      53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

initStates :: [Int] -> [Int] -> [State]
initStates prog settings = map (initState prog . (:[])) settings



runProg1 :: [Int] -> [Int] -> Int
runProg1 prog = last . snd . chain [0] . initStates prog

runProg2 :: [Int] -> [Int] -> Int
runProg2 prog = loopUntilDone [0] . initStates prog


main :: IO ()
main = do let sets =  genSettings [0..4]
              m1 = maximum $ map (runProg1 t1)  sets
              m2 = maximum $ map (runProg1 t2)  sets
              m3 = maximum $ map (runProg1 t3)  sets

              s2 = genSettings [5..9]
              m4 = maximum $ map (runProg2 t4) s2
              m5 = maximum $ map (runProg2 t5) s2
          print (m1,m2,m3)
          input <- getInput
          print $ maximum $ map (runProg1 input) sets
          print (m4,m5)
          print $ maximum $ map (runProg2 input) s2
          -- print $ run5 t4 [5,6,7,8,9]
          -- print $ maximum $ map (run5 input) s2


  -- input <- getInput
  --         mapM_  (print . flip run [8]) testInputs
  --         print $ run input [1]
  --         print $ run input [5]
  --         return ()
