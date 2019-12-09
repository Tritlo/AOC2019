{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Array.ST
import Data.STRef
import Control.Monad.ST

import Data.List
import Control.Monad (join)
import Data.Array

import Debug.Trace

import qualified Data.ByteString.Char8 as B8
getInput :: IO [Int]
getInput = map ((read @Int) . B8.unpack). B8.split ',' <$> B8.readFile "input"


data Mode = Pos | Imm | Rel deriving (Show)
data MonoOp = Input | Output | AdjustRel deriving (Show)
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
        op  9 = Mono AdjustRel
        op 99 = Exit
        op v = error ("Undefined op: " ++ show v ++ " from " ++ show i)
        m1 = toMode $ (i `div` 100) `mod` 10
        m2 = toMode $ (i `div` 1000) `mod` 10
        m3 = toMode $ (i `div` 10000) `mod` 10
        toMode 0 = Pos
        toMode 1 = Imm
        toMode 2 = Rel
        toMode _ = error "Invalid Mode"

data State = State { done :: Bool,
                     pos :: Int,
                     rel :: Int,
                     prog :: Array Int Int,
                     inps :: [Int]} deriving (Show)


initState :: [Int] -> [Int] -> State
initState prog inps = State False 0 0 (listArray bds prog) inps
 where
   bds = (0, fromIntegral $ (length prog) - 1)

run :: State -> (State, [Int])
run s@(State done cpos crel cmem cinps) = if done then (s, [])
  else runST $
  do { inp <- newSTRef cinps
     ; pos <- newSTRef cpos
     ; rel <- newSTRef crel
     ; out <- newSTRef []
     ; mmem <- (thaw cmem) :: ST s (STUArray s Int Int)
     ; let loop mem =
             do { let
                    readMem Imm p = do {  (_, ma) <- getBounds mem
                                       ; if p > ma
                                         then return 0
                                         else readArray mem p }

                    readMem Pos p = do { loc <- readMem Imm p
                                       ; readMem Imm loc }
                    readMem Rel p = do { loc <- readMem Imm p
                                       ; r <- readSTRef rel
                                       ; readMem Imm (loc + r)}
                    writeMem Rel p v = do { r <- readSTRef rel
                                          ; writeMem Imm (p+r) v }
                    writeMem _ p v =
                      do {  (_, ma) <- getBounds mem
                         ; if p <= ma
                           then do { writeArray mem p v
                                   ; loop mem }
                           else do { els <- getElems mem
                                   ; mem <- newListArray (0,p) (els ++ (repeat 0))
                                   ; writeArray mem p v
                                   ; loop mem }}
                ; p <- readSTRef pos
                ; (op, m1, m2, m3) <- readOp <$> readMem Imm p
                ; case op of
                    Exit -> return (True, mem)
                    Mono mop -> do { case mop of
                                     Input -> do { p1 <- readMem Imm (p + 1)
                                                 ; ival <- readSTRef inp
                                                 ; case ival of
                                                     (ci:r) -> do { writeSTRef inp r
                                                                  ; writeSTRef pos (p + 2)
                                                                  ; writeMem m1 p1 ci }
                                                     [] -> return (False, mem) }
                                     Output -> do { p1 <- readMem m1 (p + 1)
                                                  ; modifySTRef out (p1:)
                                                  ; writeSTRef pos (p + 2)
                                                  ; loop mem}
                                     AdjustRel -> do { p1 <- readMem m1 (p + 1)
                                                     ; modifySTRef rel (p1+)
                                                     ; writeSTRef pos (p + 2)
                                                     ; loop mem}
                                  }
                    Bin bop -> do { p1 <- readMem m1 (p + 1)
                                  ; p2 <- readMem m2 (p + 2)
                                  ; case bop of
                                      JNZ -> writeSTRef pos (if (p1 /= 0)
                                                             then p2
                                                             else (p + 3))
                                      JZ  -> writeSTRef pos (if p1 == 0
                                                             then p2
                                                             else (p + 3))
                                  ; loop mem }

                    Tri top -> do { p1 <- readMem m1 (p + 1)
                                  ; p2 <- readMem m2 (p + 2)
                                  ; p3 <- readMem Imm (p + 3)
                                  ; writeSTRef pos (p + 4)
                                  ; case top of
                                      Sum -> writeMem m3 p3 (p1 + p2)
                                      Mul -> writeMem m3 p3 (p1 * p2)
                                      LessT -> writeMem m3 p3 (if p1 < p2
                                                               then 1 else 0)
                                      Equal -> writeMem m3 p3 (if p1 == p2
                                                               then 1 else 0) }}
     ; (finished, nmem) <- loop mmem
     ; fmem <- freeze nmem
     ; np <- readSTRef pos
     ; no <- reverse <$> readSTRef out
     ; ni <- readSTRef inp
     ; nr <- readSTRef rel
     ; return $ (State finished np nr fmem ni, no) }

addInps :: [Int] -> State -> State
addInps newInps st@(State {inps = inps}) = st {inps = (inps ++ newInps)}


tests = [[109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
        ,[1102,34915192,34915192,7,4,7,99,0]
        ,[104,1125899906842624,99]]


main :: IO ()
main = do let res = map (snd . run . flip initState []) tests
          print res
          input <- getInput
          let (_, o1) = run $ initState input [1]
          print o1
          let (fs, o2) = run $ initState input [2]
          print o2
