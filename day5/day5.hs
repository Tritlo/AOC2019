{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Array.ST
import Data.STRef
import Control.Monad.ST

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


run :: [Int] -> [Int] -> [Int]
run prog inputs = runST $
  do { sta <- (newListArray (0, (length prog) - 1) prog) :: ST s (STArray s Int Int)
     ; inp <- newSTRef inputs
     ; pos <- newSTRef 0
     ; out <- newSTRef []
     ; let readInput Imm p = readArray sta p
           readInput Pos p = readArray sta p >>= readArray sta
     ; let loop =
             do { p <- readSTRef pos
                ; val <- readArray sta p
                ; let (op, m1, m2, _) = readOp val
                ; case op of
                    Exit -> return ()
                    Mono mop -> do { case mop of
                                     Input -> do { i <- readInput Imm (p + 1)
                                                 ; (ci:r) <- readSTRef inp
                                                 ; writeArray sta i ci
                                                 ; writeSTRef inp r }
                                     Output -> do { o <- readInput m1 (p + 1)
                                                  ; modifySTRef out (o:) }
                                  ; writeSTRef pos (p + 2)
                                  ; loop }
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
     ; loop
     ; reverse <$> readSTRef out }

main :: IO ()
main = do input <- getInput
          mapM_  (print . flip run [8]) testInputs
          print $ run input [1]
          print $ run input [5]
          return ()
