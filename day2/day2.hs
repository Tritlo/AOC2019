{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Array.ST
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.Unboxed
import Data.STRef

import Data.List (find)

import qualified Data.ByteString.Char8 as B8


getInput :: IO [Int]
getInput = map (read @Int . B8.unpack). B8.split ',' <$> B8.readFile "input"

inputs :: [[Int]]
inputs = [ [1,0,0,0,99]
         , [2,3,0,3,99]
         , [1,1,1,4,99,5,6,0,99]]


modifyInput :: Int -> Int -> [Int] -> [Int]
modifyInput noun verb (first:_:_:rest) = first:noun:verb:rest

sol :: [Int] -> Int
sol f = (! 0) $ runSTUArray $
  do { stua <- newListArray (0, (length f) - 1) f
     ; let go p = do { val <- readArray stua p
                     ;  case val of
                          99 -> return stua
                          _ -> do { let op =
                                          case val of
                                            1 -> (+)
                                            2 -> (*)
                                            _ -> error ("invalid op " ++ show val)
                                  ; i1 <- readArray stua (p + 1) >>= readArray stua
                                  ; i2 <- readArray stua (p + 2) >>= readArray stua
                                  ; o <- readArray stua (p + 3)
                                  ; writeArray stua o (op i1 i2)
                                  ; go (p + 4) }}
        ; go 0 }

-- Values are between 0 and 99, inclusive.

target :: Int
target = 19690720


main :: IO ()
main = do input <- getInput
          let inputs = map (uncurry modifyInput) $ (,) <$> [0..99] <*> [0..99]
              sols = map (\modify -> (p2 (modify input), sol (modify input))) inputs
              p2 (_:noun:verb:_) = 100*noun + verb
          print (sol $ modifyInput 12 2 $ input)
          print $ find (\(_,r) -> r == target) sols
