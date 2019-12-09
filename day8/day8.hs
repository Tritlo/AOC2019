{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Data.List
import Data.Function

getInput :: IO [Int]
getInput = map (read @Int . (:[]) ) . filter (/= '\n') <$> readFile "input"


testInputs :: [[Int]]
testInputs = [[1,2,3,4,5,6,7,8,9,0,1,2]]

toLayers :: (Int, Int) -> [Int] -> [[[Int]]]
toLayers (w,h) inp = map (tl w) $ tl (w*h) inp
  where
    tl _ [] = []
    tl x inp = l:(tl x r)
          where (l,r) = splitAt x inp
    


checkL layer = (numZ, num1*num2)
  where sl = group $ sort layer
        isG n (g:_) = (g == n)
        numZ = sum $ map length $ filter (isG 0) sl
        num1 = sum $ map length $ filter (isG 1) sl
        num2 = sum $ map length $ filter (isG 2) sl


checkS :: [(Int,Int)] -> Int
checkS = snd . minimumBy (compare `on` fst)

isColor :: Int -> Int -> Int
isColor 2 n = n
isColor n _ = n

colors :: [Int] -> [Int] -> [Int]
colors = zipWith isColor


main :: IO ()
main = do let l = toLayers (3,2) (testInputs !! 0)
              r = map (checkL . join) l
          input <- getInput
          let layers = toLayers (25,6) input
              ti = [0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0]
          print $ checkS $ map (checkL . join) $ layers
          print $ toLayers (2,2) $
            foldl colors (repeat 2) $ map join $ toLayers (2,2) ti
          let o = toLayers (25,6) $ foldl colors (repeat 2) $ map join layers
              p :: Int -> Char
              p 0 = ' '
              p 1 = '0'
              p _ = ' '
          mapM_ (mapM_ (putStrLn . map p)) o
