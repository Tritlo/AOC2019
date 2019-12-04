module Main where
import Control.Monad (join)
import Data.List (group)

input :: (Int, Int)
input = (278384,824795)

-- Naive bruteforce solution

inRange :: [Int] -> Bool
inRange r = n >= mi && n <= ma
  where (mi, ma) = input
        n = sum (map (\d -> (r !! (5-d) * (10^d))) [0..5])

-- Part 1
hasDouble :: [Int] -> Bool
hasDouble = any (>= 2) . map length . group

-- Part 1

addDigits :: [[[Int]]] -> [[[Int]]]
addDigits r = map (\d -> join $ map (fmap (d:)) (map (r !!) [d..9])) [0..9]

digits :: [[[Int]]]
digits = (!! 5) $ iterate addDigits base
 where base = map (return . return) [0..9]

res1 :: Int
res1 = sum (map (length . filter (\v -> inRange v && hasDouble v)) digits)

-- Part 2
hasExactlyDouble :: [Int] -> Bool
hasExactlyDouble = any (== 2) . map length . group

res2 :: Int
res2 = sum (map (length . filter (\v -> inRange v && hasExactlyDouble v)) digits)

main :: IO ()
main = print res1 >> print res2
