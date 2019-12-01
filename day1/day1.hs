{-# LANGUAGE TypeApplications #-}
module Day1 where

main :: IO ()
main = ( sum
     <$> map (func . read @Int)
     <$> lines
     <$> readFile "input")
     >>= print
  where f i = (i `div` 3) - 2
        func = sum . takeWhile (> 0) . tail . iterate f
