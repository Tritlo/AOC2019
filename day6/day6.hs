-- |
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Char8 (ByteString)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Maybe (mapMaybe)

getInput :: IO [(ByteString, ByteString)]
getInput = map parseInput <$> B8.lines <$> B8.readFile "input"

testInput :: [ByteString]
testInput = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H",
             "D)I", "E)J", "J)K", "K)L"]

testInput2 :: [ByteString]
testInput2 = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H",
              "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]


parseInput :: ByteString -> (ByteString, ByteString)
parseInput a = (x,y)
  where [x,y] = B8.split ')' a

toOrbits :: [(ByteString, ByteString)] -> Map ByteString [ByteString]
toOrbits = Map.fromListWith (++) . map (\(x,y) -> (x, [y]))

countIndirs :: Map ByteString [ByteString] -> Int -> [ByteString] -> Int
countIndirs orbs n sats = (n*(length nks)) + sum other
  where nks = map (flip (Map.findWithDefault []) orbs) sats
        other = map (countIndirs orbs (n+1)) nks

getPathTo :: Map ByteString [ByteString] -> ByteString -> ByteString -> Maybe [ByteString]
getPathTo _ target cur | cur == target = return []
getPathTo orbs target cur =
  do sats <- orbs Map.!? cur
     (cur:) <$> firstMaybe (getPathTo orbs target) sats

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f [] = Nothing
firstMaybe f (a:as) = case f a of
                        Nothing -> firstMaybe f as
                        Just b -> Just b

transferDist :: Map ByteString [ByteString] -> Int
transferDist orbs = ncp pYou pSan
  where Just pYou = getPathTo orbs "SAN" "COM"
        Just pSan = getPathTo orbs "YOU" "COM"
        ncp [] b = length b
        ncp a [] = length a
        ncp (a:as) (b:bs) | a == b = ncp as bs
        ncp as bs = length as + length bs



main :: IO ()
main = do inpOrbs <- toOrbits <$> getInput
          print $ countIndirs inpOrbs 0 ["COM"]
          print $ transferDist inpOrbs
