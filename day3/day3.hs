{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (catMaybes)

import Data.ByteString.Char8 (ByteString, unpack, split)
import qualified Data.ByteString.Char8 as B8
import Data.List (partition)


inputs :: [[ByteString]]
inputs = [ ["R8,U5,L5,D3", "U7,R6,D4,L4"]
         , ["R75,D30,R83,U83,L12,D49,R71,U7,L72",
           "U62,R66,U55,R34,D71,R55,D58,R83"]
         , ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]]

data D = R | D | U | L deriving (Show, Read)

toInp :: ByteString -> (D, Int)
toInp s = case (unpack s) of
            a:r -> (read (a:[]), read r)
            _ -> error "Invalid inp"

toInputs :: [ByteString] -> [[(D, Int)]]
toInputs = map (map toInp . split ',')


nextCoord :: (D, Int) -> (Int, Int) -> (Int, Int)
nextCoord (R,i) (cx,cy) = (cx + i, cy)
nextCoord (L,i) (cx,cy) = (cx - i, cy)
nextCoord (U,i) (cx,cy) = (cx, cy + i)
nextCoord (D,i) (cx,cy) = (cx, cy - i)

type Interval = ((Int, Int), (Int, Int))

toIntervals :: (Int, Int) -> [(D, Int)] -> [Interval]
toIntervals c (w:r) = (c,n):(toIntervals n r)
   where n = nextCoord w c
toIntervals _ [] = []

isHoriz :: Interval -> Bool
isHoriz ((ax,_), (bx,_)) = (ax == bx)

checkInter :: Interval -> Interval -> Maybe (Int, Int)
checkInter ((hx,hy1), (_, hy2)) ((vx1, vy), (vx2, _))
  = if (svx <= hx && bvx >= hx && shy <= vy && bhy >= vy)
    then Just (hx, vy)
    else Nothing
  where
    (svx, bvx) = (min vx1 vx2, max vx1 vx2)
    (shy, bhy) = (min hy1 hy2, max hy1 hy2)


intersections :: [[Interval]] -> [(Int, Int)]
intersections [w1, w2] = filter (\(x,y) -> not (x == 0 && y == 0)) combs
  where (hw1, vw1) = partition isHoriz w1
        (hw2, vw2) = partition isHoriz w2
        combs = catMaybes $ (checkInter <$> hw1 <*> vw2) ++ (checkInter <$> hw2 <*> vw1)
intersections _ = error "invalid inputs"

closest  :: [(Int, Int)] -> Int
closest = minimum . map (mdist (0,0))

contains :: (Int, Int) -> Interval -> Bool
contains (px, py) ((ix1,iy1), (ix2, iy2)) =
     (ix1 == ix2 && px == ix1 && sy <= py && py <= by)
  || (iy1 == iy2 && py == iy1 && sx <= px && px <= bx)
 where (sx, sy) = (min ix1 ix2, min iy1 iy2)
       (bx, by) = (max ix1 ix2, max iy1 iy2)

mdist :: (Int, Int) -> (Int, Int) -> Int
mdist (x1,y1) (x2,y2)
  = (abs (x2 - x1)) + (abs (y2 - y1))

toSteps :: [[Interval]] -> [(Int, Int)] -> [Int]
toSteps _ [] = []
toSteps ints@[iw1, iw2] (p:r) = (sw1+sw2):(toSteps ints r )
  where
    sw1 = (sum (map (uncurry mdist) to) + (mdist st p))
      where (to, ((st,_):_)) = span (not . contains p) iw1
    sw2 = (sum (map (uncurry mdist) to) + (mdist st p))
      where (to, ((st,_):_)) = span (not . contains p) iw2
toSteps _ _ = error "Not enough wires"

main :: IO ()
main = do let ints = map (map (toIntervals (0,0))) $ map toInputs $ inputs
              iints = map intersections ints
          print iints
          print $ map closest iints
          print $ map (minimum . uncurry toSteps) $ zip ints iints
          inputInts <- map (toIntervals (0,0)) <$> toInputs <$> B8.lines <$> B8.readFile "input"
          let inputIntersections = intersections inputInts
          print $ closest inputIntersections
          print $ minimum $ toSteps inputInts inputIntersections
