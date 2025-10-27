module Main where

import qualified Data.Map as M

type V2 = (Int, Int)
type LetterMap = M.Map V2 Char

index :: [[a]] -> [(V2, a)]
index xss = [((x,y), e) | (x, row) <- zip [1..] xss, (y, e) <- zip [1..] row]

neighbors :: V2 -> [V2]
neighbors (x,y) = [(i,j) | i <- [x-1,x,x+1], j <- [y-1,y,y+1], i > 0, j > 0]

add :: V2 -> V2 -> V2
add (x,y) (a,b) = (x+a, y+b)

scale :: Int -> V2 -> V2
scale n (dx,dy) = (n*dx, n*dy)

dimensions :: LetterMap -> (Int, Int)
dimensions m
  | M.null m  = (0,0)
  | otherwise = (maximum (map fst ks), maximum (map snd ks))
  where ks = M.keys m

letterMap :: String -> LetterMap
letterMap = M.fromList . index . lines

dirs :: [V2]
dirs = [(dx,dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]

matchFrom :: LetterMap -> V2 -> V2 -> Bool
matchFrom m o d =
  all (\(pos,ch) -> M.lookup pos m == Just ch) $ zip positions "XMAS"
  where positions = map (\i -> add o (scale i d)) [0..3]

partOne :: LetterMap -> Int
partOne m = length
  [ () | o <- M.keys m
       , M.lookup o m == Just 'X'
       , d <- dirs
       , matchFrom m o d
  ]

-- [TODO] Part Two

main :: IO ()
main = do
  file <- readFile "input.txt"
  let input = letterMap file
  print $ partOne input
