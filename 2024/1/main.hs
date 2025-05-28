import Data.List

-- Input parsing
parse :: String -> [[Int]]
parse = transpose . map (map read . words) . lines

-- Part One
distance :: [[Int]] -> Int
distance xs = sum $ map abs $ zipWith (-) (sort $ head xs) (sort $ last xs) 

-- Part Two
similarity :: [[Int]] -> Int
similarity xs = sum $ (\xs ys -> map (\x -> count x ys * x) xs) (head xs) (last xs)
  where count x = length . filter (==x)

-- IO
main = do
  input <- readFile "input.txt"
  let lists = parse input
  print $ distance lists
  print $ similarity lists
