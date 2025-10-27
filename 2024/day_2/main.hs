import Data.List

-- Input parsing
parse :: String -> [[Int]]
parse = map (map read . words) . lines

-- Part One
safeScore :: [[Int]] -> Int
safeScore = length . filter isSafe

-- Part Two
dampenedSafeScore :: [[Int]] -> Int
dampenedSafeScore = length . filter (any isSafe) . map oneOffSubsets
  where
    oneOffSubsets xs = filter (\ys -> length ys == length xs-1) (subsequences xs)

-- Helper functions
isSafe :: [Int] -> Bool
isSafe xs = all pos diff || all neg diff
  where
    diff = zipWith (-) (tail xs) xs
    neg x = (-3) <= x && x <= (-1)
    pos x = 1 <= x && x <= 3

-- IO
main = do
  input <- readFile "input.txt"
  print $ safeScore $ parse input
  print $ dampenedSafeScore $ parse input
