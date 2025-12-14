module Main where

import Data.List (sort, nub, subsequences)

splitOn :: Eq t => t -> [t] -> [[t]]
splitOn _ [] = []
splitOn d xs = chunk : splitOn d (drop 1 rest)
  where (chunk, rest) = span (/=d) xs

parse :: String -> [(Int, Int)]
parse str =  parsePair <$> splitOn '-' <$> splitOn ',' str
  where parsePair [a, b] = (read a, read b)

-- Analytic solution:
-- For S, a decimal string represenation of some nat n, to be composed of a
-- recurring nat x of length k, n must satisfy n = x * 10^k + x = x * (10^k +
-- 1) Let M_k = 10^k + 1. For x to be a valid generator of such an n, x must
-- satisfy x in [10^(k-1), 10^k - 1] For n in [a, b], n's generator x must also
-- satisfy a <= x * M_k <= b, and thus ceil(a/M_k) <= x <= floor(b/M_k) Thus we
-- have constructed two constraints on such x-s. Let k in [1, 2, ..., 9] Let
-- R_k = [max(10^(k-1), ceil(a/M_k)), min(10^k - 1, floor(b/M_k))]. If R_k =
-- [x_min, x_max] is a valid nat interval, then let S_k = Σ_(x = x_min)^(x_max)
-- x * M_k Clearly, S_k = M_k * Σ_(x = x_min)^(x_max) x. From Σ_(k=0)^(n) k = n
-- * (n+1) / 2, it can trivially be seen that Σ_(k=a)^(b) k = (a + b)(b - a +
-- 1)/2. Thus, S_k = M_k * (x_min + x_max)(x_max - x_min + 1)/2.
rangeSum :: (Int, Int) -> Int
rangeSum (a, b) = sum $ map solveK [1..9]
  where solveK k =
          let m      = 10^k + 1
              lowerX = 10^(k-1)
              upperX = 10^k - 1
              minX   = max lowerX (ceiling (fromIntegral a / fromIntegral m))
              maxX   = min upperX (floor (fromIntegral b / fromIntegral m))
          in if minX > maxX
            then 0
            else m * (minX + maxX) * (maxX - minX + 1) `div` 2

union :: [(Int, Int)] -> [(Int, Int)]
union = foldr merge [] . sort
  where merge (a,b) ((c,d):xs) | c <= b+1 = (a, max b d) : xs
        merge i xs = i : xs

intersect :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
intersect (l, h) = filter (\(a,b) -> a <= b) . map (\(a,b) -> (max a l, min b h))

count :: [(Int, Int)] -> Int
count = sum . map (\(a,b) -> b - a + 1)

valSum :: Int -> Int -> [(Int, Int)] -> Int
valSum len p ivs = sum $ map sumSeq validGens
  where
    m = (10^len - 1) `div` (10^p - 1)
    target = (10^(p-1), 10^p - 1)
    scaled = map (\(a,b) -> ((a+m-1)`div`m, b`div`m)) ivs
    validGens = intersect target scaled
    sumSeq (a,b) = m * (a + b) * (b - a + 1) `div` 2

partTwo :: [(Int, Int)] -> Int
partTwo inputs = sum $ map solveL [2..18]
  where
    ivs = union inputs
    primes n = nub [ x | x <- [2..n], n `mod` x == 0, null [y | y<-[2..x-1], x`mod`y==0]]
    solveL l = sum [ sgn (length sub) * valSum l (foldr1 gcd sub) ivs
                   | sub <- subsequences [ l `div` p | p <- primes l ], not (null sub) ]
    sgn k = if odd k then 1 else -1

partOne :: [(Int, Int)] -> Int
partOne = sum . map rangeSum

main :: IO ()
main = do
  input <- parse <$> readFile "./input.txt"
  print $ partOne input
  print $ partTwo input
