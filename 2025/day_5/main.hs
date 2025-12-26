import Data.List (sort)

data Input = Input {
  ranges :: [(Int, Int)],
  ingredients :: [Int]
} deriving (Show)

splitOn :: Eq t => t -> [t] -> [[t]]
splitOn _ [] = []
splitOn d xs = chunk : splitOn d (drop 1 rest)
  where (chunk, rest) = span (/=d) xs

parse :: String -> Input
parse str = Input rs is
  where (rl, il) = break null $ lines str
        rs = [(read x, read y) | l <- rl, let (x, _:y) = break (=='-') l]
        is = map read $ drop 1 il 

union :: [(Int, Int)] -> [(Int, Int)]
union = foldr merge [] . sort
  where merge (a,b) ((c,d):xs) | c <= b+1 = (a, max b d) : xs
        merge i xs = i : xs

partOne :: Input -> Int
partOne (Input rs is) = length $ filter (\i -> any (inRange i) rs) is
  where inRange n (a, b) = a <= n && n <= b

partTwo :: Input -> Int
partTwo (Input rs _) = sum $ map (\(a,b) -> b - a + 1) (union rs)

main :: IO ()
main = do
  input <- parse <$> readFile "./input.txt"
  print $ partOne input
  print $ partTwo input
