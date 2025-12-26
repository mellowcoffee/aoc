import Data.Char (digitToInt)

maxPair :: [Int] -> Int
maxPair = snd . foldr step (-1, -1)
  where step i (maxSuffix, currentMax)
          | maxSuffix < 0 = (i, currentMax)
          | otherwise = (max i maxSuffix, max currentMax (10 * i + maxSuffix))

maxTwelve :: String -> Int
maxTwelve s = read . take 12 . reverse . fst $ foldl' step ([], length s - 12) s
  where
    step (acc, 0) c = (c:acc, 0)
    step (acc, d) c = insert acc d c

    insert [] d c = ([c], d)
    insert (x:xs) d c
      | d > 0 && c > x = insert xs (d - 1) c
      | otherwise      = (c:x:xs, d)

partOne :: [[Int]] -> Int
partOne = sum . map maxPair

partTwo :: String -> Int
partTwo = sum . map maxTwelve . lines

parse :: String -> [[Int]]
parse str = (fmap . fmap) digitToInt $ lines str
 
main :: IO ()
main = do
  input <- readFile "./input.txt"
  print $ partOne $ parse input
  print $ partTwo input
