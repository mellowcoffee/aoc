import Data.List (transpose)

op "+" = (+)
op "*" = (*)

splitOn :: Eq t => t -> [t] -> [[t]]
splitOn _ [] = []
splitOn d xs = chunk : splitOn d (drop 1 rest)
  where (chunk, rest) = span (/=d) xs

partOne :: String -> Int
partOne str = sum $ (parseCol . reverse) <$> transpose (words <$> lines str)
  where parseCol (o:ns) = foldr1 (op o) (read <$> ns)

partTwo :: String -> Int
partTwo str = sum $ zipWith fold (map op ops) nss
  where ops = words $ last (lines str)
        cols = splitOn "    " (init <$> transpose (lines str))
        nss = (map . map) (read . concat . words) cols
        fold o c = foldr1 o c

main :: IO ()
main = do
  input <- readFile "./input.txt"
  print $ partOne input
  print $ partTwo input
