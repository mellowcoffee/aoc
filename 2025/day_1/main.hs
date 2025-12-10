parse :: String -> [Int]
parse str = step <$> words str
  where step ('R':n) = read n
        step (_:n) = negate $ read n

move :: Integral t => t -> t -> t
move a b = (a + b) `mod` 100

partOne :: [Int] -> Int
partOne = length . filter (==0) . scanl addMod 50
  where addMod a b = (a + b) `mod` 100

partTwo :: [Int] -> Int
partTwo xs = sum $ zipWith touches (scanl move 50 xs) xs
  where touches a b
         | b > 0     = (a + b) `div` 100
         | otherwise = (a - 1) `div` 100 - (a + b - 1) `div` 100

main = do
  input <- parse <$> readFile "./input.txt"
  print $ partOne input
  print $ partTwo input
