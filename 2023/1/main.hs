import Data.List (find, isPrefixOf)
import Data.Maybe (isJust)

handleLinePartOne :: String -> Int
handleLinePartOne l = read [x, y]
  where
    digits = filter (`elem` "123456789") l
    x = head digits
    y = last digits

digitMap :: [(String, Char)]
digitMap = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ['1' .. '9']

handleLinePartTwo :: String -> Int
handleLinePartTwo str = read [head nums, last nums]
  where
    nums = go str []
    go [] xs = reverse xs
    go (x : xs) ys
      | x `elem` ['1' .. '9'] = go xs (x : ys)
      | otherwise = case find (\(word, _) -> word `isPrefixOf` (x : xs)) digitMap of
          Just (word, digit) -> go xs (digit : ys)
          Nothing -> go xs ys

partOne :: String -> Int
partOne s = sum $ map handleLinePartOne $ lines s

partTwo :: String -> Int
partTwo s = sum $ map handleLinePartTwo $ lines s

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ partOne input
  print $ partTwo input
