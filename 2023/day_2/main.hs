import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe)

-- Types
type Draw = (Int, Int, Int) -- RGB
type Game = (Int, [Draw]) -- id and draws

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d xs = x:splitOn d (drop 1 rest)
  where (x, rest) = span (/= d) xs

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseDraw :: String -> Draw
parseDraw str = (r, g, b)
  where
    parts = map trim (splitOn ',' str)
    pairs = map (\xs -> (tail $ dropWhile isDigit xs, read $ takeWhile isDigit xs)) parts
    r = fromMaybe 0 $ lookup "red" pairs
    g = fromMaybe 0 $ lookup "green" pairs
    b = fromMaybe 0 $ lookup "blue" pairs

parseDraws :: String -> [Draw]
parseDraws str = map (parseDraw . trim) (splitOn ';' str)

parseGame :: String -> Game
parseGame xs = (read $ drop 5 idStr, parseDraws drawsStr)
  where [idStr, drawsStr] = splitOn ':' xs

validateDraws :: [Draw] -> Bool
validateDraws = all (\(r,g,b) -> r<=12 && g<=13 && b<=14)

minCubes :: [Draw] -> Draw
minCubes draws = (maximum rs, maximum gs, maximum bs)
  where (rs,gs,bs) = unzip3 draws

partOne :: [Game] -> Int
partOne = sum . map fst . filter (validateDraws . snd)

partTwo :: [Game] -> Int
partTwo = sum . map ((\(r,g,b) -> r*g*b) . minCubes . snd) 

main :: IO()
main = do
  input <- readFile "./input.txt"
  let games = parseGame <$> lines input
  print $ partOne games
  print $ partTwo games
