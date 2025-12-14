{-# LANGUAGE ViewPatterns #-}
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.List (unfoldr)
import qualified Data.Map as Map

type Pos = (Int, Int) -- Row, Col
type Grid = Map Pos Char

parse :: String -> Grid
parse str = Map.fromList $ do
    (r, line) <- zip [0..] (lines str)
    (c, char) <- zip [0..] line
    return ((r, c), char)

neighborhood :: Pos -> Grid -> [Pos]
neighborhood (x, y) m = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard $ (dx, dy) /= (0, 0)
  let n = (x+dx, y+dy)
  guard $ Map.member n m
  return n

isAccessibleRoll :: Grid -> Pos -> Bool
isAccessibleRoll m p = 
    case Map.lookup p m of
        Just '@' -> neighborCount < 4
        _        -> False
  where 
    neighborCount = length $ filter (== '@') $ mapMaybe (`Map.lookup` m) (neighborhood p m)

accessibleRolls :: Grid -> [Pos]
accessibleRolls m = filter (isAccessibleRoll m) (Map.keys m)

partOne :: Grid -> Int
partOne = length . accessibleRolls 

partTwo :: Grid -> Int
partTwo = sum . unfoldr step
  where step (accessibleRolls -> []) = Nothing
        step m@(accessibleRolls -> xs) = Just (length xs, foldr Map.delete m xs)
  -- where step m | partOne m == 0 = Nothing
  --              | otherwise = Just (length $ accessibleRolls m, foldr Map.delete m (accessibleRolls m))

main :: IO ()
main = do
  input <- parse <$> readFile "./input.txt"
  print $ partOne input
  print $ partTwo input
