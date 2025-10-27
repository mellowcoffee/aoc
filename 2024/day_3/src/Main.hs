{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Attoparsec.Text as P

import Data.Text (Text)
import Data.Bool (bool)
import Data.Either (fromRight)
import Control.Applicative (asum, (<|>))
import Control.Monad.State

parseMul :: Integral a => P.Parser a
parseMul = do
  P.string "mul("
  n1 <- P.decimal
  P.char ','
  n2 <- P.decimal
  P.char ')'
  return $ n1 * n2

partOne :: Text -> Int
partOne input =
  let
    loop = parseMul <|> (P.anyChar >> loop)
    traverse = do
      ns <- P.many1 loop
      return $ sum ns
  in
    fromRight (error "no parse") (P.parseOnly traverse input)

partTwo :: Text -> Int
partTwo input = let
  enable = do
    lift $ P.string "do()"
    put True
  disable = do
    lift $ P.string "don't()"
    put False
  mul = lift parseMul
  loop = asum [
    enable >> loop,
    disable >> loop,
    bool (const 0) id <$> get <*> mul,
    lift P.anyChar >> loop ]
  traverse = sum <$> P.many1 loop
  in fromRight (error "no parse") (P.parseOnly (evalStateT traverse True) input)

main :: IO ()
main = undefined
