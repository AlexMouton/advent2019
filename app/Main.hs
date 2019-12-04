module Main where

import Prelude
import Data.Functor (void)
import Data.Function ((&), fix)
import Control.Monad ((>=>))
import Control.Arrow ((>>>))

main :: IO ()
main = do
  _ <- day1
  return ()

day1 :: IO ()
day1 = do
  f <- readFile "day1.txt"
  let is = read <$> lines f
  print $ day1v1 is
  print $ day1v2 is

day1v1 :: [Int] -> Int
day1v1 = sum . fmap massToFuel

day1v2 :: [Int] -> Int
day1v2 = sum . fmap (fix massToFuelRec)

massToFuel :: Int -> Int
massToFuel x = (div x 3) - 2

massToFuelRec :: (Int -> Int) -> Int -> Int
massToFuelRec rec = massToFuel >>> \a -> if a <= 0 then 0 else a + rec a
