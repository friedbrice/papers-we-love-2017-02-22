module Beta (psi, psiR) where

import Data.List (foldl')

psi :: (Integer, Integer, Integer, Integer) -> Double
psi (r, s, r', s') =
  numer (r, s, r', s') `over` denom (r + s, r' + s')

numer :: (Integer, Integer, Integer, Integer) -> Integer
numer (r, s, r', s') = sum [ left a * right a | a <- [0..r']]
  where
    left a = (r + r' - a) `choose` r
    right a = (s + s' + 1 + a) `choose` s

denom :: (Integer, Integer) -> Integer
denom (n, n') = (n + n' + 2) `choose` (n + 1)

psiR :: (Integer, Integer, Integer, Integer) -> Double
psiR (r, s, r', s') =
  numerR (r, s, r', s') `over` denomR (r + s, r' + s')

numerR :: (Integer, Integer, Integer, Integer) -> Integer
numerR (r, s, r', s') = sum [ left a * right a | a <- [0..r']]
  where
    left a = denomR (r - 1, r' - 1 - a)
    right a = denomR (s - 1, s' + a)

denomR :: (Integer, Integer) -> Integer
denomR (_, -1) = 1
denomR (-1, _) = 1
denomR (n, n') = denomR (n, n' - 1) + denomR (n - 1, n')

factorial :: Integer -> Integer
factorial n = foldl' (*) 1 [1..n]

choose :: Integer -> Integer -> Integer
choose n k
  | k < 0 = 0
  | k > n = 0
  | otherwise = factorial n `div` (factorial k * factorial (n - k))

over :: Integer -> Integer -> Double
over n d = fromIntegral n / fromIntegral d
