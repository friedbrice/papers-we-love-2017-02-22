module Models (biasedCoin, placebo, treatment) where

import System.Random (getStdRandom, randomR)

biasedCoin :: Double -> IO Bool
biasedCoin p = sampleBernoulli (Bernoulli p)

placebo :: IO Bool
placebo = biasedCoin 0.3

treatment :: IO Bool
treatment = biasedCoin 0.4

data Bernoulli = Bernoulli Double

sampleBernoulli :: Bernoulli -> IO Bool
sampleBernoulli (Bernoulli p) = do
  draw <- getStdRandom (randomR (0, 1))
  return $ draw <= p
