import Beta (psi)
import Models (biasedCoin, placebo, treatment)

data Results = Results {
  placeboSuccess :: Integer,
  placeboFailure :: Integer,
  treatmentSuccess :: Integer,
  treatmentFailure :: Integer
} deriving Show

empty = Results 0 0 0 0
append (Results a1 b1 c1 d1) (Results a2 b2 c2 d2) =
  Results (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

givePlacebo :: IO Results
givePlacebo = do
  cured <- placebo
  if cured then return $ Results 1 0 0 0
  else return $ Results 0 1 0 0

giveTreatment :: IO Results
giveTreatment = do
  cured <- treatment
  if cured then return $ Results 0 0 1 0
  else return $ Results 0 0 0 1

-- | The likelihood that Treatment has a higher frequency of
--   success than Placebo based on the available results.
likelihood :: Results -> Double
likelihood (Results r s r' s') = psi (r, s, r', s')

-- | Use the available evidence to decide whether to give the
--   patient a placebo or the treatment. Returns the outcome.
sample :: Results -> IO Results
sample results = do
  treat <- biasedCoin $ likelihood results
  if treat then giveTreatment
  else givePlacebo

mainLoop :: Results -> IO ()
mainLoop results = do
  putStr "Results so far: "
  print results
  putStr "Likelihood that Treatment is better than Placebo: "
  print (likelihood results)
  getLine
  result <- sample results
  putStrLn (report result)
  mainLoop (results `append` result)

main :: IO ()
main = mainLoop empty

report :: Results -> String
report (Results 0 0 0 1) = "Picked Treatment: Failure!"
report (Results 0 0 1 0) = "Picked Treatment: Success!"
report (Results 0 1 0 0) = "Picked Placebo: Failure!"
report (Results 1 0 0 0) = "Picked Placebo: Success!"
