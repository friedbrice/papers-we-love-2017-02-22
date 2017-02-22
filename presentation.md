---
title: On _On the likelihood that one unknown probability exceeds another in view of the evidence of two samples_
subtitle: Papers We Love, LA
author: \texorpdfstring{Daniel Brice\newline\url{danielbrice@gmail.com}}{Daniel Brice}
date: 22 February 2017
institute: CJ Affiliate, Santa Barbara, California
header-includes:
- \usepackage[all]{xy}
theme: default
colortheme: beaver
classoption:
- aspectratio=1610
- 12pt
fonttheme: serif
fontthemeoptions:
- onlymath
---

# The Multi-armed Bandit Problem

## The Multi-armed Bandit Problem -- Problem Statement

----------------------    ----------------------------
**You have:**             **You want:**
A bag of quarters         Maximal Money
A row of slot machines    **You don't know:**
                          Which slot machine is best
----------------------    ----------------------------

How do you balance **exploration** and **exploitation** in a way that **maximizes return** on your quarters spent?


# Applications to Machine Learning

## Applications -- Thompson Sampling Algorithm

**Consideration:**

  * *Decision theory:* We want to select the best arm from limited knowledge (i.e. solve Multi-armed Bandit Problem).

  * *Reinforcement Learning:* Select arms algorithmically, remember the outcomes, and use the knowledge gained to inform our future selections.

Thompson Sampling provides a framework for doing both simultaneously.

**Algorithm:**

  1. Sample our current model for each arm.
  2. Pull the arm that results in the highest sample.
  3. Use the results from our choice to update our model for that arm.
  4. Repeat.

## Applications -- Starting Assumptions

  * A set of arms.

    *The penny slots at Chumash Casino.*

  * An observable feedback mechanism.

    *Slots give instant feedback:*

      * *Win or lose?*
      * *If win, how much?*

  * A model for the "goodness" of your arms.

    *For each slot machine:*

      * $\mathrm{Bernoulli}(p)$ with unknown $p$
      * $\mathrm{Exponential}(\lambda)$ with unknown $\lambda$
      * $p \sim \mathrm{Beta}(1, 1)$ initial prior on $p$
      * $\lambda \sim \mathrm{Gamma}(1, 1)$ initial prior on $\lambda$

## Applications -- Architecture (Stats Model)

```haskell
data Model = Model { pulls :: Int,
                     wins  :: Int,
                     money :: Double }

sample :: Model -> Random Double
sample (Model pulls wins money) = do
  x <- sampleBeta (Beta wins (pulls - wins))
  y <- sampleGamma (Gamma wins money)
  return x * y

combine :: Model -> Model -> Model
combine (Model p1 w1 m1) (Model p2 w2 m2) =
  Model (p1 + p2) (w1 + w2) (m1 + m2)
```

## Applications -- Architecture (Interface)

```haskell
data Arm = { model :: Model, ... }

pull :: Arm -> Random Model
pull arm = ... -- (1, 0, 0) if we lose, (1, 1, m) if we win

select :: [Arm] -> Random Arm
select arms = (\(s, a) -> a) ( maxBy (\(s, a) -> s) (
              map (\arm -> (sample (model arm), arm)) arms ) )

update :: Model -> Arm -> Arm
update incomingModel arm = arm { model = newModel }
  where newModel = combine incomingModel (model arm)
```

## Applications -- Architecture (Control Flow)

```haskell
initialArms :: [Arm]
initialArms = ...

mainLoop :: [Arm] -> IO ()
mainLoop arms = do
  arm <- select arms
  incomingModel <- pull arm
  let updatedArm = update incomingModel arm
      updatedArms = updatedArm : delete arm arms
  mainLoop updatedArms

main :: IO ()
main = mainLoop initialArms
```

## Applications -- Architecture (Flow Diagram)

$$
\xymatrix{
  \mathtt{initialArms} \ar[d]^{\mathtt{select}} \ar[rr]_{\mathtt{mainLoop}} &
  &
  \mathtt{updatedArms} \ar[d]^{\mathtt{select}} \ar[r] &
  ...
  \\
  \mathtt{arm} \ar[d]^{\mathtt{pull}} \ar[r]_{\mathtt{update}} &
  \mathtt{updatedArm} \ar[ur]_{\mathtt{mainLoop}} &
  \mathtt{arm} \ar[d]^{\mathtt{pull}} \ar[r] &
  ...
  \\
  \mathtt{incomingModel} \ar[ur]_{\mathtt{update}} &
  &
  \mathtt{incomingModel} \ar[ur]
}
$$

# References

## References

  * [W. R. Thompson. "On the likelihood that one unknown probability exceeds another in view of the evidence of two samples." _Biometrika_, 1933.](https://www.dropbox.com/s/yhn9prnr5bz0156/1933-thompson.pdf)

  * [O. Chapelle, L Li. "An empirical evaluation of Thompson sampling." _Advances in Neural Information Processing Systems_, 2011.](http://dl.acm.org/citation.cfm?id=2986710)

  * [D. Fink. "A compendium of conjugate priors." Unpublished manuscript, 1997.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.157.5540&rep=rep1&type=pdf)

  * ["Bayes' Theorem," "Bayesian inference," "Conjugate prior." _Wikipedia_.](https://en.wikipedia.org/wiki/)
