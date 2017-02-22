---
title: On "On the likelihood that one unknown probability exceeds another in view of the evidence of two samples" by W. R. Thompson
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

# The Problem

## Medical Experiment Design

  - You have two potential treatments: $A$ and $B$.
  - You are not certain which treatment is _better_.
  - How do you decide which patients get which treatment?

![Pills and placebos](./img/pills.jpg)

## Balancing Competing Goals

**Belief:** Treatment $B$ is better than Treatment $A$

**Confidence:** 75%

**Competing Goals:**

  - _Exploration:_ Discover which treatment really is better.
  - _Exploitation:_ Cure the most patients.

## Some Solutions (and their shortcomings)

  1. All patients get Treatment $B$.
    - Never gain new insight.
    - 25% chance we're wrong.
  2. Half get Treatment $A$, half get Treatment $B$.
    - Ethical?
    - Stopping conditions?

# "On the likelihood that one unknown probability exceeds another in view of the evidence of two samples" by William R. Thompson, 1933.

## Purpose

This paper is ...

> ... not centred upon the interpretation of particular data, but ... a general interest in problems of research planning ...

> ... there can be no objection to the use of data, however meagre, as a guide to action required before more can be collected.

Use all the data! (Even if there's not a lot.)

> Serious objection can otherwise be raised to argument based upon a small number of observations.
> Indeed, the fact that such objection can never be eliminated entirely---no matter how great the number of observations---suggested the possible value of seeking other modes of operation than that of taking a large number of observations before analysis or any attempt to direct our course.

## Structure -- Section 1

  - Section 1: Things researchers should be doing.

      - Select option proportionally to our beliefs in their benefits.

      Option    Expected Benefit    Selection Proportion
      --------  ------------------  -------------------------
      $X$       200 utils           200/400 = 50%
      $Y$       150 utils           150/400 = 37.5%
      $Z$       50 utils            50/400 = 12.5%
      --------  ------------------  -------------------------

      - Continually update our beliefs rigorously using Bayes' Theorem.

      $$
        P(\text{belief} | \text{observation}) = \frac{
          P(\text{observation} | \text{belief})
        }{
          P(\text{observation})
        } P(\text{belief})
      $$

## Structure -- Section 2

  - Section 2: An application.
      - Two populations: Group 1, Group 2.
      - Unknown frequencies of an observable trait: $\tilde{p}_1$, $\tilde{p}_2$.
      - A small number of observations:
          - $r$ samples from Group 1 with trait.
          - $s$ samples from Group 1 without trait.
          - $r'$ samples from Group 2 with trait.
          - $s'$ samples from Group 2 without trait.
      - Calculate $P(\tilde{p}_2 > \tilde{p}_1)$.

## Structure -- Section 3

  - Section 3:
      - Connections to related work.
      - Call to statisticians to continue this work.
          - Table complete for small values of $r, s, r', s'$.
          - Complete table for larger values.

## Commentary -- Benefits of Thompson's Solution

**Problem:**

  - Must treat $n$ patients
  - $0.5 < p < 1$, our confidence that Treatment $B$ is better than Treatment $A$

**Solutions:**

  1. All patients get Treatment $B$
      - Permanent $n (1 - p)$ regret
  2. Half get $A$, half get $B$
      - $0.5 n (1 - p) + 0.5 n p = 0.5 n$ regret
  3. Thompson Sampling (choose $B$ with probability $p$, updating $p$)
      - Temporary $p n (1 - p) + (1 - p) n p = 2 n p (1 - p)$ regret

## Commentary -- The Maths Bits

  - Two infinite populations: Group 1, Group 2.
  - Each population has a frequency of a trait: $\tilde{p}_1, \tilde{p}_2$.
  - We have observations: $r, s, r', s'$.
      - $r$ samples from Group 1 with trait.
      - $s$ samples from Group 1 without trait.
      - $r'$ samples from Group 2 with trait.
      - $s'$ samples from Group 2 without trait.
  - We need: $P(\tilde{p}_2 > \tilde{p}_1)$.

**Theorem:**

$$
P(\tilde{p}_2 > \tilde{p}_1) = \frac{
  \sum_{\alpha = 0}^{r'} {r + r' - \alpha \choose r} {s + s' + 1 + \alpha \choose s}
}{
  {n + n' + 2 \choose n + 1}
}
$$

where $n = r + s, n' = r' + s'$.

## Commentary -- The Math Bits

**Notation:**

  - $\psi(r, s, r', s') = \frac{N(r, s, r', s')}{D(r, s, r', s')}$
  - $N(r, s, r', s') = \sum_{\alpha = 0}^{r'}{r + r' - \alpha \choose r}{s + s' + 1 + \alpha \choose s}$
  - $D(r, s, r', s') = D(n, n') = {n + n' + 2 \choose n + 1}$ where $n = r + s, n' = r' + s'$.

**Theorem:**

  1. $D(n, n') = D(n, n' - 1) + D(n - 1, n')$
  2. $N(r, s, r', s') = \sum_{\alpha = 0}^{r'} D(r - 1, r' - 1 - \alpha) D(s - 1, s' + \alpha)$

**Theorem:**

  - Table of values of $N$ and $D$ for various small values of $r, s, r', s'$.
  - Identities useful for evaluating $N$ and $D$ for other arguments.

# Applications to Machine Learning

## Applications -- Modern Conveniences

  - Computers
      - Calculate $N$ and $D$ very quickly, no table required.
      - Stats libraries provide methods for generating random samples.
  - Conjugate Priors
      - Useful for estimating unknown parameters.
      - Bayes' theorem already worked out for many common probability distributions.

## Applications -- Conjugate Priors

**Definition:**

  - $f_\_$, $h_\_$ parametrized families of densities
  - $X \sim f_\theta(x)$
  - $\theta \sim h_\alpha(t)$
  - $x_0 \leftarrow X$

By Bayes' Theorem, $\theta' \sim g(t) = c \, L_{X, \theta}(x_0, t) \, h_\alpha(t)$.

If there exists a parameter $\alpha'$ such that $g = h_{\alpha'}$, then the family $h_\_$ is called a *conjugate prior* of the family $f_\_$

## Applications -- Conjugate Priors

**Conjugate Priors:**

  - Already worked out for many classical distributions.
  - Parameters are simple to calculate/track.
  - Stats libraries provide methods for generating random samples.

Likelihood                        Conjugate Prior                            Parameters
-------------------------------   -----------------------------------------  -------------------------------------------------
$\mathrm{Bernoulli}(p)$           $p \sim \mathrm{Beta}(a, b)$               $a = \text{successes}, b = \text{failures}$
$\mathrm{Exponential}(\lambda)$   $\lambda \sim \mathrm{Gamma}(a, b)$        $a = \text{observations}, b = \text{sum total}$
$\mathrm{Gamma}(\_, b)$           $b \sim \mathrm{Gamma}(a', b')$            $a' = \text{observations}, b' = \text{sum total}$
$\mathrm{Normal}(\mu, \_)$        $\mu \sim \mathrm{Normal}(\mu', \sigma')$  $\mu' = \dots, \sigma' = \dots$
$\mathrm{Normal}(\_, \sigma)$     $\sigma \sim \mathrm{Gamma}(a, 1/b)$       $a = \dots, b = \dots$
$\dots$                           $\dots$                                    $\dots$
-------------------------------   -----------------------------------------  -------------------------------------------------

## Applications -- Thompson Sampling Algorithm

**Considerations:**

  - *Decision theory:* We want to select the best option despite limited knowledge.

  - *Reinforcement Learning:* Choose option algorithmically, observe outcomes, and use the knowledge gained to inform future choices.

Thompson Sampling provides a framework for doing both simultaneously.

**Algorithm:**

  1. Sample our current model for each option.
  2. Choose the option that gets the highest sample.
  3. Observe the results of our choice.
  4. Use the observation to update our beliefs about that choice.
  5. Repeat.

## Applications -- Starting Assumptions

  - A set of _arms_.

    _The penny slots at Chumash Casino._

  - An observable feedback mechanism.

    _Slots give instant feedback:_

      - _Win or lose?_
      - _If win, how much?_

  - A model for the "goodness" of each arms.

    _For each slot machine:_

      - $\mathrm{Bernoulli}(p)$ with unknown $p$
      - $\mathrm{Exponential}(\lambda)$ with unknown $\lambda$
      - $p \sim \mathrm{Beta}(1, 1)$ initial prior on $p$
      - $\lambda \sim \mathrm{Gamma}(1, 1)$ initial prior on $\lambda$

## Applications -- Architecture (Stats Model)

```haskell
data Model = Model { pulls :: Int,
                     wins  :: Int,
                     money :: Double }

sample :: Model -> Random Double
sample (Model pulls wins money) = do
  x <- sampleBeta $ Beta wins (pulls - wins)
  y <- sampleGamma $ Gamma wins money
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
select arms = snd $ maxBy fst $
              map (\arm -> (sample $ model arm, arm)) arms

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

  - [W. R. Thompson. "On the likelihood that one unknown probability exceeds another in view of the evidence of two samples." _Biometrika_, 1933.](https://www.dropbox.com/s/yhn9prnr5bz0156/1933-thompson.pdf)

  - [O. Chapelle, L Li. "An empirical evaluation of Thompson sampling." _Advances in Neural Information Processing Systems_, 2011.](http://dl.acm.org/citation.cfm?id=2986710)

  - [D. Fink. "A compendium of conjugate priors." Unpublished manuscript, 1997.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.157.5540&rep=rep1&type=pdf)

  - ["Bayes' Theorem," "Bayesian inference," "Conjugate prior." _Wikipedia_.](https://en.wikipedia.org/wiki/)
