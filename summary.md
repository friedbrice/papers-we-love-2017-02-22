# Summary of _On the likelihood that one unknown probability exceeds another in the view of the evidence of two sample_ by William R. Thompson

## Section 1

### First Paragraph

Establishes the need to apply more rigorous methods to experimental data from medical research than is presently typically employed.

> not centred upon the interpretation of particular data, but ... a general interest in problems of research planning ... there can be no objection to the use of data, _however meagre,_ as a guide to action required before more can be collected.
> [Emphasis added.]

Here, Thompson is claiming that in the context of medical research, we are obligated to use all of the available data in order to inform our decisions about experiment design and treatment, even if it is a very small sample.

> Serious objection can otherwise be raised to argument based upon a small number of observations.
> Indeed, the fact that such objection can never be eliminated entirely---no matter how great the number of observations---suggested the possible value of seeking other modes of operation than that of taking a large number of observations before analysis or any attempt to direct our course.

The danger of generalizing from insufficient evidence is obvious. Less obvious is the answer to the question: how much evidence constitutes sufficient evidence? And if 10,000 observations is sufficient evidence, then is 9999 not enough to generalize?

Thompson is making a case that confidence be treated as a _sliding scale_.
If we can measure the amount of evidence an observation constitutes for the value of an unknown quantity, then we can measure how much confidence we can derive from that observation.
Then, knowledge of _both,_ (1) the estimate for the unknown, and (2) the measurement of our confidence in the estimate, can be used to guide our decisions.

Thompson is laying the foundations for applying decision theory to medical research.

### Second Paragraph

Thompson provides an example scenario.
There are two potential treatments for a life-threatening condition, $A$ and $B$.
A patient treated by either $A$ or $B$ might subsequently die.
It is unknown which treatment is _better_ (by some unstated formal criteria).
$p$ is the probability that treatment $A$ is the better, thus, $1 - p$ is the probability that $B$ is the better.
The problem: we have $n$ patients, and we must somehow decide how to treat them.

### Third Paragraph

Continuation of the above example: suppose $0.5 < p < 1$.

Now, suppose we permanently eliminate treatment $B$ for all $n$ patients.
At the time we eliminated $B$, there was still a $1 - p$ chance that $B$ was the better treatment.
All $n$ patients treated with treatment $A$ instead of treatment $B$ have a $1 - p > 0$ chance that they are receiving the inferior treatment.

From a frequentest point of view, this means that for large $n$, we have $(1 - p) n$ patients receiving the inferior treatment, which might sound acceptable if $1 - p$ is small.
However, the frequentest interpretation is not really appropriate here, because _all_ patients receive the same treatment, namely $A$.
So, even if $1 - p$ is small, we still have a $1 - p$ chance of subjecting _all_ $n$ patients to the inferior treatment.
We have $n (1 - p)$ regret.

Next, suppose we select $0.5 n$ patients to receive treatment $A$ and $0.5 n$ patients to receive treatment $B$.
Since $p > 0.5$, the overall outcome for the $n$ patients is _worse_ than if we had actually given them all treatment $A$.
This outcome, however, might be considered acceptable if the chief aim of treatment is to gather more data rather than to save the lives of the patients.
We have $0.5 n (1 - p) + 0.5 n p = 0.5 n$ regret.

Finally, suppose we choose $p n$ patients to receive treatment $A$ and $(1 - p) n$ patients to receive treatment $B$.
We have $p n (1 - p) + (1 - p) n p = 2 n p (1 - p)$ regret.

$2 n p (1 - p)$ has its maximum at $p = 0.5$.
Since $p > 0.5$, $2 n p (1 - p)$ produces less regret than the 50/50 split described above.

Notice, though that $2 n p (1 - p)$ is _not_ less than $n (1 - p)$.
This is why Thompson emphasizes the word _temporarily_ in "the expectation of such sacrifice would be _temporarily_ ... 2 p (1 - p)".

> In the _long run,_ if a real preference exists between the two _treatments,_ the expected saving by continued application of this method of apportionment rather than making immediate final decision is sensibly $1 - p$ of individuals subsequently treated.

Thought it's not explicitly stated, Thompson anticipates updating $p$ throughout the trial process, based on the results of each treatment.
As we gain more certainty that $A$ is the better, $p$ will increase, approaching $1$.
The closer $p$ gets to $1$, the closer $2 n p (1 - p)$ gets to $0$, whereas if we eliminate treatment $B$, then $p$ is fixed for all time, fixing our regret at $n (1 - p)$.

### Fourth---Sixth Paragraph

Continuing the above example, we need methods for updating $p$ based on the results of our decisions.
This is a rather general problem in probability theory not limited to medical research.

_Section 2_ gives an example where we do exactly that: we make a decisions, we take note of the outcome, and we update our beliefs according to the observed outcome.

### Seventh Paragraphs

Thompson makes the case that future medical trials should, wherever possible, be reduced to the methods demonstrated in _Section 2_.
Although it is unstated, Thompson's prescription is motivated by the desire to save the most patients in the _long run_, i.e. when the number of patients is arbitrarily large.

## Section 2

### First Paragraph

We have two infinite populations $\Omega_i$ for $i \in \{ 1, 2 \}$.
On each population, there is a random variable $X_i$:

$$
X_i \sim \textrm{Bernoulli}(\tilde{p}_i)
$$

Number of individuals exhibiting $X_i = 1$ out of $n$ drawn from population $\Omega_i$ is a random variable $R_i^n$:

$$
R_i^n \sim \textrm{Binomial}(n, \tilde{p}_i)
$$

Starting with a uniform prior for $\tilde{p}_i$, after $n_i$ trials with $r_i$ _successes_ (samples where $X_i = 1$) and $s_i$ failures (samples where $X_i = 0$), our estimate of $\tilde{p}_i is as follows:

$$
\tilde{p}_i \sim \mathrm{Beta}(r_i + 1, s_i + 1)
$$

These facts had been worked out previously in papers by Pearson and Müller, whom Thompson cites.

Modern statisticians and probability theorist will recognize the Beta distribution as being the _conjugate prior_ for the Bernoulli distribution, a conceptualization that summarizes the relationship stated above.
Thompson has the handicap of working from a lack of conceptual foundation; for example, the terms "Beta distribution" and "conjugate prior" do not appear in Thompson's paper.
In fact, Thompson's paper predates the canonization of the Beta distribution, which (when scaled and shifted) would have been called "Pearson's Type I Distribution" at the time of the paper.
As such, there were no tables available giving values for or samples from Beta distributions for various parameters.
Thus, the purpose of _Section 2_ is to numerically describe $\mathrm{Beta}(r_i + 1, s_i + 1)$ for small values of $r_i, s_i$.

### Second Paragraph

> Obviously, we may write [insert Wall-of-Math™ here].

I tend to stop reading when the word "obviously" rears its head.
Thanks, Dr. Thompson.

Courtesy to the reader aside, we have in this paragraph the answer to the central problem of the paper:

$$
P(\tilde{p}_2 > \tilde{p}_1) = \frac{
  \sum_{\alpha = 0}^{r_2} {r_1 + r_2 - \alpha \choose r_1} {s_1 + s_2 + 1 + \alpha \choose s_1}
}{
  {n_1 + n_2 + 2 \choose n_1 + 1}
}
$$

where

  * $r_1, r_2$ are the number of observed successes,
  * $s_1, s_2$ are the number of observed failures,
  * $n_1 = r_1 + s_1, n_2 = r_2 + s_2$ are the number of samples,

obtained by taking $n_1, n_2$ samples from each of $\Omega_1, \Omega_2$.

### Third Paragraph

Thompson is cleverly pointing out some symmetry in the problem statement that will be exploited later to facilitate some computations.
Use of the term "obvious" here is justified (relative to audience, or course), since he's simply reminding the reader of some basic facts of probability theory.

### Fourth Paragraph

I find it odd that Thompson has to explain how to use Pascal's Triangle, especially in the days before electronic computers.

### Fifth Paragraph

Thompson points out his use of Stirling's factorial approximation formula to facilitate computations.

### Sixth Paragraph

$\psi(r, s, r', s')$ is simply defined to be the expression in _Equation 5_, which in the context of our problem is the value we are interested in computing for small values of $r, s, r', s'$:

$$
\psi(r, s, r', s') = \frac{
  \sum_{\alpha = 0}^{r'} {r + r' - \alpha \choose r} {s + s' + 1 + \alpha \choose s}
}{
  {n + n' + 2 \choose n + 1}
}
$$

I am confused about what Thompson is claiming with the limit in Equation 11.

### Seventh Paragraph

Lots of things are obvious to Dr. Thompson.
His style of prose strikes me as being a cop-out, not to mention decidedly immodest.

The seventh paragraph is a proof of the limit statement described in the sixth, though I can't say that I follow it entirely.
Since it's obvious, though, we'll give it a pass for now.

He makes a lot of omissions in his notation.
The $\bar{R}$, $\omega_1$, and $\omega_2$ are more appropriately understood as being functions of $n_1$, thus rewriting Equation 15 as

$$
\omega_1(n_1) < \bar{R}(n_1) < \omega_2(n_1)
\text{, and }
\lim_{n_1 \to \infty} [\bar{R}(n_1)] = 1
\text{.}
$$

gives a general indication of the overall shape of Thompson's argument in this paragraph.
Unfortunately, Dr. Thompson leaves the scopes of the $\mathrm{Min}$ and $\mathrm{Max}$ operators to a context from which I cannot infer them.

### Eighth Paragraph

Multiply both sides by the (constant) denominator.

### Ninth Paragraph

Thompson here draws comparisons to Pearson's work in a vernacular that I am unacquainted with.
The point is, again, to make use of symmetry to facilitate computations.

### Tenth Paragraph

Finally, some results!
Using the machinations built up previously, we derive:

$$
\psi(r_1, s_1, 0, 0) = \frac{
  s_1 + 1
}{
  r_1 + s_1 + 2
}
$$

In other words, on the basis of only observations from population $\Omega_1$, we can make conclusions about the likelihood that $\tilde{p}_2$ is greater than $\tilde{p}_1$ (when we assume uniform priors for our estimates of $\tilde{p}_1$ and $\tilde{p}_2$).

The cases where one parameter is negative serve simply as formal idealizations in the context of the problem at hand.

### Eleventh Paragraph

Thompson draws comparisons to Pearson's work.
Pearson was using similar methods to address a related problem, and the solutions of many of Pearson's questions are answered by the values of the $psi$ function as well, for certain choices of the parameters.
Thus, the values of the $psi$ function are useful for answering a range of related questions.

### Twelfth---Fourteenth Paragraph

A new notation is introduced to help facilitate computations: $N(r, s, r', s')$ and $D(r, s, r', s')$.

$$
N(r, s, r', s') =
  \sum_{\alpha = 0}^{r'}
  {r + r' - \alpha \choose r}
  {s + s' + 1 \alpha \choose s}
$$

and

$$
D(r, s, r', s') = D(n, n') =
  {n + n' + 2 \choose n + 1}
$$

where $n = r + s, n' = r' + s'$.

In this notation, we have $\psi(r, s, r', s') = N(r, s, r', s') / D(r, s, r', s')$.

Several inductive formulas and base cases are derived for $N$ and $D$, whereby $N$ and $D$ may be calculated recursively.

### Fifteenth Paragraph

The inductive relations above do not completely characterize the $N$ and $D$ functions.
In order to calculate $\phi$, we still need a table of values of $N$ and $D$ for all tuples of positive integers $(r, s, r', s')$ where (1) $r + s \geq r' + s'$, and (2) $r \geq s$.

### Sixteenth Paragraph

Expanding the above recursive relations on the $N$ function, we find that calculating its values reduces to summing certain values of the $D$ functions, and we are given

$$
N(r, s, r', s') =
  \sum_{\alpha = 0}^{r'}
  D(r - 1, r' - 1 - \alpha)
  D(s - 1, s' + \alpha)
$$

thus we really only need to calculate the values of the (two-parameter) $D$ function.

Finally, Thompson provides us a table of values of $N$ and $D$ for some small values of the arguments.
Recall $\psi$ is the probability that $\tilde{p}_2 > \tilde{p}_1$ given $r$ successes and $s$ failures from Population 1 and $r'$ successes and $s'$ failures from Population 2.
Thus, the table solves the posed problem for small numbers of samples.

## Section 3

### First Paragraph

The table and identities of _Section 2_ cover all cases where $n, n' \leq 5$ and make it possible in those cases to select a treatment based on the probability that it is the better treatment, as suggested in _Section 1_.

### Second Paragraph

It'd be very beneficial to have a table of values of $D(n, n')$, or even just approximate values.

### Third Paragraph

Tabulation of the $psi$ function and Pearson's _hypergeometric series_ are equivalent.

### Fourth Paragraph

A future paper will return to the topic of the _method of apportionment_ (i.e., the actual Thompson Sampling algorithm).
