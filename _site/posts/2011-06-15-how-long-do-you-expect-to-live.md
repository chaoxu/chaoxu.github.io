---
title: How long do you expect to live?
tags: probability
---

While discussing conditional probability, someone said the following: 

{Problem}
    The expected life expectancy of some country is 70, and there exist people who die at every age before 70. What is the expected life expectancy for a 60 year old?

Most people would answer 10. However, he continuous:

> It could be 10, but for many distributions, it's likely more than that. You can convince yourself by thinking about the expected life expectancy for a 80 year old.

The quote above would follow directly from the proof of the following theorem:

{Theorem}
    For any real random variable $X$, if $\Pr(X\geq a)>0$, $E[X|X\geq a] \geq E[X]$.
 
{Proof}
    Let $c = \Pr(X\leq a)$ 
    \begin{align*}
    E[X] &= \int_{-\infty}^\infty x \Pr(X=x) dx\\
     &=\int_{-\infty}^a x \Pr(X=x) dx + \int_a^\infty x \Pr(X=x) dx\\
     &=\int_{-\infty}^\infty x \Pr(X=x|X\leq a)\Pr(X\leq a) dx + \int_{-\infty}^\infty x \Pr(X=x|X\geq a)\Pr(X\geq a) dx\\
     &=c\int_{-\infty}^\infty x \Pr(X=x|X\leq a) dx + (1-c)\int_{-\infty}^\infty x \Pr(X=x|X\geq a) dx \\
     &=cE[X|X\leq a] + (1-c)E[X|X\geq a] \\
    \end{align*}

    If $a = \lambda b + (1-\lambda) c$, where $\lambda \in [0,1]$, then $a \leq \max(b,c)$. Because $E[X|X\leq a]\leq a \leq E[X|X\geq a]$, $E[X] \leq E[X|X\geq a]$. 

In fact, one can easily modify the above proof and prove the next theorem:
{Theorem}
    For any real random variable $X$, if $x\geq y$ and $\Pr(X\geq x)>0$, then $E[X|X\geq x] \geq E[X|X\geq y]$. 

A heuristics conclusion: The longer you lived, you expect to live longer.