---
title: Minimum area rectangle that enclose a set of rectangles
tags: computational complexity, NP
---

I was asked the following question by a friend who seeks an algorithm to the problem. I'm sure it is well studied but I can't find any relevant information:

{Problem}
    Given a set of axis-aligned rectangles, one want to arrange them on the plane by translations, such that the smallest rectangle covering them is minimized. What is the minimum area of such a covering rectangle?

The problem have this NP-Hard feel to it. Indeed I am able to reduce it to set partition.

Consider the set of blocks are of the size $1\times a_1,\ldots,1\times a_n$, $1\times (\frac{1}{2}\sum_{i=1}^n a_i)$. It is easy to show there is a set partition of $\{a_1,\ldots,a_n\}$ iff the minimum area of the covering rectangle is $3(\frac{1}{2}\sum_{i=1}^n a_i)$. (As long as the sum is larger than 3.)

I particularly liked this reduction so I posted it here. 