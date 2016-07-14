---
title: Find the minimum of a bitonic sequence
tags: Algorithm
---

A sequence is $a_0,\ldots,a_{n-1}$ is *bitonic* if it is a circular shift of a first non-increasing then non-decreasing sequence. 

Find an algorithm that can find the minimum value in the sequence in $O(m+\log n)$ time, where $m$ is the maximum number of repeats for a single element. This problem is a generalization of [a previous problem](/posts/2013-07-27-find-the-minimum-of-an-array.html).

A trick about circular shift of an sequence is to consider the sequence as a periodic sequence. Create sequence $b$ such that $b_i = a_i$ for $i < n$ and $b_{i+n} = b_i$ for all $i\geq n$. We only have to find one local minima in any consecutive subsequence of length $n$. This time we partition a interval into 4 pieces.

We define a valley to be 3 points $x < y < z$ such that $b_x \geq b_y$, $b_y \leq b_z$, but not $b_x=b_y=b_z$. The valley can't be too large ($|z-x|\leq n$). If we have such an valley and this valley contains a local minima, then it is easy to create a smaller (3/4 of the original size) valley that also contain the local minima. 

If $|y-x|\geq |z-y|$, pick the midpoint $w$ between $x$ and $y$, and consider the relations.
$w < y$, then we have an valley $(x,w,y)$. $w>y$, then we have an valley $(w,y,z)$, if $b_w=b_y$, consider the midpoint of $w$ and $y$ to be $u$. If $b_w=b_u=b_y$, we know at least $1/8$ of the values are the same, and do a linear search between $x$ and $z$. Otherwise, we must have $b_w > b_u < b_y$(draw it and see why) and this is a new valley!

If we have $|y-x|<|z-y|$, something similar to above can be done.

It's easy to see the recursion gets us $O(m+\log (\frac{n}{m}))=O(m+\log n)$. So the only problem comes from how do we find the first valley that contains the local minima. Let $0 < k < n$ If $b_0 > b_k$, then $b_0,b_k,b_n$ is a valley. If $b_0 < b_k$, then $b_k,b_n,b_{k+n}$ is a valley. So we pick 3 possible $k$ that is $n/4$ apart, and either one of them allow us to construct a valley, or at least $n/4$ of the points have the same value, and we use linear search.

<script src="https://gist.github.com/chaoxu/6263718.js"></script>

This algorithm is basically the simplified version of the algorithm in [Boris Veroy's paper](http://www.sciencedirect.com/science/article/pii/0885064X8990006X) that can handles repeats. 