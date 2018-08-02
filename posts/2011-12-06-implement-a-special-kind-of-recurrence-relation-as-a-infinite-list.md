---
title: Implement a special kind of recurrence relation as a infinite list
tags: Haskell
---
A common recurrence has the form
\[
a_n = \begin{cases}
   \sum_{i=0}^\infty b_ia_{n-m_i} &\text{if }n>k\\
   c_n & \text{if }n\leq k
       \end{cases}
\]
, where $m_i$ and $b_i$ are both *infinite* sequences, and $c_i$ is a finite sequence. $m_i\in \mathbb{N}$. $a_{-i}=0$ for all positive $i$. This is well defined as long as $b_i, a_i, c_i$ are in the same ring.

One can use a balanced binary tree to store the entire infinite list, and the time to generate the $n$th element is $O(d(n)\log n)$, where $d$ is the density function of $\{m_i\}$.

Using an array would make it $O(d(n))$, but it is too imperative for our taste, how about we only use list and achieve $O(d(n))$ time, elegantly?

The idea is that we are summing the first item of infinite many stacks. However we don't have to really sum the infinite stacks, we only sum the stack we require. [The code](https://gist.github.com/1438136): 

<script src="https://gist.github.com/1438136.js?file=rec.hs"></script>

What's a practical usage of this? Produce a infinite list of partition numbers, such that finding the first $n$ elements can be done in $O(n^{\frac{3}{2}})$ time and 3 lines of code. [The code](https://gist.github.com/1438142)

<script src="https://gist.github.com/1438142.js?file=PartitionNumbers.hs"></script>