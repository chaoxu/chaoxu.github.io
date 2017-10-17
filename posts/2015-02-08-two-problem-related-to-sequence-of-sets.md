---
title: Two problem related to sequence of sets
tags: algorithm
---

{Problem}
    Given a sequence of sets $S_1,\ldots,S_n$ containing a total of $m$ integers, and a integer $k$.
    Decide if there exists $i$ and $j$ such that $i\neq j$ and $|S_i\cap S_j|\geq k$.

We assume the elements in the sets are in $[m]$. Let $S=\bigcup_{i=1}^n S_i$.

For $k=0,1$, we can solve it in $O(m)$ time: Decide if any element appears more than once in the sets.

For larger $k$, we shall compute $|S_i\cap S_j|$ for every pair $i$ and $j$. To do this, we start with an all zero $n\times n$ matrix $C$. At the end of the algorithm, $C_{i,j} = |S_i\cap S_j|$ for all $i,j\in [n]$. For each element $x$, we find $E_x = \set{i|x\in S_i}$. This takes $O(m)$ time. We increment $C_{i,j}$ for all $i,j\in E_x$. 
We claim this algorithm have running time $O(nm)$. Indeed, for each $x$, we spend $|E_x|$ time in incrementing $C_{i,j}$ where $i,j\in E_x$. Hence the running time is bounded by $\sum_{x\in S} |E_x|^2$. We know $\sum_{x\in S} |E_x|=m$ and $|E_x|\leq n$. We see the worst case is when $|E_x|=n$ and $|S|=m/n$. In that case, we have running time $O(\sum_{x\in S} n^2)=O(mn)$. 

Since we just want to find a pair $\set{i,j}$ where $|S_i\cap S_j|\geq k$. We can stop the algorithm as soon as $C_{i,j}\geq k$ for some $i$ and $j$. This means we can increment at most $(k-1)n^2$ times.

Together, the running time become $O(\min(nm,k n^2+m))$.

For $k=2$. One can improve the running time when $n$ is large by reduce it to a problem similar to [finding rectangles](/posts/2015-02-02-rectangle-in-point-set.html). Together, the final running time for $k=2$ is $O(\min(nm,m^{3/2},k n^2+m))$.

{Problem}
    Given a sequence of sets $S_1,\ldots,S_n$ with a total of $m$ elements. Partition $[n]$, such that if $i,j$ is in the same partition, then $S_i = S_j$.

The idea is basically build a trie for the bit vector representation of the sets. We will be a bit more clever and skip all the $0$ elements. We should get a $O(m)$ time algorithm.