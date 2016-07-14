---
title: Two problem related to sequence of sets
tags: algorithm
---

Given a sequence of sets $S_1,\ldots,S_n$. $\sum_{i=1}^n |S_i|=m$. The elements in the sets are ordered. We are interested in the following problems

{Problem}
    Decide if there exist $i\neq j$ such that $|S_i\cap S_j|\geq k$.

For $k=0,1$, we can solve it in $O(m \log m)$ time: Take the union and see if there is any repeats. This goes to element distinctness problem.

For larger $k$, we look through each element, and find all sets containing that element(this can be done in $O(m\log m+nm))$ time). For each pair of sets containing that element, say if $i,j$ are such pair, we increment a counter in $D[i,j]$. Then we look through the table until we find a position where $D[i,j]\geq k$. Total this is a $O(m\log m+nm)$ time algorithm. One can improve the running time when $n$ is large by reduce it to a similar problem of [finding rectangles](http://www.chaoxuprime.com/posts/2015-02-02-rectangle-in-point-set.html).

{Problem}
    Partition $[n]$, such that if $i,j$ is in the same partition, then $S_i=S_j$.

The idea is basically build a trie for the bit vector representation of the sets. Except we will be a bit more clever and skip all the $0$ elements. We should get a $O(m\log m)$ time algorithm.