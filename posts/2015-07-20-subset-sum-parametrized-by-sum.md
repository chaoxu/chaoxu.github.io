---
title: Subset sum of elements sum to $\sigma$
---

We assume the input of the subset sum problem is a sequence of $n$ positive integers that sums to $\sigma$. We are interested if there is a subsequence sums to $t$.

In this case, the subset sum problem can be solved in $O(\sigma \log^2 \sigma)$ time. In fact, it output all possible subset sums in the same running time.

Consider we partition the input into two subsequences, each have sum in between $\sigma/4$ and $3\sigma/4$, and solve each recursively then take the Minkowski sum. One can analyze this and get $O(\sigma \log^2 \sigma)$ running time. Notice if at some point, such partition cannot be found, then there is a side with a single huge element, and hence can be solved in constant time. 