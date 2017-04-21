---
title: List the smallest $k$ subset sums
tags: algorithm
---

{Problem}

    Given a set of positive reals $\{x_1,\ldots,x_n\}$ where $x_1<x_2<\ldots<x_n$, 
    find the smallest $k$ subset sums.

We can assume $n\leq k$, as there is no need to use $x_j$ where $j>k$. 

Torsten Gross and Nils Blüthgen posted [a $O(k^2)$ time solution on arXiv](https://arxiv.org/abs/1704.05795).

We show a $O(k\log k)$ time algorithm, which is optimal if we want to output the numbers in order.

The idea is that we list sums one by one, and maintain what sum to pick next through a priority queue. We start with the empty set. Assume that we added the sum induced by $I\subset [n]$ (that is, $\sum_{i\in I} x_i$) into the output, let $j=1+\max I$. Now we can consider two possibilities by extending the current solution: the sum induced by $I\cup \{j\}$ will be in the output, the sum induced by $I\cup \{k\}$ where $k>j$ will be in the output. We will add both possibilities to the queue so that one can check them later.
Also, there is no need actually to store the sets, only the values.

Here is a quick python implementation. 

```python
def first_k_subset_sums(x,k):
    n = len(x)
    h = []
    output = [0] # need to account for the empty set
    heapq.heappush(h,(x[0],0))
    while h and len(output)<k:
        (u,b) = heapq.heappop(h)
        output.append(u)
        if b+1<n:
            heapq.heappush(h,(u+x[b+1],b+1))
            heapq.heappush(h,((u-x[b])+x[b+1],b+1))
    return output
```

If we want to output the sets themselves, not just the values, does the running time change? If a set $I$ is in the output, then all subsets of $I$ must also be in the output. Hence the largest set we can ever output has size $O(\log k)$. Therefore the total output length is at most $O(k\log k)$.

This is also a lower bound. Consider when $x_i=2^i$, then we will output all subsets of $\{x_1,\ldots,x_{\log k}\}$, and we know that $\sum_{i=1}^{\log k} i{\log k\choose i} = \Omega(\log k)$.

If we don't have to list the smallest $k$ subset sum values in order, then $O(k)$ is possible, see [this mathoverflow answer](https://mathoverflow.net/a/222341/6886) by [David Eppstein](https://www.ics.uci.edu/~eppstein/).

If instead, we ask for the smallest $k$ distinct subset sum. I don't know of any algorithm that performs better than $O(nk)$, even if we know that $n=\Omega(k)$.

# Acknowledgements {-}

I would like to thank Tana Wattanawaroon for helpful discussions and taking an interest in this problem. 