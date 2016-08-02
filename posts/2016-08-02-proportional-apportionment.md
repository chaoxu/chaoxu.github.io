---
title: Processor distribution and proportional apportionment
tags: optimization, integer
---

I saw a interview problem about assigning identical processors to embarrassingly parallel jobs. The running time equals the running time on a single processor divided by the number of processors thrown in, and we are interested in minimize the maximum running time. Formally, we get the following nice problem.

{Problem}

    Given positive $a_1,\ldots,a_n$ and positive integer $k$, find the minimum $\theta$ that equals to $\max_{i} a_i/x_i$, where $\sum_{i} x_i \leq k$ and $x_i$'s are non-negative integers. 

If there is no integral requirement on $x_i$'s, then the problem is easy. Let $A=\sum_{i} a_i$. There is a closed solution of $x_i = k \frac{a_i}{A}$, and $\theta = A / k$.

Otherwise, it's easy to check if $\theta'>0$ is a feasible solution. $\theta'$ is feasible iff $ \sum_{i} \lceil a_i/\theta' \rceil \leq k$. Therefore one can apply binary search, and get the result in $O(n\log k)$ time.

One can also get a $O(n\log n)$ time algorithm, by first compute $y_i = \lceil k \frac{a_i}{A} \rceil$. Greedily find $i$ such that $a_i/y_i$ is maximized, and decrease $y_i$ by $1$. Until we have $\sum_{i} y_i=k$. This can be supported in $O(\log n)$ per operation using a binary search tree. 

Linear time is possible by seeing the connection to proportional apportionment. This is the problem of finding $\lambda$, such that $\sum_{i} \lceil \lambda a_i \rceil = k$. This can be shown to be solvable in $O(n)$ time using [@Cheng2014], and later a simpler algorithm[@DBLP:journals/corr/WildR15]. 