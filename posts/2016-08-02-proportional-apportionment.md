---
title: Processor distribution and proportional apportionment
tags: optimization, integer
---

I saw an interview problem about assigning identical processors to embarrassingly parallel jobs. The running time of a job equals the running time on a single processor divided by the number of processors. We are interested in minimizing the maximum running time. Formally, we get the following problem.

{Problem}

    Given positive reals $a_1,\ldots,a_n$ and positive integer $k$, find non-negative integers $x_1,\ldots,x_n$, such that $\sum_{i} x_i \leq k$ and $\theta = \max_{i} a_i/x_i$ is minimized.

If there is no integral requirement on $x_i$'s, then the problem is easy. Let $A=\sum_{i} a_i$. There is a closed solution of $x_i = k \frac{a_i}{A}$, and $\theta = A / k$.

Otherwise, it is easy to check if $\theta'>0$ is a feasible solution. $\theta'$ is feasible iff $\sum_{i} \lceil a_i/\theta' \rceil \leq k$. Therefore one can apply binary search, and get the result in $O(n\log k)$ time.

One can also get a $O(n\log n)$ time algorithm. First compute $y_i = \lceil k \frac{a_i}{A} \rceil$. Greedily find a $i$ such that $a_i/y_i$ is maximized, and decrease $y_i$ by $1$. We stop when we have $\sum_{i} y_i=k$. This takes $O(\log n)$ per operation using a binary search tree. 

Linear time algorithm also exists. It is connected to proportional apportionment. This is the problem of finding the smallest $\lambda$, such that $\sum_{i} \lceil \lambda a_i \rceil = k$. Cheng and Eppstein found a $O(n)$ time algorithm [@Cheng2014]. Reitzig and Wild found a simpler algorithm later [@Reitzig2017].

There is a similar interview problem. Given $n$ points on the real line, add $k$ more points, such that it minimizes the maximum length between adjacent points. The problem is the same as the following one.

{Problem}

    Given positive $a_1,\ldots,a_n$ and positive integer $k$, find non-negative integers $x_1,\ldots,x_n$, such that $\sum_{i} x_i \leq k$ and $\theta = \max_{i} a_i/(x_i+1)$ is minimized.

The linear time algorithm for proportional apportionment should also work for the above problem. It is interesting how much can we change the problem before the linear time algorithm no longer works.