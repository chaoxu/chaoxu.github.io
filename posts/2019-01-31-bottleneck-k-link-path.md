---
title: Bottleneck $k$-link path
tags: algorithm, data structure
---

A DAG is called complete, if there are vertices $v_1,\ldots,v_n$, and $v_iv_j$ is an edge if and only if $i<j$. Let $w(i,j)$ be the edge weights from $i$ to $j$. The weight is called ordered, if $w(i,j)<w(i,j+1)$ and $w(i+ 1,j)<w(i,j)$.

{Problem}(Bottleneck $k$-link path problem)
    Find a path consists of $k$ edges from $v_1$ to $v_n$, such that the maximum weight of the edges in the path is minimized. 

One can formulate a dynamic programming algorithm, which takes $O(kn^2)$ time. My [previous writing](https://chaoxuprime.com/posts/2013-08-16-more-algorithms-on-perfectly-balanced-photo-gallery.html) shows an $O(kn)$ time algorithm using the monge property. Using binary search, there is also an $O(k\log(n/k)\log M)$ time algorithm if all weights are positive integers no larger than $M$.

We show there is an $O(n+k\log(n/k)\log n)$ time algorithm. 
Assume $\lambda^*$ is the optimal weight. First, there is an oracle that given $\lambda$, decides if $\lambda\geq \lambda^*$.
Indeed, we can apply the greedy algorithm. Find the sequence $a_1,\ldots,a_k$, as follows. $a_0=1$, $a_i$ is the largest value such that $w(a_{i-1},a_i)\leq \lambda$. If $a_k=n$, then it is clear that $\lambda \geq \lambda^*$. Also, we can show if $a_k<n$, then $\lambda < \lambda^*$. $O(n)$ time seems to be a quite large bound. We could do it in $O(k\log (n))$ instead by doing binary search for each $a_i$. Using [exponential search](https://en.wikipedia.org/wiki/Exponential_search) instead, we can obtain a $O(k\log(n/k))$ time algorithm.

One need to do binary search for $\lambda^*$. There are $\Omega(n^2)$ weights, let it be $W$. One does not have to know all of them in order to apply binary search. Note that $w$ is a matrix sorted in both row and column, hence we need a selection algorithm that returns the $k$th smallest element for such matrix. There is an [$O(n)$ time algorithm](https://chaoxuprime.com/posts/2014-04-02-selection-in-a-sorted-matrix.html) for that. Hence we can do binary search on the sorted $W$ by spending $O(n)$ time to access $i$th element. We now obtain a $O((n+ k\log(n/k)) \log n) = O(n\log n)$ time algorithm. Not bad. 

We can speed it up even further. Instead of selection in the sorted matrix, we can do [search in the sorted matrix](https://chaoxuprime.com/posts/2019-01-30-search-sorted-matrixhtml). We are given an oracle to test if a value is smaller than $\lambda^*$ after all. We can do search for $\lambda^*$ using $O(\log n)$ oracle calls and $O(n)$ time. Hence this gives us a $O(n+k\log (n/k) \log n)$ time algorithm for the problem. Whenever $k=O(\frac{n}{\log(n) \log \log n})$, this is $O(n)$ time. 

As an application, we obtain a solution to [Leetcode 410 Split Array Largest Sum](https://leetcode.com/problems/split-array-largest-sum/). The problem is also called the *linear partitioning* problem. The problem asks one to partition array into $k$ contagious subarrays that minimizes the maximum sum of each subarray. It was an example for learning dynamic programming in chapter 8.5 of [@Skiena10book]. An $O(kn^2)$ algorithm was given. Reading the discussion online, one would find $O(n\log M)$ time algorithm is the suggested solution, where $M$ is the maximum over all integers. 
The algorithm is actually fairly useful for photo galleries. There is the [NPM package `linear-partitioning`](https://www.npmjs.com/package/linear-partitioning), used by multiple photo galleries packages. My [first encountered of the problem](https://chaoxuprime.com/posts/2013-08-16-more-algorithms-on-perfectly-balanced-photo-gallery.html) was also for photo gallery. The linear partition problem reduces to the bottleneck $k$-link path problem because we can define $w(i,j)$ to be the sum of elements from the $i$th index of the array to the $j$th index of the array. After $O(n)$ preprocessing, $w(i,j)$ can be computed in $O(1)$ time. This results a $O(n+k\log (n/k) \log n)$ running time algorithm. 

What about when $k$ is large? We believe the result in [@FredericksonZ17] can be used to solve the problem in $O(n)$ time even for large $k$. 