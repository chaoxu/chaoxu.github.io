---
title: Maximum sum $k$-disjoint subarrays
tags: classic, algorithm
---

# Introduction

A common problem, the [maximum subarray problem](http://en.wikipedia.org/wiki/Maximum_subarray_problem) asks the subarray with the maximum sum. 

There are many generalizations, for example into higher dimensions. In 2D, a $n\times n$ matrix, a common solution takes $O(n^3)$ time[@TamakiT98;@Takaoka02]. There is no $O(n^{3-\e})$ algorithm assuming All-Pairs Shortest Paths cannot be solved in $O(n^{3-\e})$ for some constant $\e>0$ [@BackursDT16]. 

Another way is to understand it as a graph problem. We are given a path, and there are weights on the vertices. We require a maximum weight connected subgraph. The problem is NP-hard even for planar graphs. However, it is solvable in polynomial time for bounded treewidth graphs [@Álvarez-MirandaLM13].

We consider a similar problem where instead of a single subarray, we want at most $k$ disjoint subarrays, such that the sum together is maximized. In fact, this is the [Maximum Subarray III problem on LintCode](http://www.lintcode.com/en/problem/maximum-subarray-iii/).

::: {.Problem title="Maximum $k$-Disjoint Subarray Problem"}
  Given array $A[1..n]$, find a non-decreasing sequence of indices $i_1,\ldots,i_{2k}$, such that $\sum_{i=1}^k \sum_{j=i_{2i-1}}^{2i} A[j]$ is maximized.
:::
There is obviously an $O(nk)$ algorithm by extending the dynamic programming algorithm for the $k=1$ case.

# Solutions

## Dynamic Programming

Let $f(i,k)$ to be the maximum value obtainable by $k$ subarray of $A[1..i]$.
Let $g(i,k)$ to be the maximum value obtainable by $k$ subarray of $A[1..i]$, that uses the $i$th value in the last subarray. 
The recurrence $f(i,k) = \min(f(i,k-1),f(i-1,k),g(i,k))$, and $g(i,k) = \min(g(i-1,k)+A[i],f(i-1,k-1)+A[i])$ solves the problem.

## Greedy

This kind of solution would possibly work on interviews. But can we do better?
It is in fact possible to get $O(n\log n)$ with some care. 

wlog, let's assume the array is alternating, where all odd index are positive and all even index are negative. 
If we have the solution for the $k$ case, we can get a solution for $k-1$ case by either discard one of the arrays or "merge" two adjacent arrays by taking a negative piece in the middle. 

This shows that once we have the solution for the $k$ case, we can just "contract" a entire subarray into one single value. Csűrös showed that we can just use one merge operation[@Csuros04]. It find a array element with minimum absolute value, say it's $A[i]$, then it is replaced by $A[i-1]+A[i]+A[i+1]$, and then we remove $A[i-1]$ and $A[i+1]$ from the array. (For boundary cases, assume $A[0]=A[n+1]=0$).
The idea is a merge can "discard" a value, and a merge is also adding a negative piece and then do contraction. This operation is done until there are exactly $k$ positive numbers, which in that case, the best solution is to just take all $k$ of them.

Thus this implies a $O(n\log n)$ greedy algorithm, by keep merging and keep track of min absolute value item using a heap. Interestingly, this algorithm was also suggested by students in [CS 473](https://courses.engr.illinois.edu/cs473/). [Hsien-Chih](http://web.engr.illinois.edu/~hchang17/) and I discovered it is correct by failing to find counterexamples to the greedy approach.

## Speed up greedy

One can see the smallest absolute value does not decrease throughout the algorithm, so instead of just keep finding and merging the item with smallest absolute value, what if one just keep merge merge item with absolute value smaller than $t$? There are three possibilities: we picked $t$ so nicely that after all the merges, we get exactly $k$ positive elements left. We picked $t$ too large, we get less than $k$ positive elements. We picked $t$ too small, and we get more than $k$ positive elements.

Bengtsson and Chen uses this idea[@Bengtsson06]. They showed they can guess $t$ in a way such that the some measure of the problem size gets smaller by at least $2/3$, and also shows how to keep track of the merges so it takes $O(n\alpha(n))$ time. Later on, they removed the need of the union-find data structure improved the time bound to the optimal $O(n)$ time [@Bengtsson07]. 

## Optimal running time reducing to $k=1$ queries

There are other approaches to obtain the same running time. We can consider a query version of the problem when $k=1$. 
Given indices $i$ and $j$, find indices $i'$ and $j'$ such that $i\leq i'\leq j'\leq j$, and sum of the elements in $A[i'..j']$ is maximized. Chen and Chao showed how to use a data structure that can be built in $O(n)$ time, and return the solution to the above query in $O(1)$ time [@ChenC07]. It is not a simple data structure. Gawrychowski and Nicholson showed such data structure can be used to solve the [Problem 1] in $O(n)$ time [@GawrychowskiN15]. The reduction is easy, but again the bottleneck is the heavy hammers to build the data structure. 

## A very simple solution

Recently, I've seen a truly simple result. A related problem is the following.

::: Problem
  Given array $B[1..n]$, find a non-decreasing sequence of indices $i_1,\ldots,i_{2k}$, such that $\sum_{i=1}^k B[i_{2i}]-B[i_{2i-1}]$ is maximized.
:::

This problem is featured in interviews, and is also [on leetcode as Best time to buy an sell stock IV](https://leetcode.com/problems/best-time-to-buy-and-sell-stock-iv/) and showed up in codeforces as [stock trading](http://codeforces.com/contest/391/problem/F3). 
[Problem 1] and [Problem 2] can be reduced to each other in linear time. For one direction, we can define $B[i]=\sum_{j=1}^i A[j]$. The other direction, we let $A[i]=B[i]-B[i-1]$ for all $i$. The editorial in codeforces showed [a solution similar to [@Bengtsson07]](http://codeforces.com/blog/entry/10727) for [Problem 2].

A surprising algorithm for [Problem 2] was found by [Zhiqing Xiao](https://zhiqingxiao.weebly.com) that claims to solve the problem in $O(n)$ time by building upon the [observation of leetcode user yishiluo](https://leetcode.com/problems/best-time-to-buy-and-sell-stock-iv/discuss/54118/C%2B%2B-Solution-with-O(n-%2B-klgn)-time-using-Max-Heap-and-Stack). Hence it shows [Problem 1] can be solved in linear time, and the only (a bit) heavy hammer is the [selection algorithm](https://en.wikipedia.org/wiki/Selection_algorithm). Although the solution is simple, it is fairly unclear how to prove correctness. [Fangrui Song](http://maskray.me) wrote [a better explanation](http://maskray.me/blog/2015-03-27-leetcode-best-time-to-buy-and-sell-stock-iv) in Chinese. Although it still does not fully prove correctness, it is a step toward a proof. 

# Open problem

Can we find a linear time algorithm for trees? (either weights on edges or on vertices)
