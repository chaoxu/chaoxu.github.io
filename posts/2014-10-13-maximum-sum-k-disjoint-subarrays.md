---
title: Maximum sum $k$-disjoint subarrays
tags: classical, algorithm
---

A common problem, the [maximum subarray problem](http://en.wikipedia.org/wiki/Maximum_subarray_problem) asks the subarray with the maximum sum. 

There are many generalizations, for example into higher dimensions. In 2D, a $n\times n$ matrix, a common solution takes $O(n^3)$ time. Also, it can be generalized to trees, where node has weights. It can be generalized even further, to maximum weight connected subgraph(which is NP-hard even for planar graphs).

We consider a similar problem where instead of consider $k$ disjoint subarrays, such that the sum together is maximized. In fact, this is the [Maximum Subarray III problem on LintCode](http://www.lintcode.com/en/problem/maximum-subarray-iii/).

There is obviously an $O(nk)$ algorithm by extending the dynamic programming algorithm for the $k=1$ case.

Let $f(i,k)$ to be the maximum value obtainable by $k$ subarray of $A[1..i]$.
Let $g(i,k)$ to be the maximum value obtainable by $k$ subarray of $A[1..i]$, that uses the $i$th value in the last subarray. 
The recurrence $f(i,k) = \min(f(i,k-1),f(i-1,k),g(i,k))$, and $g(i,k) = \min(g(i-1,k)+A[i],f(i-1,k-1)+A[i])$ solves the problem.

This kind of solution would possibly work on interviews. But can we do better?
It is in fact possible to get $O(n\log n)$ with some care. 

wlog, let's assume the array is alternating, where all odd index are positive and all even index are negative. 
If we have the solution for the $k$ case, we can get a solution for $k-1$ case by either discard one of the arrays or "merge" two adjacent arrays by taking a negative piece in the middle. 

This shows that once we have the solution for the $k$ case, we can just "contract" a entire subarray into one single value. Csűrös showed that we can just use one merge operation[@Csuros04]. It find a array element with minimum absolute value, say it's $A[i]$, then it is replaced by $A[i-1]+A[i]+A[i+1]$, and then we remove $A[i-1]$ and $A[i+1]$ from the array. (For boundary cases, assume $A[0]=A[n+1]=0$).
The idea is a merge can "discard" a value, and a merge is also adding a negative piece and then do contraction. This operation is done until there are exactly $k$ positive numbers, which in that case, the best solution is to just take all $k$ of them.

Thus this implies a $O(n\log n)$ greedy algorithm, by keep merging and keep track of min absolute value item using a heap. Interestingly, this algorithm was also suggested by students in [CS 473](https://courses.engr.illinois.edu/cs473/). [Hsien-Chih](http://web.engr.illinois.edu/~hchang17/) and I discovered it is correct by failing to find counterexamples to the greedy approach.

One can see the smallest absolute value does not decrease throughout the algorithm, so instead of just keep finding and merging the item with smallest absolute value, what if one just keep merge merge item with absolute value smaller than $t$? There are three possibilities: we picked $t$ so nicely that after all the merges, we get exactly $k$ positive elements left. We picked $t$ too large, we get less than $k$ positive elements. We picked $t$ too small, and we get more than $k$ positive elements.

Bengtsson and Chen uses this idea[@Bengtsson06]. They showed they can guess $t$ in a way such that the some measure of the problem size get's smaller by at least $2/3$, and also shows how to keep track of the merges so it takes $O(n\alpha(n))$ time. Later on, they removed the need of the union-find data structure improved the time bound to $O(n)$ time[@Bengtsson07].  

Open problem: Can we find a linear time algorithm for trees? (either weights on edges or on vertices)

