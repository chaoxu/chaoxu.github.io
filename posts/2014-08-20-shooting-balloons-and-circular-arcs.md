---
title: Shooting balloons and problems on circular arcs
---

The Fall 1999 [UIUC theory qual](https://sarielhp.org/research/algorithms/quals/) has a problem on shooting balloons. There are $n$ balloons (disks) on the plane, and you are standing at a given point. Find the minimum number rays to shoot so you can pop all the balloons. 

It reminds me of the interval version of the problem.

{Problem}
  
  There are $n$ interval on the real line, find minimum number of points on the line such that every interval contains at least one of the points.

There is a common $O(n\log n)$ algorithm where the time is dominated by sorting. Pick the left most right end point of the uncovered intervals until all intervals are covered. 

Almost all interval related problem can be generalized to circular arcs. There are a certain set of problems on circular arcs that seems quite easy to solve. Wlog, assume no circular arc completely contain another. 

{Problem}
  
  There are $n$ arcs on the unit circle, find minimum number of points on the circle such that every arc contains at least one of the points.

We can use $O(n\log n)$ time to sort. Break the circle at the start of some arc and consider the real line version of the problem. Total of $O(n^2)$ time because there are $n$ possible starting positions. 

However, one can show any starting arc can get you the same solution, and use the same algorithm as the interval version! In fact, this idea works for a few other problems too, include maximum independent set of arcs. [@HsuT91]

For interval graphs, if it is hard to come up with a combinatorial algorithm, one based on LP might still work. The interval graph induces a totally unimodular matrix. For example, find a maximum weight independent set on a interval graph can be solved with dynamic programming, but what about finding a maximum weight set where we allow each arc to intersect at most $k$ times? [@WinklerZ03]

For proper circular arc graphs, the adjacency matrix is not totally unimodular, however, it is nearly totally unimodular, which has nice properties like totally unimodular matrices. One particularly interesting problem is the following.
An arc is an $\alpha$-arc, if the arc length of the arc is at most $\alpha$.
Given a set of points on the circle, one is interested in finding a covering using disjoint set of $\alpha$-arcs. The problem was conjectured to be NP-hard, but one can devise a combinatorial algorithm using nearly totally unimodular formulation [Gijswijt05].