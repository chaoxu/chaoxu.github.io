---
title: Shooting balloons and problems on circular arcs
---

UIUC theory qualify exam has a problem on shooting balloons. There are $n$ balloons(disks) on the plane, and you are standing at a given point. Find the minimum number rays to shoot so you can pop all the balloons. 

It reminds me of the interval version of the problem.

{Problem}
	
	There are $n$ interval on the real line, find minimum number of points on the line such that every interval contains at least one of the points.

There is a common $O(n\log n)$ algorithm where the time is dominated by sorting. Pick the left most right end point of the uncovered intervals until all intervals are covered. 

Almost all interval related problem can be generalized to circular arcs. There are a certain set of problems on circular arcs that seems quite easy to solve. Wlog, assume no circular arc completely contain another. 

{Problem}
	
	There are $n$ arcs on the unit circle, find minimum number of points on the circle such that every arc contains at least one of the points.

We can use $O(n\log n)$ time to sort. Break the circle at the start of some arc and consider the real line version of the problem. Total of $O(n^2)$ time because there are $n$ possible starting positions. 

However, one can show any starting arc can get you the same solution, and use the same algorithm as the interval version! In fact, this idea works for a few other problems too, include maximum independent set of arcs. [@Hsu1991]

Each arc $a$ can be specified by two points, $h(a)$ and $t(a)$, where all the points of the arc is exactly the points traced from $h(a)$ to $t(a)$ counterclockwise. We define a relation on arcs. The next arc of $a$ is defined to be the first arc with the property that it is contained in the arc traced from $t(a)$ to $t(b)$ counterclockwise.  

For interval graphs, certain problems are hard to come up with a combinatorial algorithm, but because of interval graph induces a totally unimodular matrix, there are still ways to solve the problem. For example, find a maximum weight independent set requires some dynamic programming, but what about finding a maximum weight set where we allow each arc to intersect at most $k$ times? [@Winkler]

For proper circular arc graphs, the adjacency matrix is not totally unimodular, however, it is nearly totally unimodular, which has nice properties like totally unimodular matrices. One particularly interesting one was conjectured to be NP-hard, but one can devise a combinatorial algorithm using nearly totally unimodular formulation. [@Gijswijt].

{Problem}

	Given $\alpha$ and set $S$ of $n$ points on the unit circle. A set of arcs each with measure $\alpha$ is independent if each arc covers at most $1$ point in $S$. Find the minimum number of independent set of arcs such that every point is covered by some independent set of arcs.

# References