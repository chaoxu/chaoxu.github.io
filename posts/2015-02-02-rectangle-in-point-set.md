---
title: Rectangles in point set
tags: classical, algorithm, computational geometry, graph theory
---

::: Problem
  Given $n$ points in the plane $P$, find if any $4$ of them are the vertices of some axis-aligned rectangle.
:::
The idea is we guess the left vertical segment of the rectangle, and see what would be the right vertical segment. If we pick an left vertical line $l$, and then for each $(x,y)\in l\cap P$, we consider all the points with the same $y$ coordinate and to the right of $x$, and add a counter to the vertical lines that contains it. This can be done in linear time with respect to number of vertices with the same $y$ coordinate if one already builds a data structure before hand. If any counter become $2$, then we are done. 

It is a $O(n^2)$ time algorithm, since the time it takes is at most $O(n)$ to guess each left vertical segment. One could, however analyze this algorithm better, and realize the time is actually $O(ln)$, where $l$ is the maximum number of points on a horizontal line. 
Indeed, we look at a horizontal line that contain $l$ points, and realize for each point on the horizontal line, we need to increment a counter at most $l$ times.  

We can also solve the problem in $O(t^2+n)$ time, where $t$ is the number of vertical lines. Note since we can stop as soon as a counter become $2$. The number of counter we will increase is at most $t^2$. Indeed, the counter depend only one two vertical lines. 

There is an $O(n^{3/2})$ algorithm [@vanKreveldD91], using this $O(n^2)$ algorithm as subroutine.

We consider the set of vertical lines that contains at least $2$ points. This give us two set of vertical lines, $S$ are the lines with at most $\sqrt{n}$ points and $L$ are the lines contain at least $\sqrt{n}+1$ lines.

There are 3 possibilities. There is a rectangle with two sides contained in $S$, or one side in $S$ one in $L$, or both in $L$.

For the first case, just use our $O(ln)$ algorithm, which gives us $O(n^{3/2})$ time.

For the second case, consider we pick one line in $S$ and the union of all the points in the lines in $L$. For each point in $S$, we will find if this point is in some line in $L$, if it is, we increment a counter for that line. We would increment at most $\sqrt{n}$ counters. The running time is therefore $\sum_{l\in S} |l\cap P|(\log n + \sqrt{n}) = O(n^{3/2})$.

Once we are done with the first two case, consider remove all the points lying on the small lines. Now we only have large lines. Since there are at most $\sqrt{n}$ large lines, we can rotate the plane and run the algorithm again, but this time, we know all the lines are small.

As noted in [@AlonYZ97], rectangle finding problem can be reduced to finding a $C_4$ in a bipartite graph. The vertices are $X=\set{x|(x,y)\in P}$ and $Y=\set{y|(x,y)\in P}$, and the edges are $P$. So this is a nice interaction with graph theory.
In the same article, some stronger bounds were established. First, if the graph have [degeneracy](https://en.wikipedia.org/wiki/Degeneracy_(graph_theory)) $d$, the running is $O(md)$. They used the fact and lead to finding a $C_4$ takes $O(m^{4/3})$ time [@AlonYZ97]. 

We briefly mention how this is done.

First, it is known that if $m \geq c n^{1/2}$, then there exists a $C_4$. 
If $d\geq c n^{3/2}$, then one can find a subgraph that has degree at least $d$ in $O(m)$ time. Note this implies in the subgraph, there exists a $C_4$, which there is a particular algorithm to solve for it.


