---
title: Rectangles in point set
tags: classical, algorithm, computational geometry
---

{Problem}
    Given $n$ points in the plane $P$, find if any $4$ of them are the vertices of some axis-aligned rectangle.

The idea is we guess the left vertical segment of the rectangle, and see what would be the right vertical segment. If we pick an left vertical line $l$, and then for each $(x,y)\in l\cap P$, we consider all the points with the same $y$ coordinate and to the right of $x$, and add a counter to the vertical lines that contains it. This can be done in linear time with respect to number of vertices with the same $y$ coordinate if one already builds a data structure before hand. If any counter become $2$, then we are done. 

It is a $O(n^2)$ time algorithm, since the time it takes is at most $O(n)$ to guess each left vertical segment. One could, however analyze this algorithm better, and realize the time is actually $O(lk)$, where $l$ is the maximum number of points on a horizontal line and $k$ is maximum number of points on a vertical line.

There is an $O(n^{3/2})$ algorithm [@vanKreveld1990], using this $O(n^2)$ algorithm as subroutine.

We consider the set of vertical lines that contains at least $2$ points. This give us two set of vertical lines, $S$ are the lines with at most $\sqrt{n}$ points and $L$ are the lines contain at least $\sqrt{n}+1$ lines.

There are 3 possibilities. There is a rectangle with two sides contained in $S$, or one side in $S$ one in $L$, or both in $L$.

For the first case, just use out $O(lk)$ algorithm, which gives us $O(n^{3/2})$ time.

For the second case, consider we pick one line in $S$ and the union of all the points in the lines in $L$. For each point in $S$, we will find if this point is in some line in $L$, if it is, we increment a counter for that line. We would increment at most $\sqrt{n}$ counters. The running time is therefore $\sum_{l\in S} |l\cap P|(\log n + \sqrt{n}) = O(n^{3/2})$.

Once we are done with the first two case, consider remove all the points lying on the small lines. Now we only have large lines. Since there are at most $\sqrt{n}$ large lines, we can rotate the plane and run the algorithm again, but this time, we know all the lines are small.