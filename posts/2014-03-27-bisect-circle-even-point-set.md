---
title: Bisect circle for a balanced set of points
---

{Problem}
    $S$ is a set of $n$ points on the unit circle. No two points in $S$ lies on the same diagonal. Find a line that passes through the origin that divide the unit circle into two open semicircles, such that each piece have the same number of points in $S$.

A harder version of this problem is [Problem 1 in UIUC's 1995 Fall Theory Qual](http://sarielhp.org/research/algorithms/quals/19xx/1995-a.ps).
Let $C$ be the unit circle. The angle made by the lines and the $x$-axis uniquely defines the line. Let the line pass through the origin with an angle $\theta$ be $l_\theta$. Let $C(a,b) = \cup_{\theta \in (a,b)} l_\theta \cap C$, $L$ is the semicircle below the $x$-axis, $U$ is the semicircle above the $x$-axis. $U(a,b)$ and $L(a,b)$ are define the same way as $C(a,b)$. 

We can solve this problem in $O(n)$ time by reducing it to a somewhat more general problem.

{Problem}
    Given two arcs $L(a,b)$ and $U(a,b)$ and a set of points $S$ on lying on the two arcs. $|S\cap L(a,b)|\geq k$ and $|S\cap U(a,b)|\leq k$. $|l_\theta\cap S|\leq 1$ for all $\theta\in(a,b)$. Find a $l_\theta$, such that $\theta\in(a,b)$ and $|S\cap (L(a,\theta)\cup U(\theta,b))|=k$ in $O(|S|)$ time.

Let's consider an algorithm that returns line $l_\theta$ be $partition(S,a,b,k)$.

Such a line must exist. Proof left as an exercise to the reader.  

Note if we can solve this problem. We just need to rotate the circle so the points in the upper semicircle is at most the number of points in the lower semicircle, and then compute $partition(S,0,\pi,n/2)$.

Let's consider how to compute $partition(S,a,b,k)$. First, note for any line, we can decide the number of points in the intersection of constant number of half planes in linear time. This allow us to do cardinality computations of points lying on some arc in linear time.

We can find the $i$th point on an arc from the left by using a linear time selection algorithm.

If the lower arc contains more than $2k$ points, find the $k$th point from the left in the lower arc. Assume it intersects $l_\theta$. We return $partition(S\cap C(a,\theta),a,\theta,k-|S\cap U(a,\theta)|)$.

So we are only dealing with the case where the lower arc contains less than $2k$ points. We find the $k/2$th point from the left in the lower arc. Assume it intersects $l_\theta'$, and let $\theta=\theta'+\epsilon$ for a small enough epsilon(which would be apparent how small it has to be, and it can be found in linear time also.). Let $(u,v,y,x) = (|S\cap L(a,\theta)|,|S\cap L(\theta,b)|,|S\cap U(a,\theta)|,|S\cap U(\theta,b)|)$. Note $u=k/2$ and $u+x = |S\cap (L(a,\theta)\cup U(\theta,b))|$, $x+y\leq k$, $u+v\geq k$.

- If $u+x = k$, then we are done, return $l_\theta'$.
- If $u+x > k$, then return $partition(S\cap C(a,\theta),a,\theta,k-x)$.
- If $u+x < k$, then return $partition(S\cap C(\theta,b),\theta,b,k/2)$.

![Bisect Circle Example](/files/bisect_circle.png)

One can verify it's valid to call the partition functions, namely the precondition for the number of points lower arc and upper arc is satisfied. There might be some off by one error somewhere. But the general idea is there.

When $k$ is small enough we solve the problem by brute force.

Every time we nest a $partition$ call, we spend linear time on the current point sets, then we call the function again but with a point set of size a constant times smaller. This give us the linear time algorithm required.

This is actually a special case of the ham sandwich problem in $2$D. There are $n$ red points and $m$ blue points on the plane. Find a line such that it divides the plane into two half-planes, so the interior of each half-plane contains at most $\lfloor n/2 \rfloor$ red points and $\lfloor m/2 \rfloor$ blue points. So our problem is when the red point is purely the origin.