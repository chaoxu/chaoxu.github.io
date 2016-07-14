---
title: Given sequence of angles, find a polygon
tags: computational geometry
---

{Problem}
    Find a polygon $p_1\ldots p_n$, such that the angle at vertex $p_i$ is $\alpha_i$.

Define $p_{n+1} = p_1$ for a polygon $p_1\ldots p_n$.
If one consider a polygon's sides are vectors, then a polygon is set of vectors that equals to 1, such that the magnitude of the vectors are greater than 0.

Angles at each vertex determines the direction of the vector. Let $u_i$ be the unit vector with direction of $p_ip_{i+1}$, we have

\[
\sum_{i=1}^n c_iu_i = 0
\]

$c_i>0$ for all $i$. $u_i$ can be computed from $\alpha_i$ trivially. We assume we have normalized the polygon by rotate it, so $u_1=(0,1)$. Once we find a set of $c_i$, then we determine a solution to the problem.  $(c_1,\ldots,c_n)$ is in the intersection of the solutions of two set of linear systems. One might first calculate the solution to the linear systems individually, then take the intersection, and pick a point where all coordinates are positive.

This is not easy to code, and it take $O(n^3)$ time to just solve the linear system. A better and faster solution exploits the fact we are operating in a small dimension.

Observe that a polygon lies on a plane, a 2D space, we might think we can arbitrarily put down the length of $n-1$ vectors, and calculate the last one from it. It is not true, the $c_i>0$ condition shows $\{c_1u_1+c_2u_2|c_1,c_2>0\}$ does not contain the origin unless $u_1=-u_2$.

If we can find $u_1,u_2,u_3$, such that the convex hull of them contains the origin (here the convex hull means the open polygon contained inside), then clearly we can use $u_1,u_2,u_3$ to span the entire plane. The span here is defined as all the values can be obtained from conical combination. This inspires the following algorithm:

Find $u_x,u_y,u_z$ that spans the entire plane by checking if the convex hull contains $(0,0)$. Arbitrarily assign $c_i$, such that $i\neq y,z$. Calculate $c_y$ and $c_z$ from the assignment.

Once $u_x,u_y,u_z$ are found, only $O(n)$ time is required to find the polygon. Find the three spanning vectors is linear time. Pick any two adjacent vectors as $u_x,u_y$, and do a search for $u_z$ that satisfy the condition. One can find a solution in $O(n)$ time.

If the algorithm fails, there are two possibilities: 

1. There is no such polygon. 
2. There are only four directions, and they are exactly $(1,0),(0,1),(-1,0),(0,-1)$. We can solve this special case by arbitrarily assign $c_i$ except the 2 of the directions. Calculate the length for those two from the other ones. This special case can be solved in $O(n)$ time too. 