---
title: Open Problems
---

Here are some problems that are either open to the best of my knowledge, or asked by me but unable to answer. If you know a solution, or can offer me more reference on the material, please comment. I appreciate it greatly. :)

# Probably NP-hard

{Problem}

    Given a directed graph $G$ and a vertex $v$, what's the minimum number of vertices has to be removed such that there exist a vertex $u$ such that there is no path from $v$ to $u$ or $u$ to $v$.

{Problem}

    [This one](http://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves).

{Problem}
    Given $a_1,\ldots,a_n$ and $b_1,\ldots,b_n$. Does there exist a permutation such that
    \[ 
    \sum_{i=1}^n a_{\pi(i)}a_{\pi(i+1)} - b_{\pi(i)}b_{\pi(i+1)} < 0
    \], where we define $\pi(n+1)=\pi(1)$.
    Is this problem NP-hard?

Note this is basically traveling salesman problem such that the distance between two vertices is the difference of two symmetric product matrix, where a matrix $M$ is a product matrix of vector $x$ and $y$ if $M_{ij}=x_iy_j$.

{Problem}

    There are $n$ people sitting on a circle of $n$ seats. Out of the $n$ people, there are $k$ couples($2k$ people). You can swap any two people. Find an algorithm that swap the least amount of times such that all the couples are sitting with his/her partner.

# Easy problem

It comes from the game [FTL: Faster Than Light](http://www.ftlgame.com/). The beam weapons attacks a entire line segment. The damage is proportional to the total number of rooms hit by the line segment. 

{Problem}
    Input is the length $l$ and polygons on the plane. Find a segment of length $l$ that intersects the most number of polygons.

Variations: Allow weights, can we do it faster if polygons are all axis-aligned rectangles and pairwise disjoint?

This problem doesn't seem hard. Common computational geometry technique should handle it well. 

{Problem}
    Hypergraph minimum cut in near linear time?

# Connectivity

{Problem}
    Given a simple unit edge capacity undirected graph $G=(V,E)$ with a subset of vertices $T$ with unit capacity, and remaining vertices with infinite capacity. Can one find a $st$-maximum flow in deterministic $O(n^{2.5})$ time?

{Problem}
    Let $G$ be a directed graph with a source vertex $s$, how fast can one find $\min_{t\in T} \lambda(s,t)$? 

{Problem}
    [Maximum local edge connectivity](http://cstheory.stackexchange.com/questions/25531/maximum-local-edge-connectivity)

{Problem}
    There are disjoint sets of red vertices $R$ and blue vertices $B$. Let $V=R\cup B$. $|R|,|B|$ are odd.

    Consider any submodular function $f:2^V\to \mathbb{R}$, such that $f(V)\leq f(Y)$ for all $Y$, where $|Y\cap R|$ and $|Y\cap B|$ are odd.

    Show that there exist $r\in R$ and $b\in B$, s.t. for any set $S$ where $r,b\in S$, we have $f(S)\geq f(V)$.
    
# Other problems

{Problem}
    How fast can we [improve the pseudo-polytime algorithm for subset sum](http://cstheory.stackexchange.com/questions/21533/faster-pseudo-polynomial-time-algorithm-for-subset-sum)? 

Interesting to think about but probably can't be improved easily.

