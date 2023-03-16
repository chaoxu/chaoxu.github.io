---
title: Algebraic framework for algorithms
tags: algorithm, algebra
---

A common algorithmic problem is to find if there is a way to do arbitrage. If we create a directed graph, such that each vertex is a currency, and each edge is weighted by the exchange rate of the currency, we want to find a paths from a vertex to itself, such that the product of the edges is greater than 1.

To solve the problem, take the $-\log$ of the edge weights, and the problem become find any negative cycle. Bellman-Ford does well for this problem. One might question what other kind of optimization problem we can use this kind of tricks so the original algorithm still works? Once we notice that $-\log$ is a order preserving homomorphism from $(\R^+,\cdot)$ to $(\R,+)$, then everything becomes clear. 

A group $(G,+)$ is a linearly ordered abelian group if it's abelian and for all $a,b,c\in G$, if $a\leq b$, then $ac\leq bc$. Find any proof of Bellman-Ford algorithm, and replace the use of reals with such a group, it would still work. Many weighted graph algorithms that only uses the addition property of the reals, all of them would work well with the underlaying set replaced by a linearly ordered abelian group.

In fact, if there is no negative edges, we don't even need to require it to be a group, a monoid structure is enough.

I found it interesting and like to know what is some algorithms in it's full generality. There is also a semiring framework for dynamic programming algorithms on graphs. Monoid, lattices have their use in parallel and distributed algorithms. It be great if there is a nice survey on the use of algebraic framework for describing algorithms.