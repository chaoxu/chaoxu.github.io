---
title: Reconstructing edge-disjoint paths, a tighter analysis
tags: connectivity
---

This is a small improvement of the analysis of [@Conforti2003], this assume you already read that paper.

{Problem}
	Given a simple undirected graph of $n$ vertices and $m$ edges, construct a data structure such that we can query two vertices $u$ and $v$, and it returns the maximum edge disjoint paths between $u$ and $v$.

We let $MF(n,m)$ to be the time to compute $st$-maximum flow on a undirected unit capacity graph.

The current state of art in the article is:

1. Proprocessing time $O(nMF(n,m))$.
2. $O(n^3)$ space.
3. $O(\alpha(n)\lambda(u,v)n)$ query time.

We can improve this by having a tighter analysis.

First, if $f$ and $g$ be $k$-edge disjoint path from $x$ to $y$ and $y$ to $z$ respectively. Let $m$ be the number of edges in $f\cup g$, then there exist an algorithm to output $k$-edge disjoint path from $x$ to $z$ in $O(m)$ time. One can see this by update section $2$ of their paper by using a stable matching algorithm without the dummy edges, because this is just stable matching with incomplete lists[@iwama]. 

Second, acyclic flow of value $f$ uses only $O(\sqrt{f}n)$ edges(Theorem 3.1 in [@Karger1998]), therefore we do not have to store any non-acyclic flows. This also speed up all computations because the number of edges are smaller.

Third, we can use $O(n^{2.5}\log n)$ space and get a query time of $O(\sqrt{\lambda(u,v)}n)$. In fact there is a time space trade off [@Alon87optimalpreprocessing].

Thus finally, we get

1. Proprocessing time $O(nMF(n,m))$.
2. $O(n^{2.5}\log n)$ space.
3. $O(\sqrt{\lambda(u,v)}n)$ query time.

# References