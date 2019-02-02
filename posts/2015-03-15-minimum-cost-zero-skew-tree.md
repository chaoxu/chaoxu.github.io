---
title: Minimum cost zero skew tree
tags: series-parallel, tree
---

{Problem}
    Let $T$ be a rooted tree with real costs $c(e)$ and length lower bounds $\ell(e)$ on each edge $e$. We are interested in compute a function $f$, such that $f(e)\geq \ell(e)$, for any root to leaf path $P$, $\sum_{e\in P} f(e)=t$ and $\sum_{e\in E} c(e)f(e)$ is minimized. 

This is the [zero skew tree problem](http://theory.cs.uni-bonn.de/info5/steinerkompendium/node27.html) where the tree is fixed.

Here we show how this problem can be solved in $O(n\log n)$ time by reducing it to minimum cost flow on 2-terminal series parallel graph. [@Booth1993416]

$C(v)$ denote the set of all the children of $v$. 

For $vu\in E(T)$, and $u\in C(v)$, the graph $P(vu)$ is the parallel connection of one single edge with cost $c(vu)$, lower bound $\ell(vu)$, infinite upper bound and a series-parallel graph $S(u)$.
Let the graph $S(v)$ for $v\in V(T)$ to be the series connection of $P(vu)$ for all $u\in C(v)$(the connection can be ordered arbitrarily).

The base case is when $u$ does not have any children, and $P(vu)$ is just a single edge with two terminals. 

Let $G=S(r)$, where $r$ is the root of $T$. There is a bijection between the edges in $T$ and the edges in $G$. 

Finding the minimum cost flow of value $t$ in $G$ with the two terminals gives us the desired solution by going back to the original edge using the bijection.

