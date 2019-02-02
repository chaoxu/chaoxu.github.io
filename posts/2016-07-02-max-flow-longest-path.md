---
title: Maximum flow running time depend on longest path
tags: flow
---

Consider a directed graph with specified vertices $s$ and $t$. The graph has $m$ edges and $n$ vertices with length of the longest simple $st$-path to be $k$. Dinic's algorithm finds a maximum flow consists of blocking flow computations, where each takes $O(m\log n)$ time with dynamic trees [@Sleator1983; @Dinitz]. After each blocking flow, the distance between $s$ and $t$ in the residual graph increases by at least $1$. Therefore there are at most $k$ blocking flow computations.  Dinic's algorithm takes $O(km\log n)$ time to find a maximum $st$-flow.
