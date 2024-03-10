---
title: Densest subgraph variation
tags: combinatorial optimization
---

Let $G=(V,E)$ be a graph. Consider an edge weight function $w:E\to \R^+$ and a vertex cost function $c:V\to \R^+$.

We are interested in finding $S\subset V$, such that $w(E(S))-c(S)$ is maximized.

This is very close to the densest subgraph problem, as it is basically the Lagrangian relaxation of the problem.

This problem is equivalent to a min-$st$-cut computation on a suitable graph.
Indeed, minimizing $w(E(S))-c(S)$ is equivalent to minimizing $c(S) + \frac{1}{2} w(E(S,\bar{S})) + \frac{1}{2} \sum_{v\in \bar{S}} \deg(v)$.
This can be solved easily by modeling it as a min-$st$-cut.
