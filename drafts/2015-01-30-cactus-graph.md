---
title: Cactus graph
---

Many algorithm on paths gets generalized to algorithm on cycles and trees without lose of efficiency. Because both generalization maintain some of the best properties about a path:

1. Cycle: all edges degree at most $2$, so the amount of choices are extremely limited.

2. Tree: There is a unique path between any two vertices. 

A common generalization of the two is a cactus graphs.

A graph is a cactus graph if any only if every edge is contained in at most $1$ cycle. 

Thus, it make sense to see if fast algorithms can be generalized to cactus graphs.

We are mostly interested in results that runs in linear or almost linear time, and are not directly from the fact that cactus has treewidth 1, or cactus is an outerplanar graph. 