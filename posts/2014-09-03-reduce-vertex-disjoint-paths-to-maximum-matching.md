---
title: Reduction between vertex disjoint paths and maximum matching 
---

# Reduce maximum bipartite matching to maximum vertex disjoint paths

Given a bipartite graph $G=(S,T,E)$, add a vertex $s$ and $t$, such that $s$ connect to all vertices in $S$ and $t$ connect to all vertices in $T$. For each vertex disjoint path from $s$ to $t$, alternately remove the edges, the remaining is a maximum matching.

# Reduce maximum vertex disjoint paths to two maximum bipartite matching
This is a slight modification of the reduction in Sankowski's paper on bipartite matching[@Sankowski20094480]. It is also a solution to the first problem of the [algorithmic trends hw 1](http://duch.mimuw.edu.pl/~sank/wordpress/wp-content/uploads/2014/03/homework1.pdf).

Notice this consist of two parts, find the value of a maximum matching, and then find a perfect matching on two modified graphs.

Let $G=(V,E)$ to be a directed graph with specified nodes $s$ and $t$(for simplicity, assume $st\not\in E$). Define $G_k=(V^- \cup S_k ,V^+ \cup T_k,E')$ be a bipartite graph of $O(m+k^2)=O(n^2)$ edges and $O(n)$ vertices, where $V^- = \{v^-|v\in V\}$, $V^+ = \{v^+|v\in V\}$, $S_k=\{s_1,\ldots,s_k\}$ and $T_k=\{t_1,\ldots,t_k\}$. Notice it is similar to the common vertex split in max flow, where $v$ get's split into $v^-$ and $v^+$.

$E'$ consist of following edges:

1. $u^-v^+\in E'$ if $uv\in E$ and $\{u,v\}\cap \{s,t\} = \emptyset$.
2. $u^-u^+\in E'$ for all $u\in V$.
3. $s_iv^+\in E'$ for all $1\leq i\leq k$ and $sv\in E$.
4. $v^-t_i\in E'$ for all $1\leq i\leq k$ and $vt\in E$.

We can find maximum number of vertex disjoint paths from $s$ to $t$ by solving a maximum matching problem on a bipartite graph $G_n$. Any matching that covers both $V^-$ and $V^+$ would induce some set of vertex disjoint paths by contracting $v^-v^+$ into $v$. It's clear no such path can start and end in $s_i$ and $s_j$ for some $i$ and $j$, similarly no path can start at $t_i$ and end at $t_j$. Thus we can contract $S_n$ into $s$ and $T_n$ into $t$ and remove paths doesn't involves any $s$ and $t$ to get the set of disjoint paths. The number of such disjoint paths can be readily read from the size of the maximum matching, because the number equals to the number of $s_i$ and $t_j$'s matched. Of course, an algorithm for maximum matching might not return a maximum matching that covers both $V^-$ and $V^+$, but we use a value to build such an graph.

Let the size of the maximum matching be $n+l$, then there exist $l$ maximum vertex disjoint paths. Build graph $G_l$ and find a maximum matching in $G_l$. This time the matching would be a perfect matching, and use the same idea as above gives the $l$ vertex disjoint paths between $s$ and $t$.

# Reference
