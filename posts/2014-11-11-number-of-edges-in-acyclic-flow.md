---
title: Number of edges in acyclic flow
tags: Flow
---

The graph in this post are simple graphs with unit edge capacity.

{Theorem}(Main Theorem)
	Let $G=(V,E)$ be a graph with $n$ vertices and $m$ edges, then an acyclic integer flow of value $v$ saturates $O(n\sqrt{v})$ edges.

Everything here is in the Karger and Levine's paper[@Karger1998], I'm writing this down for my own sake.

{Theorem}
	Let $d$ to be the distance of the shortest path between $s$ and $t$. Let $v$ to be the value of a $s$-$t$-flow, then $v\leq 2(n/d)^2$ and $v\leq 2 m/d$.

{Proof}
	Let $V_i$ to be the set of vertices with distance exactly $i$ from $s$. The maximum flow is clearly bounded by $|V_i||V_{i+1}|$, as all flows has to go in between the two levels and the maximum number of edges in between happens when it is a complete bipartite graph. Notice in order to maximize this value, we have $|V_i|=n/d$. Hence $v\leq 2(n/d)^2$. Similarly, we can also distribute the number of edges in between every two partitions, and get $v\leq 2m/d$.

{Theorem}
	If we take shortest augmenting path successively in the residual graph to find a flow of value $v$, then we saturate $O(n\sqrt{v})$ edges.

{Proof}
	$v\leq 2(n/d)^2$, thus $d\leq n\sqrt{2}/\sqrt{v}$. If we take the shortest path successively, we have the number of edges we use in each augmenting path is

	\[
		\sum_{i=1}^v n\sqrt{2}/\sqrt{i} = O(n\sqrt{v})
	\]

For the above theorem, one might ask what happens if we use $v\leq 2m/d$ inequality instead. Then we get the sum of the length of all the augmenting paths we ever route is $O(m\log m)$, pretty neat. 

{Proof}(Main theorem)
	Let $f$ be an acyclic flow of value $v$ in $G$. Remove all edges outside $f$, and let the remaining graph be $G'$. Consider use shortest augmenting path algorithm sending a flow of value $v$ in $G'$. Notice this would produce $f$. Thus this shows $f$ has $O(n\sqrt{v})$ edges.

{Corollary}
    Let $G$ be a simple graph on $n$ vertices. There exist $\lambda(s,t)$ $st$-edge disjoint paths, such that the $k$ shortest paths has total of $O(\sqrt{k}n)$ edges for all $k\leq \lambda(s,t)$.

{Proof}
    There exist $\lambda(s,t)$ $st$-edge disjoint paths with total length $O(\sqrt{\lambda(s,t)}n)$. The average length is $O(\frac{n}{\sqrt{\lambda(s,t)}})$. The $k$ shortest paths has total length $O(k\frac{n}{\sqrt{\lambda(s,t)}})=O(k\frac{n}{\sqrt{k}}) = O(\sqrt{k}n)$.  



