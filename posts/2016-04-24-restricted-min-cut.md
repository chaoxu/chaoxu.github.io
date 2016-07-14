---
title: Minimum cuts with restrictions
tags: cut
---

There is a nice algorithm design technique for cuts: fix a small number of partial solutions, and guess all possible solutions built from the partial solution. Here are two demonstrations.

# $k$-size-cut

A cut $(X,\bar{X})$ such that both $|X|,|\bar{X}|\geq k$ is called a $k$-size-cut.

{Problem}

    Given a graph $G$, find a minimum $k$-size-cut.

The standard algorithm is based on finding all possible min-$ST$-cut, where $S,T$ are all possible disjoint sets of size $k$. The running time is $O(n^{2k})$ min-cut computations.

One can improve it by first fix an arbitrary set of $k$ vertices $Y$. Consider a min-$k$-size-cut $(X,\bar{X})$. Let $S'=X\cap Y$ and $T'=\bar{X}\cap Y$. We can try to find the min-cut of all possible $S$ and $T$ such that $S'\subset S$,$T'\subset T$ and $|S|=|T|=k$. Since we don't know what $S'$ and $T'$ are, we will try all $2^k$ possibilities. Here the $S'$ and $T'$ are the partial solutions. This gives us an algorithm with running time $O(2^k n^k)$ min-cut computations. The idea is also used in computing matroid connectivity. 

A further improvement depending on the fact that there are only $O(n^{k-1})$ cuts we try to avoid. We can enumerate all the cuts from smallest to largest, with a delay of a single application of Hao-Orlin algorithm [@HAO1994424]. The running time of Hao-Orlin algorithm is approximately a single maximum flow. One of the smallest ${n \choose k-1}+1$ cuts is the min-$k$-size-cut. The running time is $O(n^{k-1})$ Hao-Orlin computation [@Yeh2009]. 

It's interesting to wonder if the running time can be improved, especially for the case where $k=2$. Fix a set $S=\{s,t\}$ of size $2$. There are two cases, either the min-$2$-size-cut crosses $S$, then one of the the $3$ smallest $st$-cuts is our solution. The other case is $S$ is on one side of the min-$2$-size-cut, and we are interested in finding a cut so the side doesn't contain $S$ has at least $2$ vertices.

This prompt a interesting problem:

{Problem}
	
	Find the second smallest $st$-cut, if we already have a min-$st$-cut and it's corresponding flow(or some other useful information obtained through a push relabel flow computation).

Can we solve this in $O(m)$ time? What if we also know the min-$st$-cut is induced by $t$? 

# $2$-restricted-cut

A problem which appeared as a [question on cstheory](http://cstheory.stackexchange.com/questions/29474/finding-a-minimum-directed-cut-that-splits-a-weakly-connected-bipartite-graph-in).

{Problem}

    Given a graph $G$, find the minimum cut under the constraint that each side is connected and has at least $2$ vertices. (Assume it exists).

This is the $k$-restricted edge connectivity problem when $k=2$.

$\lambda_k(G)$, the $k$-restricted edge connectivity of $G$, defined as the smallest number of edges such that the removal result exactly $2$ connected component, each with at least $k$ vertices. The rest of the article describes the algorithm by Esfahanian and Hakimi [@ESFAHANIAN1988195].

$\lambda_2(G)$ can be found in $O(m^2)$ flow computations. The idea is to contract any two independent edges $e$ and $e'$ to $s$ and $t$, and then find a $st$-min-cut. The cut will give us the desired partition. 

It can be improved with the idea of fixing a partial solution. Consider a single edge $e$ that incident to a vertex with lowest degree, contract it to vertex $s$. Pick another edge $e'$ that not incident to $s$, we contract it to $t$. The min-cut between $s$ and $t$ reflects a $2$-restricted cut. If $e$ is on one side of the min-$2$-restricted cut, then this algorithm finds it in $O(m)$ flow computations by trying all possible $e'$.

Otherwise, $e$ is an edge crossing every min-$2$-restricted cut. Let $e=uv$ and and wlog $\deg(u)=\delta$, the min degree. We fix another partial solutions where $u$ and $v$ are on different side of the min-$2$-restricted cut. One can contract any edge incident to $u$ and any edge incident to $v$ and apply a flow computation. There are at most $\deg(u) \deg(v)\leq \delta n = O(m)$ flow computations.
