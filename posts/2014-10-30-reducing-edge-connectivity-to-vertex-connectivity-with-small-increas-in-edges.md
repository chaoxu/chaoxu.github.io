---
title: Reducing edge connectivity to vertex connectivity with small increase in edges
tags: connectivity
---

{Problem}
	Let $G=(V,E)$ be a graph, we are interested in finding a graph $G'=(V',E')$, such that $G$ is a minor of $G'$. We want a partition $P$ of $V'$ and a bijection $f:P\to V$, such that for any $s\in S$, $t\in T$ and $S,T\in P$, we have $\kappa_{G'}(s,t) = \lambda_{G}(f(S),f(T))$.

Although there is rarely a reason to reduce edge connectivity computation to vertex connectivity computation, because vertex connectivity is much harder in almost every way. It is still a interesting exercise.

The naive method: For each vertex $v$, expand it into a clique of size $deg(v)$, and find a bijective label between new vertex in the clique with the edges incident to $v$. Connect all the vertices with the same label. Thus in the worst case, we can loosely bound the edges in $G'$ to be $\sum_{v} deg^2(v)\leq \sum_{v} (m/n)^2 = m^2/n = O(nm)$.

There is another way with the edge blow up to be $O(nm)$ [@Galil:1991:REC:122413.122416].

However, we want to blow up the number of edges by at most $O(m)$.

{Definition}
	A $n$-connector is a graph with two disjoint set of vertices $I$ (inputs) and $O$ (outputs) such that $|I|=|O|=n$, and for any bijection $f:I\to O$, there exist vertex disjoint paths connecting $v$ and $f(v)$ simultaneously.

It is known a $n$-connector exists with $O(n)$ edges, because there is a even stronger notion of $n$-nonblocking graph[@BLTJ:BLTJ2972]. 

$n$-connector seems to be what we need here, but it forces us to have input and output vertices. Thus we can define the following.

{Definition}
	A $n$-symmetric-connector is a graph with a set of $n$ vertices $T$, such that for any $f$ a bijection between two disjoint subsets of $T$, there exist vertex disjoint paths connecting $v$ and $f(v)$ simultaneously.

Let $G$ be a $n$-connected with inputs $I$ and outputs $O$, we construct a $n$-symmetric connector by make a copy of $O$ as $O'$, maintaining the edges of the copies too. Then we pick $f$ any bijection between $I$ and $O$, and contract $v$ and $f(v)$ into one vertex. The contracted vertex will be the set $T$, and the new graph is a $n$-symmetric-connector.

