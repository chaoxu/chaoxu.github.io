---
title: There exist a path of length $\lceil d(G)\rceil$ for every graph $G$
tags: graph theory
---
# Unweighted Graph
$G$ is a simple graph, then $d(G) = \frac{2e(G)}{|G|}$ be the average degree of a simple graph.

{Lemma}

    If $G$ is a connected graph, then it contain a path of length $\min(2\delta(G), |G|-1)$, where $\delta(G)$ is the minimum degree of $G$. (exercise 1.7. in Graph Theory by Diestel)

{Lemma}

    Every graph $G$ has a component $H$, such that $d(H)\geq d(G)$.

{Proof}

    Fact: If $x_i,y_i>0$ and $\frac{x_i}{y_i} < t$ for all $1\leq i\leq k$, then $\frac{\sum_{i=1}^k x_i}{\sum_{i=1}^k y_i} < t$. Assume the lemma is false, then consider all it's components $H_1,\ldots,H_k$, $d(H_i) = \frac{2e(H_i)}{|H_i|} < d(G)$, then $d(G) = \frac{\sum_{i=1}^k 2e(H_i)}{\sum_{i=1}^k |H_i|} < d(G)$, a contradiction.
 
{Theorem}

    A simple graph $G$ must contain a path of length at least $d(G)$.

{Proof}

    Proof by induction. It is true for graph with 1 vertex. Assume it is true for all graphs with $k$ vertices, $k < n$. Consider graph $G$ of $n$ vertices. If the graph has more than 1 component, then we use [Lemma 2] and show there exist a subgraph $H$ with strictly smaller number of vertices, such that $d(H)\geq d(G)$, and just apply the induction hypothesis.

    Otherwise, the graph is connected. If there exist a vertex $v$ with degree at most $\frac{1}{2}d(G)$, we can remove it, and $d(G-v) \geq d(G)$, then by inductive hypothesis, in $d(G-v)$ there will be a path of length at least $d(G)$. If there is no such vertex. then we must have $\delta(G) > \frac{1}{2} d(G)$. By [Lemma 1], we have it has a path of length $\min(2 \delta(G) ,|G|-1)$. $2\delta(G) \geq d(G)$ and $d(G)\leq |G|-1$, thus it contain a path of length at least $d(G)$.

# Weighted Graph
Can we generalize this problem to weighted graphs?
$G=(V,E)$, $(G,w)$ is a weighted graph, where $w: E\to \mathbb{R}$. Define the average weighted degree of graph $G$ to be 
\[
d_w(G) = \frac{2\sum_{e\in E} w(e)}{|G|},
\]
and the minimum weighted degree
\[
\delta_w(G) = \min_{e\in E} \{w(e)\}.
\]
If $P$ is a path, then the weight of the path is $W(P) = \sum_{e\in P} w(e)$. What can we say about the path with maximum weight? 
It is easy to prove that there is a path of weight at least $\delta_w(G)$. 

{Theorem}

    For a weighted graph $(G,w)$, there exist a path of weight at least $\delta_w(G)$. 

{Proof}

    Consider the longest path $v_1,\ldots,v_n$. Claim: $w(\{v_i,v_n\})\leq w(\{v_i,v_{i+1}\})$ for all $i$. Assume not, then the path $v_1,\ldots,v_{i-1},v_i,v_n,v_{n-1}\ldots,v_{i+1}$ would be heavier, a contradiction. Therefore we have
    \[
    \delta_w(G) \leq \sum_{\{v_i,v_n\}\in E} w(\{v_i,v_n\}) \leq \sum_{i=1}^{n-1} w(\{v_i,v_{i+1}\})  = W(v_1\ldots v_{n})
    \]

However, we want something stronger, say instead of $\delta_w(G)$, can it be $d_w(G)$? I have a proof but it uses a difficult lemma.

{Definition}

    A *perfect path double cover* (PPDC) for graph $G$ is a set of paths, such that every edge is covered exactly two times, and every vertex is the end point of exactly two of the paths. 

Note a PPDC for a graph with $n$ vertices is a set of $n$ paths.

{Lemma}

    Every simple graph has a perfect path double cover.

The lemma is proven by [Hao Li in 1990](http://onlinelibrary.wiley.com/doi/10.1002/jgt.3190140604/abstract). 

Now we conclude with the theorem that eats up all the above special cases.

{Theorem}

    For a weighted graph $(G,w)$, there exist a path of length at least $d_w(G)$. 

{Proof}

    Consider a PPDC of $G$ with $n$ paths $p_1,\ldots,p_n$. $\sum_{i=1}^n l(p_i) = nd_w(G)$, by pigeonhole principle, at least one of the path has length at least $d_w(G)$.

I wonder if there is a simpler proof of the fact, so no high machinery like PPDC are involved. 

All these results are of course, already known. Frieze, McDiarmid and Reed have solved it in 1992. It is a 10 page proof, and thanks to Charalampos who provided it in the comment. The proof for PPDC only used 2 pages, so this would be an easier proof. 

How do we find such a path? PPDC can be computed in $O(m\Delta(G))$ time, so we actually have a polynomial time algorithm for this.

This give us a fun problem.

{Problem}

    Prove that for every weighted multigraph with no self loop, if it has $M$ edges and the largest set pairwise non-parallel edges has $m$ edges, then there exist a path of weight at least $\frac{d_w(G)}{M-m+1}$.
