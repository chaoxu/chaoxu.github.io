---
title: The high-degree low-degree technique and arboricity 
tags: algorithm, graph
---

In this piece we demonstrate the high-degree low-degree technique in graphs. Often, we obtain running times that depends on the individual degrees of the vertices. If the graph has only low degree vertices, then a faster algorithm exists. For graph with only large degrees, then it is dense, and can often be handled in another way.

We will also use the information of [arboricity](https://en.wikipedia.org/wiki/Arboricity). 
Mainly, there are a few useful statements. 

{Theorem}
    For a graph $G=(V,E)$ with arboricity $\alpha$, we have 
    \[
        \sum_{uv\in E} \min(\deg(u),\deg(v)) \leq 2\alpha m
    \]

{Theorem}
    If the arboricity of a graph is $\alpha$, then there exists a induced subgraph with minimum degree at least $\alpha$.

Often, using the arboricity, we can obtain the same complexity algorithm without high-degree low-degree technique. Note the arboricity is $O(\sqrt{m})$. The application of arboricity are from [@ChibaN85].

Some of the algorithms described can be speedup by using matrix multiplication, or faster combinatorial boolean matrix multiplication. We avoid them for simplicity of exposition. 

# Dominating set with few edges

The set cover problem, given $\mathcal{S} = \set{S_1,\ldots,S_n}$ are $n$ set contains a total of $m$ elements. $U=\bigcup_{S\in \mathcal{S}} S$ is the universe, with size $u$.

{Theorem}
    There is a probability distribution $D$ over $\mathcal{S}$, such that for each $u$, the probability a random set $S$ covers $u$ is at least $\e$. There exists a set cover of $\ceil{\frac{\log u}{\e}}$.

{Proof}
   There exists a set that covers at least $\e |U'|$ for any $U' \subset U$. Therefore each greedy iteration decrease the size of uncovered universe by an $\e$ fraction. So there can be at most $t$ iterations, where $(1-\e)^t<1$. One can show $\ceil{\frac{\log u}{\e}}$ suffices. 

{Theorem}
    There is a dominating set incident to $O(n\sqrt{n\log n})$ edges.

{Proof}
    Fix a $\delta$. We repeatedly removing vertices with degree no more than $\delta$ from the graph, and add it into a set $D$. The total degree of $D$ is at most $n\delta$. Now the remaining vertices has degree at least $\delta$. 
    Using the set cover theorem, and let the distribution to be the uniform distribution. If all elements are covered at least by $\e$ fraction of the set, then we obtain a set cover of size $O(\frac{\log u}{\e})$. Now, let the sets $N(v)$ for each $v$. Since degree is bounded by at most $n$, we can obtain a dominating set of size $O(\frac{n\log n}{e})$. We set $\e=\delta/n$. Since the degree of each vertex is at least $\delta$, then there is a covering of $O(\frac{n^2\log n}{\delta})$. Add the vertices induces this set cover to $D$.
    $D$ is a dominating set, and its size is $O(n\delta +\frac{n^2\log n}{\delta})$, set $\delta=\sqrt{n\log n}$ and we obtain the desired result.

One can show the above result is almost optimal, as there exists graphs where every dominating set incidents $\Omega(n^{3/2})$ edges. The same bound holds for weakly connected dominating set, that is a dominating set $D$ such that the edges incident to $D$ forms a connected graph. The stronger modification of this result was used in deciding the $4$-connectivity of a matroid [@Rajan87]. 

# Finding small subgraphs

## Finding triangle

A _triangle_ is $3$ vertices pairwise adjacent to each other, another name for $K_3$.

{Theorem}
    There is a $O(m\Delta)$ time algorithm to decide if the graph has a triangle, where $\Delta$ is the maximum degree.

{Proof}
    Indeed, for each vertex $v$, we consider its neighbors, see if any is adjacent to each other. We then delete $v$. The algorithm takes $O(\sum_{v} \deg^2(v)) = O(m\Delta)$ time. 

{Theorem}
    There is a $O(n^3)$ time algorithm to decide if the graph has a triangle.

{Proof}
    The naive algorithm, for each $3$ vertices, we decide if it forms a triangle. 

{Theorem}
    There is a $O(m^{3/2})$ time algorithm to decide if the graph has a triangle. 

{Proof}
    Let $t$ be a parameter we will find later. Apply the above algorithm by picking the vertex with the smallest degree, until the next vertex has degree at least $t$. It will use at most $O(mt)$ time. Now, for the remaining graph, it is clear the maximum degree is at least $t$. Note, there can be at most $n/t$ vertices. We use the $O(n^3)$ time algorithm. The final running time is $O(mt+(m/t)^3)$. Set $t=\sqrt{m}$ and we are done. 

{Proof}(Alternative)
    We modify the algorithm a little. For each vertex $v$, we consider its neighbor $u$, and check if $u$ has a neighbor that is in $v$. Then we delete $v$, and move on to next vertex. The running time become $\sum_{v\in V} (\deg(v)+\sum_{u\in N(v)} \deg(u))$. Now, assume we pick vertices by the _largest_ to _smallest_ in term of degrees. We rearrange the sum and obtain $\sum_{v\in V}  (\deg(v)+\sum_{u\in N(v)} \deg(u)) = \sum_{v\in V} \deg(v) + 2 \sum_{uv\in E} \min(\deg(u),\deg(v)) = O(\alpha m)$. 
    But $\alpha\leq \sqrt{m}$, therefore we have the running time $O(m^{3/2})$.

## Finding $K_{2,\ell}$ in bipartite graphs

This section we use technique that follows from [@AlonYZ97], which solved the special case $K_{2,2}$.
For two vertices $u,v\in A$, decide if there is a intersection of the neighborhood of size at least $\ell$. 
The claim is using the same argument as the triangle case with arboricity. Of course one need to be careful with designing the algorithm. However, the following would not be difficult to obtain.

{Theorem}([@ChibaN85])
    One can check if there exists a $K_{2,\ell}$ in the bipartite graph $G=(A,B,E)$ in $O(\alpha m)$ time.

Again, we directly obtain $O(m^{3/2})$ running time. However, we show something better using stronger theorems. 

{Theorem}
    One can check if there exists a $K_{2,\ell}$ in the bipartite graph $G=(A,B,E)$ in $O(\ell n^2)$ time.

Now, we combine the two algorithms. It requires a theorem in extremal graph theory. 

{Theorem}($K_{2,\ell}$-free theorem [@Furedi96])
    There exists a constant $c$, such that each $n$ vertex graph with $c n^{3/2} \ell^{1/2}$ edges contains a $K_{2,\ell}$.

{Theorem}
    There is a $O(\ell^{1/3}m^{4/3})$ time algorithm to find a $K_{2,\ell}$ in the graph.

{Proof}
    If the arboricity is $t$. We use the first algorithm and we get running time $O(t m)$. Otherwise, we know there is a subgraph with minimum degree at least $t$. The subgraph can be found by repeatedly deleting vertices of minimum degree. The subgraph $G'$ with the previous property has $n'\leq n$ vertices and $m'\leq n't$ edges. One can see $n'\leq m'/t\leq m/t$. If $cn'^{3/2}\ell^{1/2}\leq m' \leq n't$, then we know there exists a $K_{2,\ell}$ in $G'$ by the previous theorem, and we can apply the $O(\ell n^2)$ time algorithm in the subgraph to find the $K_{2,\ell}$. The total running time is therefore $O(tm + \ell n'^2) = O(tm+\ell (m/t)^2)$.
    We set $t=c^{3/2} (\ell m)^{1/3}$. One can check after lot of algebra, it make sure the condition $cn'^{3/2}\ell^{1/2}\leq n't$ is satisfied. The algorithm takes $O(\ell^{1/3}m^{4/3})$ time. 