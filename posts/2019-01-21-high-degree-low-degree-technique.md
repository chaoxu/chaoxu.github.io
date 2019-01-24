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

## Finding a triangle

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
    Because $\alpha\leq \sqrt{m}$, we have the running time $O(m^{3/2})$.


## A motivating problem

Let $S_1,\ldots,S_n$ be sets with total of $m$ elements. How quickly can we find two distinct $i$ and $j$ such that $|S_i\cap S_j|\geq \ell$? This problem can be shown to be equivalent to finding a colored $K_{2,\ell}$ in a bipartite graph. That is, for input bipartite graph $G=(A,B,E)$. Find a $K_{2,\ell}$ where the side of two vertices has to be in $A$. 

## Finding a $C_4$ in bipartite graphs

This section we use technique that follows from [@AlonYZ97]. Although we are into finding $C_4$, but some theorems are more general for $K_{2,\ell}$, so we will state them too. Note finding a colored $K_{2,2}$ and finding a $K_{2,2}$ is the same problem due to symmetry. 

Let $v_1,\ldots,v_n$ be an ordering such that $\deg(v_i)\geq \deg(v_j)$. There exists an algorithm that finds an ordering of vertices $v_1,\ldots,v_n$, and returns $N_i(v_i)\cap N_i(v_j)$ for each $i$ and $j>i$. Here $N_i(v)$ is the set of neighbors of $v$ in $G[\set{v_i,\ldots,v_n}]$.
Here we show an algorithm solves the above problem when the arboricity is small. 

The algorithm is as follows [@ChibaN85]. Take $v$, we consider each neighbor $u$. Maintain a set $S_w$ for each vertex $w$ distance $2$ from $v$. Add $u$ into each of $u$'s neighbor in $w$. $S_w$ would give us information of $N(v)\cap N(w)$. We delete $v$ and keep going. It is known the algorithm takes $O(\alpha(G)m)$ time. This allows us to compute $C_4$ in the same time. Hence we directly obtain $O(m^{3/2})$ running time. However, we show something better is possible if we are not interested in finding all $C_4$, but find any $C_4$. We also need the following theorem.

{Theorem}
    One can check if there exists a $K_{2,\ell}$ in the bipartite graph $G=(A,B,E)$ in $O(\ell n^2)$ time.

Now, we combine the two algorithms. It requires a theorem in extremal graph theory can be found in [@Furedi96]. 

{Theorem}($K_{2,\ell}$-free theorem)
    There exists a constant $c$, such that each $n$ vertex graph with $c n^{3/2} \ell^{1/2}$ edges contains a $K_{2,\ell}$.

{Theorem}
    There is a $O(m^{4/3})$ time algorithm to find a $C_4$ in the graph.

{Proof}
    If the arboricity is $t$. We use the first algorithm and we get running time $O(t m)$. Otherwise, we know there is a subgraph with minimum degree at least $t$. The subgraph can be found by repeatedly deleting vertices of minimum degree. The subgraph $G'$ with the previous property has $n'\leq n$ vertices and $m'\leq n't$ edges. One can see $n'\leq m'/t\leq m/t$. If $cn'^{3/2}\leq m' \leq n't$, then we know there exists a $C_4$ in $G'$ by the previous theorem, and we can apply the $O(n^2)$ time algorithm in the subgraph to find the $C_4$. The total running time is therefore $O(tm + n'^2) = O(tm+(m/t)^2)$.
    We set $t=c^{3/2} m^{1/3}$. One can check after lot of algebra, it make sure the condition $cn'^{3/2}\leq n't$ is satisfied. The algorithm takes $O(m^{4/3})$ time. 

## Finding a colored $K_{2,3}$ in bipartite graphs

For finding $K_{2,\ell}$, the low arboricity algorithm for $C_4$ works here. The $O(\alpha(G)m)$ algorithm is still fine. It's not hard to generalize and show a $O(\ell^{1/3}m^{4/3})$ running time algorithm.

However, in order to solve the motivating problem. We need a colored $K_{2,\ell}$.

Let's consider a $K_{2,\ell}$ in $G$, and consider the first indexed vertex $v$. If $v\in A$, then we are done, as the algorithm will find it. If $v\in B$, then we will solve the problem in another way, which gives us some time improvement when $\ell=3$. 

For each $v_i$ in $B$, we consider $v_j\in B$ that has distance $2$ from $v_i$ and $j>i$. We consider the set of vertices $S_{i,j} = N_i(v_i)\cap N_i(v_j)$. 
If there are two sets $S_{i,j}$ and $S_{a,b}$ has intersection size at least $2$, then we claim there exists a $K_{2,3}$ in $G$. Now, this becomes finding a $C_4$ in the input sets. The total size of the input sets are $O(\alpha(G)m)$. Hence we can use $O((\alpha(G)m)^{4/3})$ time to find a $C_4$. Hence this implies a $O((\alpha(G)m)^{4/3})$ time algorithm to find a $K_{2,3}$. 

Using the idea for finding $C_4$, we can mix the $((\alpha(G)m)^{4/3})$ time algorithm and the $O(n^2)$ time algorithm. Working out the algebra shows the following theorem.

{Theorem}
    There is a $O(m^{28/15})$ time algorithm for finding a colored $K_{2,3}$.
