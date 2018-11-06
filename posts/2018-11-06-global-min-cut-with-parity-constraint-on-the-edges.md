---
title: Global min-cut with parity constraint on the edges
tags: algorithms, min-cut
---

In a discussion with [Patrick Lin](https://patrickl.in/), a nice problem was born.

Let $\delta(S)$ to be the set of edges with exactly one endpoint in $S$. $\delta^-(S)$ to be the set of edges with its head in $S$ and tail in $V\setminus S$.
Given a non-negative weighted graph, we define the cut function $f:2^V\to \R^+$ to be $f(S) = \sum_{e\in \delta(S)} w(e)$.
For directed graphs, $f(S) = \sum_{e\in \delta^-(S)} w(e)$.
$f(S)$ is called the value of the cut $S$.

Let $k$ be a constant, we consider the following problem.

{Problem}
    Give a graph and $k$ set of edges $F_1,\ldots,F_k$, $a_1,\ldots,a_k,b$. Find a cut $S$ satisfies that $|\delta(S)\cap F_i|\equiv a_i \pmod b$ for all $i$, and the value is minimized. 

We will try to reduce this problem to the following 

{Problem}(submodular minimization under congruence constraints)
    Given $T_1,\ldots,T_k$ and a submodular function $f$. Find a set $S$ such that $|T_i\cap S| \equiv a_i\pmod b_i$, and $f(S)$ is minimized. 


The above problem is known as submodular minimization under congruence constraints. It is known to be solvable in polynomial time under certain conditions on the $b_i$'s [@NageleSZ18]. 
We sketch the reductions.

# Undirected case

In the undirected case, we only consider when $b=2$. Patrick showed a the following construction.
Create a new graph $G'$ as follows. 
For each $uv$ in $E$, split it into edges $ux$, $xy$, $yv$, $w(ux)=w(yv)=\infty$, and $w(xy)=w(uv)$. 
Let $T_i$ contains the vertex $x$ and $y$ if $uv\in F_i$.  
We now solve the submodular minimization under congruence constraints problem on input $f$, which is the cut function for $G'$, and same $a_1,\ldots,a_k$ and $b_1,\ldots,b_k=2$. 

# Directed case

In the directed case, a similar approach works. But now, instead of $\mod 2$, we can do $\mod b$ for any $b$.
We consider the same approach.  
$(u,v) \in E$ split into $(u,x_1),\ldots,(x_b,v)$ and $w(u,x_1)=w(u,v)$, $w(x_i,x_{i+1})=\infty$, $w(x_b,v)=\infty$.
Now, let $T_i$ contain vertices $x_1,\ldots,x_b$ of $uv\in F_i$.