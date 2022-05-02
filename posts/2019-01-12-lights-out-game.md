---
title: Lights out game on a grid
tags: algorithm, algebra
---

::: Problem
  Let $G$ be a graph, let $A$ be the adjacency matrix of $G$. Solve the equation $Ax=b$ in $\F_2$.
:::

The problem is equivalent to the [lights out game](https://en.wikipedia.org/wiki/Lights_Out_%28game%29). Each vertex has state $0$ or $1$. Activate a vertex flips the state of itself and all its neighbors. Find a set of activations that turns all state into $0$.

Originally I thought this problem can be solved in $O(n^{\omega/2})$ when $G$ is planar graph on $n$ vertices by [nested dissection](https://en.wikipedia.org/wiki/Nested_dissection). However, only recently I found out the matrix must be non-singular [@alon_matrix_2013]. Therefore, nested dissection does not apply. In particular, even grid graphs can have singular adjacency matrix. For example, a $2\times 3$ grid. 

If the graph is a $n\times n$ grid, then solving the linear system takes $O(n^6)$ tmie. Recently, I saw an $O(n^3)$ time solution. The solution by Zheng Wang and can be seen [here](https://zhuanlan.zhihu.com/p/53646257)(in Chinese). Here I gave my interpertation of the algorithm. 

# The algorithm

Given a $n\times n$ grid graph. Let $v_{i,j}$ be the node on the $i$th row and $j$th column. Let $b_{i,j}$ be the state of the vertex $v_{i,j}$ in the input. Each state is in $\F_2$. If we activates a node, the state of the node and its neighbors change by $1$. The set of activated node is called the activation set. 

We are interested in finding an activation set $S$, such the state of all nodes after activate $S$ is $0$.

Let $S$ be a activation set, and $S_1\subseteq S$ to be the activation set of the first row. 

::: Theorem
  $S_1$ uniquely determines $S$. Moreover, One can compute $S$ from $S_1$ in $O(n^2)$ time.
:::
::: Proof
  Indeed, consider apply activation to the nodes in $S_1$. Consider any vertex in row $1$. If it $0$, then the remaining neighbor (on the second row) cannot be activated. If it is $1$, then the remaining neighbor has to be activated. 
:::

Naively, by the above theorem, we can obtain a $O(2^n n^2)$ time algorithm by trying all possible $S_1$ until we find one that works. 

Wang realized the proof of the theorem actually allows us to solve for $S_1$ directly. In the beginning, we don't know what $S_1$ should be, but we do know its relations with $S$. The high level idea is a $3$ step process.

 1. Express the activation state of $v_{i,j}$ as a linear combination of the activation state of $v_{1,j}$ for all $i,j$. This allows us to express $S$ parametrized by $S_1$. 
 2. Solve for a $S_1$ by setting up a system of linear equations with the activation states of the last row. 
 3. Using $S_1$ to obtain $S$.

Formally, we create formal variables $Z=\set{z_1,\ldots,z_n}$. Here $z_i$ is an indicator variable that represents if $v_{1,i}\in S_1$. Let $D[i,j]$ be a linear combination of $Z$, such that it evaluates to $1$ if $v_{i,j}\in S$ and $0$ otherwise.

We compute $D[i,j]$ through dynamic programming. First, set $D[1,j] = z_j$ for all $j$. For each $i$ and $j$, $D[i+1,j] = 1 + D[i,j-1]+D[i,j]+D[i,j+1]+D[i-1,j]+b_{i,j}$. We can compute $D[i,j]$ in $O(n)$ time. Computing all $D[i,j]$ for $i\geq 2$ takes $O(n^3)$ time.

At this point, we have expreseed if $v_{i,j}\in S$ through a linear combination of $Z$. How do we find a $S_1$ that works? The idea is to consider the linear relations defined by $D[n,1], \ldots, D[n,n]$. We can see it is of the following form.

\[
\begin{aligned}
D[n,1] &= C_{1,1} z_1+\ldots +C_{1,n} z_{n} + u_{1}\\
D[n,2] &= C_{2,1} z_1+\ldots +C_{2,n} z_{n} + u_{2}\\
 \vdots &\qquad  \vdots\\
D[n,n] &= C_{m,1} z_1+\ldots + C_{m,n} z_n + u_{n}
\end{aligned}
\]

If we solve the equation $Cz=u$, we find $z_1,\ldots,z_n$, which implies we find $S_1$. Note $C$ is just a $n\times n$ matrix, solving the equation takes $O(n^3)$ time. We have found the activation set restricted on the first row. Now, we use the theorem again to find the entire activation set $S$ through $S_1$ in $O(n^2)$ time. 

Building the table $D$ takes $O(n^3)$ time, solving $Cz=u$ also takes $O(n^3)$ time, compute $S$ from $S_1$ takes $O(n^2)$ time. The total running time is $O(n^3)$. 

# Remarks

One can generalize this further. We can obtain $O(m^2n)$ running time for a $m\times n$ grid, where $m\leq n$.
Also, there is no reason we have to work in $\F_2$, any arbitrary field is fine. 

::: Theorem
  Let $G$ be a $m\times n$ grid and $A$ is a matrix where the non-zero entires are precisely the position of $1$s in the adjacency matrix of $A$. Finding $Ax=b$ can be done in $O(m^2n)$ time. 
:::

How can this be generalized to other graphs? I haven't thought deeply about it. It would be interesting to see an algorithm with running time $O(n^{3/2})$ for a planar graph of $n$ vertices. 