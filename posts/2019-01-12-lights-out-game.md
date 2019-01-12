---
title: Lights out game on a grid
tags: algorithm, algebra
---

{Problem}
  Let $G$ be a graph, let $A$ be the adjacency matrix of $G$. Solve the equation $Ax=b$ in $\F_2$.

The problem is equivalent to the [lights out game](https://en.wikipedia.org/wiki/Lights_Out_%28game%29). Each vertex has state $0$ or $1$. Activate a vertex flips the state of itself and all its neighbors. Find a set of activations that turns all state into $0$.
Originally I thought this problem can be solved in $O(n^{\omega/2})$ when $G$ is planar graph on $n$ vertices by [nested dissection](https://en.wikipedia.org/wiki/Nested_dissection). However, only recently I found out the matrix must be non-singular. Therefore nested dissection does not apply. 

Recently I saw an algorithm that shows if the graph is a $n\times n$ grid, then it can be solved in $O(n^3)$ time. The solution in Chinese and can be seen [here](https://zhuanlan.zhihu.com/p/53646257).

Given a $n\times n$ grid graph. Let $v_{i,j}$ be the node on the $i$th row and $j$th column. 
Let $b_{i,j}$ be the state of the vertex $v_{i,j}$. The state is in $\F_2$
If we activates a node, the state of the node and its neighbors change by $1$.
The set of activated node is called the activation set.

We are interested in finding an activation set $S$, such the state of all nodes after activate $S$ is $0$.

Let $S$ be the activation set, and $S_1$ to be the activation set of the first row. 

{Theorem}
  $S_1$ uniquely determines $S$. Moreover, One can compute $S$ from $S_1$ in $O(n^2)$ time.

{Proof}
  Indeed, consider apply activation to the nodes in $S_1$. Consider any vertex in row $1$. If it $0$, then the remaining neighbor (on the second row) cannot be activated. If it is $1$, then the remaining neighbor has to be activated. 

Let $D[i,j]$ indicates if we activate $v_{i,j}$ or not.
We create formal variables $Z=\set{z_1,\ldots,z_n}$. Here $z_i$ is an indicator variable that represents if $v_{1,i}$ is activated or not.
The base case $D[1,j] = z_j$.
The observation shows that for each $i$ and $j$, $D[i+1,j] = 1 + D[i,j-1]+D[i,j]+D[i,j+1]+D[i-1,j]+b_{i,j}$.
We can express $D[i,j]$ as a sum of elements in $Z$ and a constant, and we are summing a constant number of previous states. So it has size $O(n)$.
We can compute the expression of $D[i,j]$ in $O(n)$ time.
So computing all $D[i,j]$ for $i\geq 2$ takes $O(n^3)$ time.

We are interested in $D[n,1],\ldots,D[n,n]$. We can see it is of the following form.

\begin{align*}
D[n,1] &= c_{1,1} z_1+\ldots +c_{1,n} z_{n} + u_{1}\\
D[n,2] &= c_{2,1} z_1+\ldots +c_{2,n} z_{n} + u_{2}\\
 \vdots &\qquad  \vdots\\
D[n,n] &= c_{m,1} z_1+\ldots + c_{m,n} z_n + u_{n}\\
\end{align*}

We solve the equation $Cz=u$. Note here $C$ is just a $n\times n$ matrix. We finds $z_1,\ldots,z_n$. So now we have found the activation set restricted on the first row. We can use it to find the entire activation set. 

The total running time is $O(n^3)$. Building the table $D$ and solving $Cz=u$.
One can generalize this a bit further. We can obtain $O(m^2n)$ running time for a $m\times n$ grid, where $m\leq n$.
Also, there is no reason we have to work in $\F_2$, any arbitrary field is fine. 

{Theorem}
  Let $G$ be a $m\times n$ grid and $A$ is a matrix where the non-zero entires are precisely the position of $1$s in the adjacency matrix of $A$. Finding $Ax=b$ can be done in $O(m^2n)$ time. 

I did not think too much into it, but maybe it works for all integral domains too.
Interestingly, this algorithm is so special, that we have no idea how to extend it to other graphs. 
Maybe it works for directed graph, maybe it works for subgraph of the grid graphs.

It would be really interesting to see an algorithm with running time $O(n^{3/2})$ for a planar graph of $n$ vertices.