---
title: Balanced partition for trees
tags: tree, dynamic programming
---

{Problem}

    Let $T$ be a tree with non-negative weights on the edges. Partition the vertices so each side have $n/2$ vertices and minimize the sum of the weights of the edges crossing the partition.

This problem can be solved in $O(n^3)$ time, and it generalizes to graphs with constant treewidth [@doi:10.1137/S009753970139567X]. 

Here we describe the dynamic programming solution for tree and also show the running time of $O(n^2)$ is possible. This is probably the solution in [@doi:10.1137/S009753970139567X] when the input is a tree, so this might lead to $O(2^kn^2)$ time algorithm for graphs with treewidth $k$.

This problem can be reduced to the following problem by arbitrarily root the tree at a leaf, and replace each vertex with degree $d+1$ into a binary tree with $d$ leaves, and assign weight $0$ to the internal nodes (except the root) of the replacing binary tree, and infinite edge weight for the edges connecting internal nodes inside the binary tree. 

{Problem}
    Let $T$ be a rooted binary tree with non-negative weights on the edges, and $0-1$ weights on the vertices. Partition the vertices so each side have the same weight and minimize the sum of the weights of the edges crossing the partition.

Let $W(v)$ to be the sum of the vertex weights of the subtree rooted at $v$. Let $D(v,k)$ to be the subproblem of minimum cost partition of the the vertices of the tree rooted at $v$ into vertices $(A,B)$ such that $v\in A$ and $\sum_{a\in A} {w(a)}=k$. Assume $v$ have two children $u$ and $w$. There are 4 cases to consider, depending on which of $\{u,w\}$ is on the same side of the partition as $v$.

\[
D(v,k) = \min \begin{cases}
\min_{i} D(u,i) + D(w,k-w(v)-i)\\
\min_{i} w(vu) + D(u,W(u)-i) + D(w,k-w(v)-i)\\
\min_{i} w(vw) + D(u,i) + D(w,W(w) - (k-w(v)-i))\\
\min_{i} w(vu) + w(vw) + D(u,W(u)-i) + D(w,W(w)-(k-w(v)-i))\\
\end{cases}
\]

Where $i$ taken over all the numbers that make sense. Other cases are similar and simpler. 
Note that $D(v,k)$ for a particular $k$ can be computed $O(k \min\{W(u),W(w)\})$ time. So computing $D(v,k)$ for all $1\leq k\leq W(v)$ takes $O(W(u)W(w))$ time.

Hence the running time would obey $T(n) \leq \max_{a+b=n} T(a)+T(b)+O(ab)$. One can show $T(n)=O(n^2)$ is a solution by induction.

