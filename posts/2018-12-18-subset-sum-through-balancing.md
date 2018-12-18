---
title: Subset sum through balancing
tags: algorithms, subset sum
---

This is a note for Pisinger's balancing algorithm for subset sum [@Pisinger19991]. Let $\SSS(S)$ be the set of all subset sums of $S$. The subset sum problem, the input is $S\subset \N$, and we are interested in checking if $t\in \SSS(S)$. 

We define a variation of the subset sum problem. The \emph{balanced subset sum problem}. In this problem, we are given a vector $v$ of integers(does not have to be positive). We let $M=\|v\|_\infty$. We are interested in find a subset that sums to $t\in [M]$. 

{Theorem}
    Each subset sum problem on $n$ elements can be reduced to a balanced subset sum problem in $n$ elements in $O(n)$ time. 

{Proof}
    Consider the input to the subset sum problem $S$ and $t$. Greedily find a subset of elements $S'$, such that adding any other element will exceed $t$. Let $\sigma(S')=t'$. Now, we negate all the elements in $S'$, and ask for balanced subset sum with input set $-S' \cup (S\setminus S')$ and target number $t-t'$.

We will partition $S$ into $A = [-M..0]\cap S$ and $B=S\setminus A$. We also define $A_i = \set{a_1,\ldots,a_i}$ and $B_i=\set{b_1,\ldots,b_i}$.

A set is balanced by the following recursive definition.
Let $S$ be a set.

  - $S=\emptyset$ is balanced.
  - $\sigma(S)> t$, then $S\cup \set{a}$ is balanced, where $a\in A$.
  - $\sigma(S)\leq t$, then $S\cup \set{b}$ is balanced, where $b\in B$.

Consider a set $T$, such that $(i,j,k)\in T$ if and only if $k$ is a subset sum of $A_i\cup B_j$. 
Certainly, we are interested if $(|A|,|B|,t)$ is in $T$.
However, the state space is already $O(n^2M)$, which is no better than the standard dynamic programming algorithm. 

There is a nice dominating relation. If $(i,j,k)\in T$, then for $(i',j')\geq (i,j)$, we have $(i',j',k)\in T$.
We can ask for each $k$, what are all the minimal $(i,j)$ pairs where $(i,j,k)\in T$. Such value will be $g(j,k)$.
Formally, $g(j,k) = \min \set{i | (i,j,k)\in T}$, one can see that $g(j,k) \geq g(j+1,k)$. 
Also, we know the solution corresponding to $g(j,k)$ must contain $a_{g(j,k)}$ as an element.

One can get a very nice recurrence relation for $g$ as below.

\[
g(j,k)= \min \begin{cases}
g(j-1,k)\\
g(j-1,k-b_j) & \text{if }k-b_j\leq t\\
i & \text{if }k-a_i > t \text{ and } i>g(j,k-a_i)
\end{cases}
\]

Let $A_i'\subset A_i$, $B_j'\subset B_j$ and $i$ is as small as possible such that $A_i'\cup B_j'$ is balanced and sums to $k$. Note that $a_i\in A_i'$.

We obtained $A_i'\cup B_j'$ by inserting an element in $B$ or $A$ to another balanced set. If the inserted element is in $B$, but not $b_j$, then we know $i=g(j-1,k)$. If it is $b_j$, then $i=g(j-1,k-b_j)$.
If the last inserted is $a_i$, then $g(j,k)=i$. Note we observe in this case, $g(j,k-a_i)<i$.
A direct dynamic programming implementation seems to imply a $O(n^2M)$ time algorithm, since there does not seem to be a quick way to obtain $i$. 

On the other hand, if we think bottom up instead of top down, we can obtain a better result. Below is the algorithm.

![The algorithm](/files/balanced_subsetsum.png)

The value $D[j,k]$ eventually equals $g(j,k)$. 
It is frustrating that the DP algorithm cannot be inferred directly from the recurrence relation. 
Indeed, we mainly obtained this through the fact that we can prune the search space if we start bottom up, which is unclear from the recurrence relation. 