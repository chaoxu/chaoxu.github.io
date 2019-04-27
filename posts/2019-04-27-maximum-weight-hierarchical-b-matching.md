---
title: Maximum weight hierarchical $b$-matching
tags: combinatorial optimization, matching, matroid
---

We consider the following problem, which appeared in [@EmekKSZ19].

Let $\mathcal{L}$ be a laminar family consists of sets $F_1,\ldots,F_k$. Let $u_1,\ldots,u_k$ to be positive integers. 
Consider a graph $G=(V,E)$ with a weight function $w:E\to \N$ and capacity function $c:E\to \N$. We are interested in finding a $y\leq c$, such that for every $F_i\in \mathcal{L}$, we have $\sum_{v\in F_i} \sum_{e:v\in e\in E} y_e \leq u_i$, and $\sum_{e\in E} y_ew_e$ is maximized.

Formally, it is the following integer program.

\begin{aligned}
& \max_{y\in \Z^m} & & \sum_{e} w_e y_e & \\
& \text{s.t.} & & \sum_{v\in F_i} \sum_{e:v\in e\in E} y_e \leq u_i & i\in [k] \\
& & &  0\leq y_e \leq c_e & \forall e\in E \\
\end{aligned}

This is a generalization of the maximum weight $c$-capacitated $b$-matching problem. Indeed, we can simply set $F_i = \set{v_i}$ and $u_i=b_i$. 
However, this problem is actually no more general than the maximum weight $c$-capacitated $b$-matching problem.

Let $A \in \Z^{m\times n}$ be a matrix such that $\sum_{i=1}^m |A_{i,j}|\leq 2$ for every $j$. We call $A$ a bidirected matrix.

{Theorem}
    Given $A \in \Z^{m\times n}$ a bidirected matrix and vectors $a,b\in \Z^m$, $c,d,w\in \Z^n$. The integer program $\max_{x\in \Z^n} \set{wx \mid a\leq Ax\leq b, c\leq x\leq d}$ can be solved in polynomial time. In particular, it is equivalent to the maximum weight $b$-matching problem on graph of size $poly(m,n)$.

The above theorem can be found in [@Schrijver03, chap. 36]. Note that in Schrijver's book, one requires $\sum_{i=1}^m |A_{i,j}|=2$. It is not hard to see the statement still holds even if we have $\leq$ in place of $=$.

We will express the maximum weight hierarchical $b$-matching problem as an integer program over a polytope defined over a bidirected matrix. The integer program is a modification of the integer program in [@KaparisLM17]. The integer program here is simpler, because we are not trying to reduce to *perfect* $b$-matching.

We define $F_i' = F_i \setminus \bigcup_{j: F_j\subsetneq F_i} F_j$.
We also define $C_i$ to be the indices $j$, such that for all $k$, $F_j\subseteq F_k \subsetneq F_i$ implies $j=k$.
$y_e$ denote the amount of capacities we assign to $e$, $x_v$ denotes the capacitated degree, hence $x_v = \sum_{e:v\in e\in E} y_e$. We define $z_i = \sum_{v\in F_i} x_v$, which can be transformed to $z_i = \sum_{v\in F_i'} x_v + \sum_{j\in C_i} z_j$. Therefore we obtain the following integer program by directly applying substitutions.

\begin{aligned}
& \max_{x\in \Z^n,y\in \Z^m, z\in \Z^k} & & \sum_{e} w_e y_e & \\
& \text{s.t.} & & \sum_{v\in F_i'} x_v + \sum_{j\in C_i} z_j - z_i= 0 & i\in [k] \\
& & &  \sum_{e: v\in e\in E} y_e -x_v = 0 & \forall v\in V \\
& & &  0\leq y_e \leq c_e & \forall e\in E \\
& & &  0\leq z_i \leq u_i & \forall i\in [k] \\
\end{aligned}

The matrix here is a bidirected matrix. This shows the original problem can be solved in polynomial time. 