---
title: Bounds on number of cuts
tags: Graph Theory
---

Consider we have an undirected graph $G=(V,E)$ of $n$ vertices, and there are _positive_ cost $c:E\to \R^+$ on the edges. We define $c(F)=\sum_{e\in F}c(e)$, to be the cost (value) of $F\subset E$. 

Let $\mathcal{P}$ be a partition of $V$ where each partition class is non-empty. We define $E(\mathcal{P})$ to be the set of edges with end points in two different partition classes. 
A set of edges $F$ is called a _$k$-cut_, if $F=E(\mathcal{P})$ for some $\mathcal{P}$ such that $|\mathcal{P}|\geq k$.

We stress that by this definition, a $k$-cut is always a $j$-cut for $j\leq k$. A _cut_ is defined as a $2$-cut. A min-$k$-cut is a $k$-cut of minimum value (cost). We let $\lambda_k$ to denote the value of the min-$k$-cut. A $\alpha$-approximate $k$-cut is a cut of value at most $\alpha\lambda_k$.

It is well known that the number of min-cuts in a graph is ${n\choose 2}$. [@KargerS96] 

Unless specifically stated, we assume $k$ is a fixed integer at least $2$, and $\alpha$ is a fixed value at least $1$.

We can express the state alternatively.

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \lambda_2$ is $O(n^2)$.

:::

# Bounds related to approximate min-cuts

What happens when we want to know about the number of cuts with value at most $\alpha \lambda_2$? 

By simply analyzing Karger's algorithm, one can obtain the following.

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_2$ is $O(n^{2\alpha})$.

:::

With more careful analysis using tree packing, Karger added a floor function to the exponent [@Karger00]. 

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_2$ is $O(n^{\floor{2\alpha}})$.

:::

Indeed, we do have a lower bound of ${n \choose \floor{2\alpha}}$. Just consider an unweighted cycle, where min-cut has value $2$. We can pick any $\floor{2\alpha}$ edges, which forms a cut of value at most $\alpha$ times the min-cut. 

Note that we require $\alpha$ to a fixed value. This is because there is a dependency on $\alpha$ hidden inside the big $O$. Our lower bound is absolute and does not depending on $\alpha$ being fixed. It would be interesting to obtain a matching upper bound for arbitrary $\alpha$. Hence we can consider the problem with **strict inequality**. 

::: {.Conjecture #approxcutconjecture}

  The number of cuts $F$ such that $c(F)< \alpha \lambda_2$ is $O(n^{\ceil{2\alpha}-1})$.

:::

Henzinger and Williamson showed the conjecture true for all $\alpha\leq \frac{3}{2}$ [@HenzingerW96].

# Bounds related to $\alpha$-approximate min-$k$-cut

There are multiple ways to obtain the following theorem. For example, directly generalize Karger's argument for cut counting. 

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \lambda_k$ is $O(n^{2(k-1)})$.

:::

One can show a lower bounds of the form ${n\choose k}$. Again, a cycle would be an example of such lower bound. The min-$k$-cut has value $k$, and can be obtained by picking any $k$ edges. Recently, the upper bound has been closed (up to constant factor if $k$ is fixed) [@GuptaHLL21].

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \lambda_k$ is $n^k k^{O(k^2)}$. Namely $O(n^k)$ for fixed $k$.

:::

In fact, they proved it by prorving the stronger theorem on $\alpha$-approximate min-$k$-cuts.

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_k$ is $n^{\alpha k} k^{O(\alpha k^2)}$. Namely $O(n^{\alpha k})$ for fixed $k$.

:::

Unlike $\alpha$-approximate min-cuts, there is no floor function in the exponent. Hence a natural conjecture would be. 

::: Conjecture 

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_k$ is $O(n^{\lfloor \alpha k\rfloor})$.

:::

# Bounds on (approximate) parametric cuts

Consider we have $d$ weight functions $c_1,\ldots,c_d:E\to \R_{\geq 0}$. Define $c^\mu(e) = \sum_{i=1}^d \mu_i c_i(e)$ for $\mu\in \R_{\geq 0}^d$. We are interested in knowing about cuts $F$ such that $c^\mu(F)$ is bounded by $\alpha \lambda_{\mu,k}$, where $\lambda_{\mu,k}$ is the min-$k$-cut value when the cost function is $c^\mu$.

Karger showed the following [@Karger16].

::: Theorem

  The number of cuts $F$ such that $c^\mu(F)\leq \lambda_{\mu,2}$ for some $\mu\in \R_{\geq 0}^d$ is $O(n^{d+1})$.

:::

A even more general theorem follows, allowing $\alpha$-approximate $k$-cuts.

::: Theorem

  The number of cuts $F$ such that $c^\mu(F)\leq \alpha \lambda_{\mu,k}$ for some $\mu\in \R_{\geq 0}^d$ is $O(n^{2\alpha(k-1)+d-1})$.

:::

Of course, knowing the current result for number of $\alpha$-approximate $k$-cuts, we would conjecture the following for parametric cuts.

::: Conjecture

  The number of cuts $F$ such that $c^\mu(F) \leq \alpha \lambda_{\mu,k}$ for $\mu\in \R_{\geq 0}^d$ is $O(n^{\alpha k+d-1})$.

:::

Note, we might relax the requirement that all $c_i$ and $\mu$ are non-negative. Aissi et. al. showed the following [@AissiMMQ15], only assuming $c^\mu \geq 0$.

::: Theorem

  The number of cuts $F$ such that $c^\mu(F)\leq \lambda_{\mu}$ for some $c_\mu\geq 0$ is $O(m^d n^2\log^{d-1} n)$.

:::

Can we obtain a stronger bound for $c^\mu \geq 0$? 

Parametric min-cut is related to multicriteria min-cut, which also have a lot of open problems [@BeidemanCX23]. 

# Projected cut bounds?

Let $\tau_e = \min_{U:e\in \delta(U)}c(\delta(U))$, i.e. the minimum value of a cut containing $e$. Fung et. al. showed a projected generalization of the cut counting bound [@FungHHP19] by ignore edges appeared in small cuts. Let $E_\lambda = \set{ e | \tau_e \geq \lambda }$.

::: Theorem
  
  The number of sets of the form $F\cap E_\lambda$ where $F$ is a cut such that $c(F)\leq \alpha \lambda$ is $O(n^{2\alpha})$.

:::

If we let $\lambda$ be the min-cut value, this is precisely the $\alpha$-approximate cut counting bound. 

We can ask if all our theorem can have a projected cut version. We don't know if it extends to $k$-cuts (or what is the correct generalization).

# What about hypergraphs?

Most of the above results in graphs generalizes to rank $r$ hypergraphs with an extra factor related to $r$, often is $2^r$. See the table in [@BeidemanCX23] for a survey. However, almost all *exact* min-cut counting problem are unknown for hypergraphs with unbounded rank.

::: Conjecture

All the exact min-cut counting can be generalized to hypergraphs with unbounded rank.

:::

 One can construct a hypergraph where every set $\emptyset \subsetneq S\subsetneq V$ induces a distinct $\alpha$-approximate min-cut for all $\alpha>1$, so counting $\alpha$-approximate min-cut is unintresting for hypergraphs of unbounded rank. 