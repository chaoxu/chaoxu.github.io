---
title: Bounds on number of cuts
tags: Graph Theory
---

Recently there are some advances on counting the number of min-cut in graphs.

Consider we have an undirected graph $G=(V,E)$ of $n$ vertices, and there are _positive_ cost $c:E\to \R^+$ on the edges. We define $c(F)=\sum_{e\in F}c(e)$, to be the cost (value) of $F\subset E$. 

Let $\mathcal{P}$ be a partition of $V$ where each partition class is non-empty. We define $E(\mathcal{P})$ to be the set of edges with end points in two different partition classes. 
A set of edges $F$ is called a _$k$-cut_, if $F=E(\mathcal{P})$ for some $\mathcal{P}$ such that $|\mathcal{P}|\geq k$.

We stress that by this definition, a $k$-cut is a $k-1$-cut. A _cut_ is a $2$-cut. A min-$k$-cut is a $k$-cut of minimum value (cost). We let $\lambda_k$ to denote the value of the min-$k$-cut.

It is well known that the number of min-cuts in a graph is ${n\choose 2} = O(n^2)$. [@KargerS96] 

In the entire article, unless specifically stated, we assume $k$ is a fixed integer at least $2$, and $\alpha$ is a fixed value at least $1$.

We can express the state alternatively.

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \lambda_2$ is $O(n^2)$.

:::

# Bounds related to scaling of the min-cut

What happens when we want to know about the number of cuts with value at most $\alpha \lambda_2$? 

By simply analyzing Karger's algorithm, one can obtain the following.

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_2$ is $O(n^{2\alpha})$.

:::

With more careful analysis using tree packing, Karger obtained the following [@Karger00]. 

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_2$ is $O(n^{\floor{2\alpha}})$.

:::

Indeed, we do have a lower bound of ${n \choose \floor{2\alpha}}$. Just consider an unweighted cycle, where min-cut has value $2$. We can pick any $\floor{2\alpha}$ edges, which forms a cut of value at most $\alpha$ times the min-cut. 

Note that we require $\alpha$ to a fixed value. This is because there is a dependency on $\alpha$ hidden inside the big $O$. Our lower bound is absolute and does not depending on $\alpha$ being fixed. It be interesting to obtain a matching upper bound. Hence we can consider the problem with strict inequality. 

::: {.Conjecture #approxcutconjecture}

  The number of cuts $F$ such that $c(F)< \alpha \lambda_2$ is $O(n^{\ceil{2\alpha}-1})$.

:::

Henzinger and Williamson showed the conjecture true for all $\alpha\leq \frac{3}{2}$ [@HenzingerW96].

# Bounds related to min-$k$-cut

There are multiple ways to obtain the following theorem. For example, directly generalize Karger's argument for cut counting. 

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \lambda_k$ is $O(n^{2(k-1)})$.

:::

There were many attempts, and people can only obtain lower bounds of the form ${n\choose k}$. Again, a cycle would be an example of such lower bound. The min-$k$-cut has value $k$, and can be obtained by picking any $k$ edges. The gap is pretty large. Hence one would tempt to conjecture the following.

::: {.Conjecture #kcutconjecture}

  The number of cuts $F$ such that $c(F)\leq \lambda_k$ is $O(n^k)$.

:::

Recently, Gupta, Lee and Li almost closes the gap [@GuptaLL19].

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \lambda_k$ is $\hat{O}(n^k)$.

:::

Here $\hat{O}$ hides a factor smaller than any $n^\epsilon$. While closing the gap, they showed thhe following interesting theorem. 

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \frac{(2-\epsilon)\lambda_k}{k}$ is $O(n)$.

:::

::: Conjecture

  The number of cuts $F$ such that $c(F)< \frac{2\lambda_k}{k}$ is $O(n)$.

:::

Note this theorem is basically shows we can also obtain interesting results for $\alpha = \frac{2-\epsilon}{k} < 1$. 


How about approximate min-$k$-cuts? Chekuri, Quanrud and I extended the tree packing analysis of Karger, and obtained the following result for $k$-cuts [@ChekuriQX19].

::: Theorem

  The number of cuts $F$ such that $c(F)\leq \alpha \lambda_k$ is $O(n^{\floor{\alpha 2(k-1)}})$.

:::


Combining the [@kcutconjecture] and [@approxcutconjecture], we get a unified conjecture, even for $\alpha<1$.

::: Conjecture

  The number of cuts $F$ such that $c(F)< \alpha \lambda_k$ is $O(n^{\ceil{\alpha k}-1})$.

:::

# Bounds on parametric cuts

Now, let's consider parametric cuts. Consider we have $d$ weight functions $c_1,\ldots,c_d:E\to \R_{\geq 0}$. Define $c_\mu(e) = \sum_{i=1}^d \mu_i c_i(e)$. We interested in knowing about cuts $F$ such that $c_\mu(F)$ is bounded by $\alpha \lambda_{\mu,k}$, where $\lambda_{\mu,k}$ is the min-$k$-cut value when the cost function is $c_\mu$.

Karger showed the following [@Karger16].

::: Theorem

  The number of cuts $F$ such that $c_\mu(F)\leq \lambda_{\mu,2}$ for some $\mu\in \R_{\geq 0}^d$ is $O(n^{d+1})$.

:::

A even more general theorem follows. 

::: Theorem

  The number of cuts $F$ such that $c_\mu(F)\leq \alpha \lambda_{\mu,k}$ for some $\mu\in \R_{\geq 0}^d$ is $O(n^{2\alpha(k-1)+d-1})$.

:::

Hence, we would have the following conjecture.

::: Conjecture

  The number of cuts $F$ such that $c_\mu(F)< \alpha \lambda_{\mu,k}$ for some $\mu\in \R_{\geq 0}^d$ is $O(n^{\ceil{\alpha k}+d-2})$.

:::

Note, we might relax the requirement that all $c_i$ and $\mu$ are non-negative. Aissi et. al. showed the following [@AissiMMQ15].

::: Theorem

  The number of cuts $F$ such that $c_\mu(F)\leq \lambda_{\mu}$ for some $c_\mu\geq 0$ is $O(m^d n^2\log^{d-1} n)$.

:::

The following would be even stronger conjecture. 

::: Conjecture

  The number of cuts $F$ such that $c_\mu(F) < \alpha \lambda_{\mu,k}$ for some $c_\mu\geq 0$ is $O(n^{\ceil{\alpha k}+d-2})$.

:::

# Projected cut bounds

Let $\tau_e = \min_{U:e\in \delta(U)}c(\delta(U))$. Fung et. al. showed a projected generalization of the cut counting bound [@FungHHP19]. Here I state a nicer form that appeared in [Harvey's lecture notes on sparsification](https://www.win.tue.nl/~nikhil/courses/2013/2WO08/Nick-sparsification.pdf).

::: Theorem
  
  Let $B\subset E$, and $\tau_B = \min_{e\in B} \tau_e$, the number of sets of the form $F\cap B$ where $F$ is a cut such that $c(F)\leq \alpha \tau_B$ is $O(n^{2\alpha})$.

:::

If we let $B=E$, this is precisely the approximate cut counting bound. 

We can of course ask if all our theorem can be applied to projected cuts. We don't even know if it extends to $k$-cuts. However, we can expect the following ultimate conjecture.

Let $\tau_{\mu,k,e}$ be the minimum over all $c_{\mu}(F)$, where $F$ is a $k$-cut containing $e$. 
Let $\tau_{\mu,k,B} = \min_{e\in B} \tau_{\mu,k,e}$.

::: Conjecture
  
  Let $B\subset E$, the number of sets of the form $F\cap B$ where $F$ is a cut such that $c_{\mu}(F)< \alpha \tau_{\mu,k,B}$ for some $c_\mu\geq 0$ is $O(n^{\ceil{\alpha k}+d-2})$.

:::
