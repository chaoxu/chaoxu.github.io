---
title: Open problems
---

These questions remain open to my knowledge. Last status check: 20 November 2026. These are the only three combinatorial optimization problems I still care deeply about. Since the rise of AI, the problems I care about have been solved one by one. Once these are solved, I will probably take a long break and do something else. They are ordered below by how much I care about them, from most to least. Solved problems will be removed, and no history of this list will be maintained here.

::: {.Problem #problem-exact-matching title="Deterministic Exact Matching"}
Given a finite undirected graph $G=(V,E)$, a set $R\subseteq E$ of red edges, and an integer $r$, is there a deterministic polynomial-time algorithm deciding whether $G$ has a perfect matching $M$ with $|M\cap R|=r$?
:::

Randomized polynomial time is known (see [Sebő, p. 6](https://arxiv.org/pdf/2602.03966v2#page=6)).

::: {.Problem #problem-submodular-k-partition title="Minimum submodular k-partition"}
For every fixed integer $k\ge6$, is there a polynomial $p_k$ such that, given an arbitrary submodular function $f:2^U\to\mathbb R$ via a value oracle on an $n$-element set $U$ with $n\ge k$, one can partition $U$ into $k$ nonempty parts $P_1,\dots,P_k$ minimizing $\sum_{i=1}^k f(P_i)$ using at most $p_k(n)$ oracle calls and arithmetic and comparison operations?
:::

For background on fixed-$k$ submodular partitioning, see [Hirayama et al. (§7)](https://chaoxu.prof/files/papers/sub4part-journal.pdf#page=12).

::: {.Problem #problem-global-linear-k-cut title="Global linear k-cut for fixed k"}
Fix an integer $k\ge2$. Given a finite digraph $G=(V,E)$ with $|V|\ge k$ and no prescribed terminals, find a minimum-cardinality arc set $F\subseteq E$ such that $G-F$ has at least $k$ strongly connected components. Is this problem solvable in polynomial time for every fixed $k$, with the polynomial's degree allowed to depend on $k$, or is it NP-hard for some fixed $k$?
:::

See [my thesis (Definition 1.2 and Table 1.2)](https://chaoxu.prof/files/papers/thesis.pdf#page=17).
