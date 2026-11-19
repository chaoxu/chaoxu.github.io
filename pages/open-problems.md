---
title: Open problems
---

These questions remain open to my knowledge. Last status check: 16 November 2026. Solved problems will be removed, and no history of this list will be maintained here.

::: {.Problem #problem-hedge-min-cut-derandomization title="Deterministic hedge min-cut"}
Fix an integer $k\ge2$. Let $G=(V,E)$ be a finite connected undirected multigraph with $|V|\ge2$, with $E$ partitioned into nonempty *hedges* such that the subgraph formed by each hedge and its incident vertices has at most $k$ connected components. Can a minimum-cardinality set of hedges whose removal disconnects $G$ be found in deterministic polynomial time?
:::

Randomized polynomial time is known ([Chandrasekaran et al., Theorem 1.1](https://chaoxu.prof/files/papers/hypergraph-k-cut.pdf#page=3)).

::: {.Problem #problem-global-linear-k-cut title="Global linear k-cut for fixed k"}
Fix an integer $k\ge2$. Given a finite digraph $G=(V,E)$ with $|V|\ge k$ and no prescribed terminals, find a minimum-cardinality arc set $F\subseteq E$ such that $G-F$ has at least $k$ strongly connected components. Is this problem solvable in polynomial time for every fixed $k$, with the polynomial's degree allowed to depend on $k$, or is it NP-hard for some fixed $k$?
:::

See [my thesis (Definition 1.2 and Table 1.2)](https://chaoxu.prof/files/papers/thesis.pdf#page=17).

::: {.Problem #problem-submodular-k-partition title="Minimum submodular k-partition"}
For every fixed integer $k\ge6$, is there a polynomial $p_k$ such that, given an arbitrary submodular function $f:2^U\to\mathbb R$ via a value oracle on an $n$-element set $U$ with $n\ge k$, one can partition $U$ into $k$ nonempty parts $P_1,\dots,P_k$ minimizing $\sum_{i=1}^k f(P_i)$ using at most $p_k(n)$ oracle calls and arithmetic and comparison operations?
:::

For background on fixed-$k$ submodular partitioning, see [Hirayama et al. (§7)](https://chaoxu.prof/files/papers/sub4part-journal.pdf#page=12).

::: {.Problem #problem-exact-matching title="Deterministic Exact Matching"}
Given a finite undirected graph $G=(V,E)$, a set $R\subseteq E$ of red edges, and an integer $r$, is there a deterministic polynomial-time algorithm deciding whether $G$ has a perfect matching $M$ with $|M\cap R|=r$?
:::

Randomized polynomial time is known (see [Sebő, p. 6](https://arxiv.org/pdf/2602.03966v2#page=6)).

::: {.Problem #problem-gabow title="Gabow's serial symmetric exchange conjecture"}
Let $B_1$ and $B_2$ be disjoint bases of a finite rank-$r$ matroid. Can their elements be ordered as $B_1=\{x_1,\dots,x_r\}$ and $B_2=\{y_1,\dots,y_r\}$ so that $(B_1\setminus\{x_1,\dots,x_i\})\cup\{y_1,\dots,y_i\}$ and $(B_2\setminus\{y_1,\dots,y_i\})\cup\{x_1,\dots,x_i\}$ are both bases for every $1\le i\le r$?
:::

Stated in [Akrami et al. (Conjecture 1.14)](https://link.springer.com/article/10.1007/s00493-026-00217-y) and proved for [split](https://real.mtak.hu/236600/1/13537-PDFfile-58658-1-10-20260115.pdf) and [regular](https://doi.org/10.1016/j.jctb.2025.10.009) matroids.

::: {.Problem #problem-even-delta-proximity title="Finite-forbidden-set proximity for even delta-matroids"}
Let $(E,\mathcal D)$ be a finite even delta-matroid, $\Gamma$ an abelian group, $\gamma\in\Gamma^E$, $c\in\mathbb R^E$, and $F\subseteq\Gamma$ a finite forbidden set. Set $\lambda(X)=\sum_{e\in X}\gamma_e$ and $c(X)=\sum_{e\in X}c_e$, and suppose $\mathcal D_F=\{X\in\mathcal D:\lambda(X)\notin F\}$ is nonempty. For every minimizer $A$ of $c$ over $\mathcal D$, must there exist a minimizer $B$ of $c$ over $\mathcal D_F$ with symmetric difference $|A\triangle B|\le 2|F|$?
:::

Proved for matroid bases in [Liu et al. (Theorem 1)](https://arxiv.org/html/2610.32798v1).

::: {.Problem #problem-labelled-bin-filling title="Labelled-bin filling"}
Given $n$ bins, $m$ item types ($n,m\ge1$), and binary-encoded integers $1\le w_1<\cdots<w_m$ with $w_1\mid\cdots\mid w_m$, targets $h_i\ge0$, supplies $B_j\ge0$, and preferred counts $a_{ij}\ge0$ (of total input bit length $N$), find integers $x_{ij}\ge0$ minimizing $\sum_{i=1}^n\sum_{j=1}^m|x_{ij}-a_{ij}|$ subject to
$$
\begin{aligned}
\sum_{j=1}^m w_jx_{ij}&=h_i &&(1\le i\le n),\\
\sum_{i=1}^n x_{ij}&\le B_j &&(1\le j\le m),
\end{aligned}
$$
or report infeasibility.

Is this problem solvable in polynomial time for each fixed $n\ge3$ (even when $n=3$ and $w_j=2^{j-1}$)? For unrestricted $n$ and $m$, is there an exact $\operatorname{poly}(N,w_m)$-time algorithm?
:::

See my [question on filling bins with minimum moves](https://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves).
