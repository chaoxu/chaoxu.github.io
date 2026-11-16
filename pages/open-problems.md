---
title: Open problems
---

These questions remain open to my knowledge. Last status check: 16 November 2026. Solved problems will be removed, and no history of this list will be maintained here.

::: {.Problem #problem-ocdc title="Oriented cycle double cover"}
Does every finite bridgeless multigraph admit a multiset of oriented circuits traversing every edge exactly once in each direction? Parallel edges may form a $2$-cycle, whereas traversing one edge forward and immediately back is not a circuit.
:::

See [Gáborik et al. (p. 9)](https://epub.uni-bayreuth.de/id/eprint/8626/1/Multidimensional_flows%20%281%29.pdf#page=9).

::: {.Problem #problem-hedge-min-cut-derandomization title="Deterministic hedge min-cut"}
Fix an integer $k\ge2$. Let $G=(V,E)$ be a finite connected undirected multigraph with $|V|\ge2$, with $E$ partitioned into nonempty *hedges* such that the subgraph formed by each hedge and its incident vertices has at most $k$ connected components. Can a minimum-cardinality set of hedges whose removal disconnects $G$ be found in deterministic polynomial time?
:::

Randomized polynomial time is known ([Chandrasekaran et al., Theorem 1.1](https://chaoxu.prof/files/papers/hypergraph-k-cut.pdf#page=3)).

::: {.Problem #problem-global-dicut title="Global edge-bicut"}
Given a finite digraph $G=(V,E)$ with $|V|\ge2$ and binary-encoded nonnegative rational arc costs, find a minimum-cost arc set $F\subseteq E$ such that $G-F$ has two distinct vertices with no directed path between them in either direction. Unlike $s$-$t$ bicut, the separated pair is not prescribed. Is this problem polynomial-time solvable or NP-hard?
:::

Stated as open in [Bérczi et al. (p. 413)](https://karthik.ise.illinois.edu/pubs/linear-3-cut.pdf#page=3).

::: {.Problem #problem-global-linear-3-cut title="Global linear 3-cut"}
Given a finite digraph $G=(V,E)$ with $|V|\ge3$ and no prescribed terminals, find a minimum-cardinality arc set $F\subseteq E$ such that $G-F$ has at least three strongly connected components. Is this problem polynomial-time solvable or NP-hard?
:::

See [my thesis (Definition 1.2 and Table 1.2)](https://chaoxu.prof/files/papers/thesis.pdf#page=17).

::: {.Problem #problem-s-tractability-dichotomy title="Dichotomy for surjective minimum violation"}
Fix a finite digraph $H=(D,F)$ with $D\ne\varnothing$, allowing loops. Given a finite digraph $G=(V,E)$ with $|V|\ge|D|$ and binary-encoded weights $w:E\to\mathbb Z_{\ge0}$, minimize $\sum_{(u,v)\in E:\,(\phi(u),\phi(v))\notin F}w(u,v)$ over surjections $\phi:V\twoheadrightarrow D$. Characterize the patterns $H$ for which this problem is polynomial-time solvable, and determine whether it is polynomial-time solvable or NP-hard for every $H$.
:::

Formulated in [Kawarabayashi and Xu (§1, §7)](https://chaoxu.prof/files/papers/min-violation.pdf), which settles reflexive trees.

::: {.Problem #problem-rooted-connectivity-certificate title="Fast rooted connectivity certificates"}
Let $G=(V,E)$ be a finite $n$-vertex, $m$-arc digraph with root $r\in V$, and for $x\in V\setminus\{r\}$ let $\lambda_G(r,x)$ be the maximum number of arc-disjoint directed $r$-to-$x$ paths. Given an integer $k\ge1$, can one construct a spanning subgraph $H=(V,F)$ with $F\subseteq E$, $|F|=O(kn)$, and $\lambda_H(r,x)\ge\min\{k,\lambda_G(r,x)\}$ for all $x\in V\setminus\{r\}$ in $O(n+km)$ time, or even $O(n+m)$ time?
:::

Existence of such subgraphs follows from [Szeszlér (Theorem 8)](https://arxiv.org/pdf/2502.10052#page=8).

::: {.Problem #problem-submodular-k-partition title="Minimum submodular k-partition"}
For every fixed integer $k\ge6$, is there a polynomial $p_k$ such that, given an arbitrary submodular function $f:2^U\to\mathbb R$ via a value oracle on an $n$-element set $U$ with $n\ge k$, one can partition $U$ into $k$ nonempty parts $P_1,\dots,P_k$ minimizing $\sum_{i=1}^k f(P_i)$ using at most $p_k(n)$ oracle calls and arithmetic and comparison operations?
:::

For background on fixed-$k$ submodular partitioning, see [Hirayama et al. (§7)](https://chaoxu.prof/files/papers/sub4part-journal.pdf#page=12).

::: {.Problem #problem-separated-submodular-3-partition title="Minimum separated submodular 3-partition"}
Is there a polynomial $p$ such that, given an arbitrary submodular function $f:2^U\to\mathbb R$ via a value oracle on an $n$-element set $U$ with $n\ge3$ and distinct $s,t\in U$, one can partition $U$ into three nonempty parts $A,B,C$ with $s\in A$ and $t\in B$ minimizing $f(A)+f(B)+f(C)$ using at most $p(n)$ oracle calls and arithmetic and comparison operations?
:::

Approximation and variable-part variants are studied in [Bérczi et al. (§1.1.1)](https://arxiv.org/html/2510.25664v3).

::: {.Problem #problem-ranked-submodular-3-partition title="Ranked enumeration of submodular 3-partitions"}
Is there a polynomial $p$ such that, given any submodular function $f:2^U\to\mathbb R$ via a value oracle on an $n$-element set $U$ with $n\ge3$, one can output every unordered partition $\mathcal P$ of $U$ into three nonempty parts once in nondecreasing order of $\sum_{P\in\mathcal P}f(P)$ (ties broken arbitrarily), where producing the first $r$ outputs takes at most $(r+1)p(n)$ oracle calls and arithmetic and comparison operations for each $r$ up to the number of such partitions, and reporting termination takes at most $p(n)$ more?
:::

Ranked enumeration of submodular $2$-partitions appears in [Hirayama et al. (Lemma 5.2)](https://chaoxu.prof/files/papers/sub4part-journal.pdf#page=10).

::: {.Problem #problem-exact-matching title="Deterministic Exact Matching"}
Given a finite undirected graph $G=(V,E)$, a set $R\subseteq E$ of red edges, and an integer $r$, is there a deterministic polynomial-time algorithm deciding whether $G$ has a perfect matching $M$ with $|M\cap R|=r$?
:::

Randomized polynomial time is known (see [Sebő, p. 6](https://arxiv.org/pdf/2602.03966v2#page=6)).

::: {.Problem #problem-min-cost-exact-matching title="Minimum-cost Exact Matching"}
Given a finite undirected graph $G=(V,E)$, a set $R\subseteq E$ of red edges, an integer $r$, and binary-encoded costs $c:E\to\mathbb Z_{\ge0}$, can one find a perfect matching $M$ minimizing $\sum_{e\in M}c(e)$ subject to $|M\cap R|=r$, or report that none exists, in time polynomial in the input bit length (allowing randomized algorithms with success probability at least $2/3$ on every input)?
:::

Open even for bipartite graphs ([Armbruster et al., §§5.2–5.3](https://drops.dagstuhl.de/storage/00lipics/lipics-vol374-icalp2026/LIPIcs.ICALP.2026.13/LIPIcs.ICALP.2026.13.pdf#page=16)).

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

Is this problem solvable in polynomial time for each fixed $n\ge3$ (even when $n=3$ and $w_j=2^{j-1}$)? More strongly, is it solvable in $g(n)N^{O(1)}$ time for some computable $g$, or is the decision version with a binary-encoded cost threshold W[1]-hard parameterized by $n$? For unrestricted $n$ and $m$, is there an exact $\operatorname{poly}(N,w_m)$-time algorithm?
:::

See my [question on filling bins with minimum moves](https://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves).

::: {.Problem #problem-six-square-cover title="Covering a square by six unit squares"}
Can six closed unit squares in the Euclidean plane, translated and rotated independently with overlaps allowed, cover an entire square of side length strictly greater than $2$?
:::

Five unit squares cannot, while seven suffice ([Dósa et al., Conjecture 1.1 and Theorem 1.3](https://arxiv.org/abs/2601.16535v3)).
