---
title: Open problems
---

These questions remain open to my knowledge. Last status check: 16 November 2026.

## Graphs and directed cuts {.unnumbered}

::: {.Problem #problem-ocdc title="Oriented cycle double cover"}
Does every finite bridgeless multigraph admit a multiset of circuits, each given a cyclic orientation, such that every edge is traversed exactly once in each direction? Parallel edges may form a $2$-cycle, but traversing one edge forward and immediately back is not a circuit.
:::

The unrestricted existence question is discussed in Gáborik et al., [*Manhattan and Chebyshev flows*, p. 9](https://epub.uni-bayreuth.de/id/eprint/8626/1/Multidimensional_flows%20%281%29.pdf#page=9).

::: {.Problem #problem-global-dicut title="Global edge-bicut"}
Given a finite digraph $G=(V,E)$ with at least two vertices and nonnegative rational arc costs encoded in binary, find a minimum-cost set of arcs whose deletion leaves two vertices $s,t$ such that neither can reach the other by a directed path. The vertices $s,t$ are chosen as part of the solution. Is this problem solvable in polynomial time, or is it NP-hard?
:::

Bérczi et al. distinguish this global edge-deletion problem from the fixed-terminal and vertex-deletion variants and leave its complexity open in [*A tight $\sqrt{2}$-approximation for linear 3-cut*, p. 413](https://karthik.ise.illinois.edu/pubs/linear-3-cut.pdf#page=3).

::: {.Problem #problem-global-linear-3-cut title="Global linear 3-cut"}
Given a finite digraph $G=(V,E)$ with at least three vertices, find a minimum-cardinality arc set $F$ such that $G-F$ has at least three strongly connected components. Is this solvable in polynomial time, or is it NP-hard?
:::

This is the global problem, with no prescribed terminals. See [my thesis, Definition 1.2 and Table 1.2](https://chaoxu.prof/files/papers/thesis.pdf#page=17).

::: {.Problem #problem-rooted-connectivity-certificate title="Fast rooted connectivity certificates"}
Let $G=(V,E)$ be a finite digraph with $n$ vertices, $m$ arcs, and a root $r$. For $x\ne r$, let $\lambda_G(r,x)$ be the maximum number of arc-disjoint directed paths from $r$ to $x$. For an integer $k\ge1$, can one construct a subgraph $H=(V,F)$ with $F\subseteq E$ and $|F|=O(kn)$ satisfying $\lambda_H(r,x)\ge\min\{\lambda_G(r,x),k\}$ for every $x\ne r$ in total time $O(n+km)$, or even $O(n+m)$?
:::

The existence of such sparse subgraphs follows from Szeszlér's [connectivity-preserving subgraph theorem, Theorem 8](https://arxiv.org/pdf/2502.10052#page=8). The question concerns construction time. The $O(n)$ term accounts for vertices that may be unreachable from the root.

::: {.Problem #problem-hedge-min-cut-derandomization title="Deterministic hedge min-cut"}
Fix an integer $k\ge2$. Let $G=(V,E)$ be a finite connected undirected multigraph with at least two vertices, and let a partition of $E$ into nonempty classes, called hedges, be given. For each hedge, the subgraph formed by its edges and their incident vertices has at most $k$ connected components. Is there a deterministic polynomial-time algorithm finding a minimum number of hedges whose removal disconnects $G$?
:::

Chandrasekaran et al. give a randomized polynomial-time algorithm when the number of components per hedge is bounded, as a special case of [*Hypergraph k-Cut in Randomized Polynomial Time*, Theorem 1.1](https://chaoxu.prof/files/papers/hypergraph-k-cut.pdf#page=3). The question asks for derandomization.

## Cut counting {.unnumbered}

::: {.Problem #problem-hypergraph-k-cut-set-count title="Hypergraph minimum k-cut-sets"}
Fix an integer $k\ge3$. Let $H=(V,E)$ be a finite hypergraph with $n\ge k$ vertices and positive hyperedge costs. For a partition of $V$ into at least $k$ nonempty parts, its cut-set consists of the hyperedges meeting more than one part, and its cost is the sum of their costs. Is the number of distinct minimum-cost cut-sets $O(n^k)$ for every fixed $k$, with no bound on hyperedge size?
:::

The known upper bound is $O(n^{2k-2})$, while examples give $\Omega(n^k)$ distinct minimum cut-sets. Beideman et al. ask for a tighter bound in [*Deterministic enumeration of all minimum cut-sets and k-cut-sets in hypergraphs for fixed k*, §7](https://karthik.ise.illinois.edu/pubs/det-hyp-k-cut-set-enumeration.pdf#page=29). The objects counted here are hyperedge sets, so different vertex partitions with the same cut-set count once.

## Submodular optimization {.unnumbered}

::: {.Problem #problem-separated-submodular-3-partition title="Minimum separated submodular 3-partition"}
Let $f:2^U\to\mathbb R$ be submodular on a finite set $U$ with $|U|\ge3$, given by a value oracle, and let $s,t\in U$ be distinct. Can one find nonempty pairwise disjoint sets $A,D,C$ covering $U$, with $s\in A$ and $t\in D$, minimizing $f(A)+f(D)+f(C)$ using polynomially many oracle calls and arithmetic and comparison operations in $|U|$, independently of the magnitudes of the oracle values?
:::

Bérczi et al. study separating submodular partitions in [*$\{s,t\}$-Separating Principal Partition Sequence of Submodular Functions*, §1.1.1](https://arxiv.org/html/2510.25664v3). Their approximation guarantees and results for a variable number of parts leave this exact three-part question separate.

::: {.Problem #problem-ranked-submodular-3-partition title="Ranked enumeration of submodular 3-partitions"}
Let $f:2^U\to\mathbb R$ be submodular on a finite set $U$ with $|U|\ge3$, given by a value oracle. Give an unordered partition $\mathcal P$ of $U$ into three nonempty parts the value $\Phi_f(\mathcal P)=\sum_{P\in\mathcal P}f(P)$. Can every such partition be output exactly once in nondecreasing order of value, with ties in any order, so that for some polynomial $p$ and every $r$ up to the total number of outputs, producing the first $r$ partitions takes at most $(r+1)p(|U|)$ oracle calls and arithmetic and comparison operations? Termination must be certified within another $p(|U|)$ operations after the final output.
:::

A minimum three-partition can be found in polynomial time. Ranked enumeration for two parts is recorded in Hirayama et al., [*A polynomial time algorithm for finding a minimum 4-partition of a submodular function*, Lemma 5.2](https://chaoxu.prof/files/papers/sub4part-journal.pdf#page=10). The question asks for ranked enumeration of three-partitions.

## Constraint satisfaction {.unnumbered}

::: {.Problem #problem-s-tractability-dichotomy title="Dichotomy for surjective minimum violation"}
Fix a finite digraph $H=(D,F)$ with $D\ne\varnothing$, allowing loops. Given a finite digraph $G=(V,E)$ with $|V|\ge|D|$ and binary-encoded nonnegative integer arc weights $w$, minimize $\sum_{uv\in E:\,(\phi(u),\phi(v))\notin F}w(uv)$ over surjections $\phi:V\twoheadrightarrow D$. Characterize the patterns $H$ for which this problem is polynomial-time solvable. Does every fixed pattern give either a polynomial-time solvable problem or an NP-hard problem?
:::

Kawarabayashi and Xu formulate the classification question in [*Minimum violation vertex maps and their applications to cut problems*, §1 and §7](https://chaoxu.prof/files/papers/min-violation.pdf). Their results include a surjective dichotomy for reflexive trees, while the question here allows arbitrary finite digraphs.

## Matroids and delta-matroids {.unnumbered}

::: {.Problem #problem-gabow title="Gabow's serial symmetric exchange conjecture"}
Let $M$ be a finite matroid of rank $r$ with disjoint bases $B_1,B_2$. Are there orderings $B_1=\{x_1,\ldots,x_r\}$ and $B_2=\{y_1,\ldots,y_r\}$ such that, writing $X_i=\{x_1,\ldots,x_i\}$ and $Y_i=\{y_1,\ldots,y_i\}$ with $X_0=Y_0=\varnothing$, both $(B_1\setminus X_i)\cup Y_i$ and $(B_2\setminus Y_i)\cup X_i$ are bases for every $i\in\{0,\ldots,r\}$?
:::

Akrami et al. state the general conjecture in [*Matroids are Equitable*, Conjecture 1.14](https://link.springer.com/article/10.1007/s00493-026-00217-y). It is known for several restricted classes, including [split matroids](https://real.mtak.hu/236600/1/13537-PDFfile-58658-1-10-20260115.pdf) and [regular matroids](https://doi.org/10.1016/j.jctb.2025.10.009).

::: {.Problem #problem-even-delta-proximity title="Finite-forbidden-set proximity for even delta-matroids"}
Let $E$ be finite and let $\mathcal D\subseteq2^E$ be nonempty. Write $X\triangle Y$ for the symmetric difference of sets $X,Y$. Suppose the members of $\mathcal D$ all have the same cardinality parity and satisfy symmetric exchange: for $X,Y\in\mathcal D$ and $e\in X\triangle Y$, some $f\in(X\triangle Y)\setminus\{e\}$ satisfies $X\triangle\{e,f\}\in\mathcal D$. Such a pair $(E,\mathcal D)$ is an even delta-matroid.

Let $\Gamma$ be an abelian group, choose $\gamma_0,\gamma_e\in\Gamma$ and $c_0,c_e\in\mathbb R$ for $e\in E$, and define $\lambda(X)=\gamma_0+\sum_{e\in X}\gamma_e$ and $c(X)=c_0+\sum_{e\in X}c_e$. Let $F\subseteq\Gamma$ be finite, and suppose some $X\in\mathcal D$ satisfies $\lambda(X)\notin F$. Must every minimizer $A$ of $c$ over $\mathcal D$ admit a minimizer $B$ of $c$ over $\{X\in\mathcal D:\lambda(X)\notin F\}$ satisfying $\tfrac12|A\triangle B|\le|F|$?
:::

The corresponding proximity theorem for matroid bases appears in Liu et al., [*Proximity Theorem and FPT Algorithm for Minimum Group-labeled Matroid Bases*, Theorem 1](https://arxiv.org/html/2610.32798v1). The question here concerns arbitrary even delta-matroids.

## Matching {.unnumbered}

::: {.Problem #problem-exact-matching title="Deterministic Exact Matching"}
Let $G=(V,E)$ be a finite undirected graph, let $R\subseteq E$ be a designated set of red edges, and let $r$ be an integer. A perfect matching is a set of edges incident to each vertex exactly once. Is there a deterministic polynomial-time algorithm deciding whether $G$ has a perfect matching $M$ with $|M\cap R|=r$?
:::

A randomized polynomial-time algorithm is known. Sebő discusses the deterministic question for general graphs in [*Matchings: Source, Goal and Faithful Companion*, p. 6](https://arxiv.org/pdf/2602.03966v2#page=6).

::: {.Problem #problem-min-cost-exact-matching title="Minimum-cost Exact Matching"}
Let $G=(V,E)$ be a finite undirected graph, let $R\subseteq E$ be a designated set of red edges, and let $r$ be an integer. Each edge $e$ has a nonnegative integer cost $c(e)$ encoded in binary. A perfect matching is a set of edges incident to each vertex exactly once. Is there an algorithm running in time polynomial in the total input bit length that finds a perfect matching $M$ of minimum cost $\sum_{e\in M}c(e)$ subject to $|M\cap R|=r$, or reports that none exists? Randomized algorithms succeeding with probability at least $2/3$ on every input are allowed.
:::

Armbruster et al. describe the binary-cost optimization question as open even for bipartite graphs in [*Multiplicative Assignment with Upgrades*, §§5.2–5.3](https://drops.dagstuhl.de/storage/00lipics/lipics-vol374-icalp2026/LIPIcs.ICALP.2026.13/LIPIcs.ICALP.2026.13.pdf#page=16). Known randomized pseudopolynomial algorithms can depend on the numerical costs rather than their bit lengths.

## Allocation {.unnumbered}

::: {.Problem #problem-labelled-bin-filling title="Labelled-bin filling"}
There are $n\ge1$ bins and $m\ge1$ item types. The input consists of positive integer weights $w_1<\cdots<w_m$ with $w_1\mid\cdots\mid w_m$, nonnegative integer preferred counts $a_{ij}$, supplies $B_j$, and targets $h_i$, all encoded in binary. Find nonnegative integer counts $x_{ij}$ minimizing $\sum_{i=1}^n\sum_{j=1}^m|x_{ij}-a_{ij}|$ subject to
$$
\begin{aligned}
\sum_{j=1}^m w_jx_{ij}&=h_i &&(1\le i\le n),\\
\sum_{i=1}^n x_{ij}&\le B_j &&(1\le j\le m),
\end{aligned}
$$
or report that no feasible counts exist. Let $N$ be the total input bit length.

Is the problem polynomial-time solvable for every fixed $n\ge3$, including $n=3$ with $w_j=2^{j-1}$? More strongly, is there an algorithm running in $g(n)N^{O(1)}$ time for some computable function $g$, or is the decision problem with a binary-encoded cost threshold W[1]-hard parameterized by $n$? With $n$ and $m$ unrestricted, is there an exact algorithm running in $\operatorname{poly}(N,w_m)$ time?
:::

The underlying optimization model is stated in my [question on filling bins with minimum moves](https://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves). The bound $\operatorname{poly}(N,w_m)$ allows dependence on the largest weight's numerical value while keeping the dependence on counts, supplies, and targets polynomial in their bit lengths.

## Discrete geometry {.unnumbered}

::: {.Problem #problem-six-square-cover title="Covering a square by six unit squares"}
Can six closed squares of side length $1$ in the Euclidean plane cover an entire square of side length strictly greater than $2$? The unit squares may be translated and rotated independently, and overlaps are allowed.
:::

Four unit squares tile a square of side length $2$. Dósa et al. conjecture that six cannot cover a larger square in [*Covering a square by congruent squares*, Conjecture 1.1](https://arxiv.org/abs/2601.16535v3). Their Theorem 1.3 proves that five cannot, while seven suffice.
