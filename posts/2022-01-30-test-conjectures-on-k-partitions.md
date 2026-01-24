---
title: Test conjectures on $k$-partitions over submodular functions
tags: Conjectures, computational experiments, submodular, partition
---

This article considers tools one can use to quickly test conjectures. I work in combinatorial optimization, and often there are cases where one can phrase a problem as a conjecture over a finite domain. Testing conjectures using brute force quickly becomes infeasible due to combinatorial explosion. Here, we walk through an actual example and learn a few techniques. We will use [Sage](https://www.sagemath.org/) and any fast linear program solver, such as [Gurobi](https://www.gurobi.com/).

# A conjecture on $k$-partitions of a submodular function

A collection of $k$ non-empty, pairwise disjoint sets whose union is $V$ is called a $k$-partition of $V$.

Let $f:2^V\to \R$ be a submodular function. A **minimum $k$-partition** is a $k$-partition $\mathcal{X}$ of $V$ such that $\sum_{X\in \mathcal{X}} f(X)$ is minimized; that is, the sum of the function values over the parts is minimized.

A $k$-partition $\mathcal{X}$ and a $j$-partition $\mathcal{Y}$ are **noncrossing** if there exist $X\in \mathcal{X}$ and $Y\in \mathcal{Y}$ such that $X\subseteq Y$. We denote this relation by $\mathcal{X}\lhd\mathcal{Y}$.

::: Conjecture

Let $f:2^V\to \R$ be a submodular function with $|V|\geq k$. Let $\mathcal{X}$ be a minimum $(k-1)$-partition. Then there exists a minimum $k$-partition $\mathcal{Y}$ such that $\mathcal{X}\lhd \mathcal{Y}$.

:::

One way to prove the conjecture is to start with a minimum $k$-partition that does not have the desired property, and then use it to construct a new minimum $k$-partition that does. This requires introducing a few additional notions.

For two partitions $\mathcal{X}$ and $\mathcal{Y}$, define
\[
\mathcal{X}\sqcap \mathcal{Y} = \set{X\cap Y \mid X\in \mathcal{X},\, Y\in\mathcal{Y}}.
\]
That is, the new partition is obtained by intersecting every part of $\mathcal{X}$ with every part of $\mathcal{Y}$.

For two partitions $\mathcal{X}$ and $\mathcal{Y}$, write $\mathcal{X}\leq \mathcal{Y}$ if for each $X\in\mathcal{X}$ there exists $Y\in\mathcal{Y}$ such that $X\subseteq Y$. If $\mathcal{X}\leq \mathcal{Y}$, then every part of $\mathcal{Y}$ is the union of some parts of $\mathcal{X}$.

Now consider the following stronger conjecture, which implies the previous one.

::: Conjecture

Let $f:2^V\to \R$ be a submodular function with $|V|\geq k$. Let $\mathcal{X}$ be a minimum $(k-1)$-partition, and let $\mathcal{Y}$ be a minimum $k$-partition. Then there exists a minimum $k$-partition $\mathcal{Y}'$ such that $\mathcal{X}\lhd \mathcal{Y}'$ and $\mathcal{X}\sqcap \mathcal{Y} \leq \mathcal{Y}'$.

:::

We are interested in writing a program to test the stronger conjecture for small $k$.

One can first consider all possible ways $\mathcal{X}$ can intersect with $\mathcal{Y}$. A matrix encoding how $\mathcal{X}$ intersects with $\mathcal{Y}$ is called a **configuration**. For each configuration, we want to know: under that configuration, can we find the desired $\mathcal{Y}'$?

# Enumerate non-isomorphic configurations

Given $\mathcal{X} = \set{X_1,\ldots,X_{k-1}}$ and $\mathcal{Y} = \set{Y_1,\ldots,Y_{k}}$, let
\[
Z_{i,j} = X_i\cap Y_j,
\qquad
\mathcal{Z} = \{Z_{i,j}\} = \mathcal{X}\sqcap \mathcal{Y}.
\]

We want to show that some minimum $k$-partition $\mathcal{Y}'$ satisfies $\mathcal{Z}\leq \mathcal{Y}'$.

Observe that it does not matter whether $Z_{i,j}$ contains $100$ vertices or $1$ vertex -- only whether it is empty. Hence, we can consider a matrix $M$ where
\[
M_{i,j} = \max(1, |Z_{i,j}|).
\]
Such a matrix is called a **configuration** of $\mathcal{Z}$, and it encodes the information we care about. $\mathcal{X}$ and $\mathcal{Y}$ is called a realization of $M$. The goal is to prove the conjecture for each configuration. That is, all realizations of the configuration.

Two configurations are **isomorphic** if one can be obtained from the other by permuting rows and columns.

We are interested in enumerating possible configurations up to isomorphism.

This is the same as enumerating $\{0,1\}$-matrices modulo the action of a permutation group. This part can be done in Sage. There is a function that generates all integer vectors modulo a permutation group; the vectors have length $\ell$ with elements in $\set{0,\ldots,n}$. See the reference on [Integer vectors modulo the action of a permutation group](https://doc.sagemath.org/html/en/reference/combinat/sage/combinat/integer_vectors_mod_permgroup.html).

    IntegerVectorsModPermutationGroup(P, max_part=1)

Once we have the output, we filter it to obtain valid configurations. For example, a configuration must have at least one $1$ in each row.

The Sage function requires the permutation group $P$ defining the desired equivalence.

One might think we can take $S_{k-1}\times S_k$ to obtain $P$, where $S_k$ is the symmetric group on $k$ letters. Unfortunately, this is not correct: it describes the action on rows and columns, but we need the induced action on the *entries* of the matrix.

The correct way is to use wreath products. We want $(S_{k-1} \wr S_k)\cap (S_k \wr S_{k-1})$. However, we must ensure that the element labeling is consistent. The following is Sage code where the elements are labeled $1$ to $k(k-1)$. We ensure the same labeling via the permutation `pi`.

    a = SymmetricGroup(k-1)
    b = SymmetricGroup(k)
    
    matrix = [[k*x+y+1 for y in range(k-1)] for x in range(k)]
    pi = list(itertools.chain.from_iterable(map(list, zip(*matrix))))
    
    # we have to use SAGE to access GAP
    GG = gap.WreathProduct(gap(a),gap(b))
    HH = gap.WreathProduct(gap(b),gap(a))
    
    G = PermutationGroup(gap_group = GG.AsPermGroup())
    Hbad = PermutationGroup(gap_group = HH.AsPermGroup())
    # make sure the elements are labelled correctly.
    H = PermutationGroup([perm_replace(pi,g) for g in Hbad.gens()])
    
    # The correct permutation group
    P = H.intersection(G)

At this point, we obtain all matrices, some of which correspond to configurations. In particular, for $k=3,4,5,6$, the numbers of configurations are $13, 87, 1053, 28576$.

If we restrict to matrices with at least one $1$ in each row and at least one $1$ in each column, this reduces the counts to $5, 42, 633, 20755$. These are valid configurations.

We can further observe that for some configurations, we already have $\mathcal{X}\lhd \mathcal{Y}$, so we do not have to check them. Let us call the remaining configurations **good**. The numbers of good configurations for $k=3,4,5,6$ are $3, 23, 353, 12828$.

# Test if there exists a noncrossing $\mathcal{Y}'$ for a configuration $M$

Fix a configuration $M$. We now construct a linear program to decide whether there always exists a minimum $k$-partition $\mathcal{Y}'$ that is noncrossing with the minimum $(k-1)$-partition $\mathcal{X}$. For a given $M$, one can recover (a minimal) $\mathcal{X}$ and $\mathcal{Y}$ that realize $M$. It is easy to see that if our argument works for these particular $\mathcal{X}$ and $\mathcal{Y}$, it works for all $\mathcal{X}$ and $\mathcal{Y}$ realizing $M$.

Let $V$ be the vertex set, with $|V|$ equal to the number of $1$'s in $M$. Let $P_k(V)$ be the set of $k$-partitions of $V$. For each $U\subseteq V$, create a variable $x_U$ representing the value $f(U)$, and also create a variable $z$.

For a partition $\mathcal{S}$, write $x_\mathcal{S} = \sum_{S\in \mathcal{S}} x_S$.

Consider the following linear program:
\[
\begin{aligned}
& \max        & & z \\
& \text{s.t.} & & x_{S\cup \{a\}} + x_{S\cup \{b\}} \geq x_{S\cup \{a,b\}} + x_{S}
&& \forall S\subseteq V,\ a,b\in V\setminus S\\
&             & & x_\mathcal{X} \leq x_{\mathcal{X}'} && \forall \mathcal{X}'\in P_{k-1}(V)\\
&             & & x_\mathcal{Y} \leq x_{\mathcal{Y}'} && \forall \mathcal{Y}'\in P_{k}(V)\\
&             & & x_\mathcal{Y} = 1 \\
&             & & z \leq x_{\mathcal{Y}'} && \forall \mathcal{Y}'\in P_{k}(V)\ \text{with}\ \mathcal{X} \lhd \mathcal{Y}'.
\end{aligned}
\]

The first set of constraints are the submodular inequalities, which enforce that $x_S=f(S)$ for some submodular function $f$. The next constraints enforce that $\mathcal{X}$ is a minimum $(k-1)$-partition and $\mathcal{Y}$ is a minimum $k$-partition. We normalize the optimal $k$-partition value to be $1$. Finally, among all $k$-partitions that are noncrossing with $\mathcal{X}$, the variable $z$ is a lower bound on their objective values.

If the optimal value is $z=1$, then there exists at least one $k$-partition $\mathcal{Y}'$ that is noncrossing with $\mathcal{X}$ and has the same value as the minimum $k$-partition. Therefore, the conjecture holds if and only if for every configuration $M$, the above linear program has optimum $1$.

# Redundant constraints

One can easily prove the conjecture for $k=3,4$ by directly solving the above linear program over all good configurations. However, it becomes difficult for $k=5$ because the linear program is far too large. The number of $k$-partitions of an $n$-element set is the Stirling number of the second kind $\left\{{n\atop k}\right\}$:
\[
\left\{{20\atop 5}\right\} = 749206090500,
\qquad
\left\{{20\atop 4}\right\} = 45232115901.
\]
It seems many constraints are redundant due to symmetry, and it would be interesting to see if we can reduce the formulation to a manageable size. If so, the conjecture for $k=5$, or even $k=6$, might become solvable.

To do this, consider the following definition. For a vector $s=(s_1,\ldots,s_k)$, say that a family of $k$-partitions $\mathfrak{F}$ is **$s$-complete** if
\[
f(\mathcal{P})\geq f(\mathcal{Y})\ \forall \mathcal{P}\in \mathfrak{F}
\quad\Longrightarrow\quad
f(\mathcal{P})\geq f(\mathcal{Y})\ \forall \mathcal{P}\in P_k(V),
\]
where $\mathcal{Y}=\{Y_1,\ldots,Y_k\}$ is a $k$-partition with $|Y_i|=s_i$. By the previous discussion, we only need to consider the case $\sum_i s_i \leq k(k-1)$.

Let $\lambda(s)$ be the minimum size of an $s$-complete family. Instead of including constraints for all $k$-partitions, it suffices to include constraints for an $s$-complete family (for the relevant $s$). If $\lambda(s)$ is small for all relevant $s$, there is hope to solve the problem quickly.

As an example, we show a simple bound on $\lambda(a,b)$.

::: Theorem
$\lambda(a,b)\leq 2^a+2^b-3$.
:::

::: Proof
Let $\mathcal{Y}=(Y_1,Y_2)$ consist of $a$ and $b$ elements, respectively.

Consider the following family $\mathfrak{F}$: for each non-empty set $S$ such that either $S\subsetneq Y_1$ or $S\subsetneq Y_2$, include the partition $(S,V\setminus S)$ in $\mathfrak{F}$. Also include $\mathcal{Y}$ itself in $\mathfrak{F}$.

Thus, $|\mathfrak{F}|=(2^a-2)+(2^b-2)+1=2^a+2^b-3$.

Next, we show that $\mathfrak{F}$ is $(a,b)$-complete. Consider any $(X_1,X_2)\notin \mathfrak{F}$ and its intersection with $(Y_1,Y_2)$. Let $Z_{i,j}=X_i\cap Y_j$.

Let $|Z_{1,1}|=x$ and $|Z_{2,1}|=y$. Then $|Z_{1,2}|=a-x$ and $|Z_{2,2}|=b-y$. Consider the case where all $Z_{i,j}$ are non-empty. Then
\begin{align*}
\sum_{i=1}^2 \sum_{j=1}^2 \bigl(f(X_i)+f(Y_j)\bigr)
  &= 2\bigl(f(X_1)+f(X_2)+f(Y_1)+f(Y_2)\bigr)\\
  &\ge \sum_{i=1}^2 \sum_{j=1}^2 \bigl(f(Z_{i,j}) + f(X_i\cup Y_j)\bigr)\\
  &= \sum_{i=1}^2 \sum_{j=1}^2 \bigl(f(Z_{i,j}) + f(\overline{Z_{i,j}})\bigr)\\
  &\ge 4\bigl(f(Y_1)+f(Y_2)\bigr),
\end{align*}
which implies $f(X_1)+f(X_2)\ge f(Y_1)+f(Y_2)$.

Otherwise, if some $Z_{i,j}$ is empty, then necessarily $(X_1,X_2)=(Y_1,Y_2)\in \mathfrak{F}$.
:::

Currently, I do not know much about $\lambda(s)$ in general, so let us look at a special case. Define
\[
\lambda_k(d) = \lambda((d,\ldots,d)),
\]
where $d$ is repeated $k$ times. It would be very interesting to understand how large $\lambda_k(d)$ is, especially $\lambda_5(4)$. If $\lambda_5(4)$ is small, then there is hope of solving the conjecture for $k=5$.

In particular, because we see $\lambda_2(d)\leq 2^{d+1}$, I would conjecture $\lambda_k(d) = O(k^d)$.