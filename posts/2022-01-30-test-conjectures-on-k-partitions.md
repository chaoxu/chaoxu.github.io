---
title: Test conjectures on $k$-partitions over submodular functions
tags: Conjectures, computational experiments, submodular, partition
---

This article we consider tools one can use to quickly test conjectures. Testing conjectures quickly run into realm of infeasibility due to the combinatorial explosion. We look through an actual example and learn a few techniques. We use [Sage](https://www.sagemath.org/) and any fast linear program solver, like [Gurobi](https://www.gurobi.com/). 

# A conjecture on $k$-partitions of submodular function

A set of $k$ non-empty and disjoint sets that partitions $V$ is called a $k$-partition.
Let $f:2^V\to \R$ be a submodular function, a minimum $k$-partition is a $k$-partition $\mathcal{X}$ of $V$ such that $\sum_{X\in \mathcal{X}} f(X)$ is minimized.

A $k$-partition $\mathcal{X}$ and a $j$-partition $\mathcal{Y}$ is noncrossing, if $X\subseteq Y$ for some $X\in \mathcal{X}$ and $Y\in \mathcal{Y}$. We denote it $\mathcal{X}\lhd\mathcal{Y}$.

For two partitions, $\mathcal{X}\sqcap \mathcal{Y} = \set{ X\cap Y | X\in \mathcal{X}, Y\in\mathcal{Y}}$.

For two partitions $\mathcal{X}$ and $\mathcal{Y}$, $\mathcal{X}\leq \mathcal{Y}$ if for each $X\in\mathcal{X}$, $X\subseteq Y$ for some $Y\in\mathcal{Y}$.

::: Conjecture

  $f:2^V\to \R$ is a submodular function with $|V|\geq k$. Let $\mathcal{X}$ be a minimum $k-1$-partition and $\mathcal{Y}$ be a minimum $k$-partition. There exists a minimum $k$-partition $\mathcal{Y}'$ such that $\mathcal{X}\lhd \mathcal{Y}'$ and $\mathcal{X}\sqcap \mathcal{Y} \leq \mathcal{Y}'$.

:::

We are interested in writing a program to test the conjecture for small $k$.
The idea is to find different ways $\mathcal{X}$ can intersect with $\mathcal{Y}$, a matrix that encodes such information are called configurations. For each configuration, we want to know under such configuration, can we find the desired $\mathcal{Y}'$.

# Enumerate non-isomorphic configurations

Given $\mathcal{X} = \set{X_1,\ldots,X_{k-1}}$ and $\mathcal{Y} = \set{Y_1,\ldots,Y_{k}}$. Let $Z_{i,j} = X_i\cap Y_j$.

We want to show some $\mathcal{Y}'$ is a min $k$-partition, where each $Z_{i,j}$ is a subset. In particular, each partition class of $\mathcal{Y}$ has to be the union of some $Z_{i,j}$s. 

What matters is if $Z_{i,j}$ is empty or not. It doesn't matter if $Z_{i,j}$ has $100$ or $1$ vertices. Consider a matrix $M_{i,j} = \max(1,|Z_{i,j}|)$. Such a matrix is called a _configuration_. 
Define two configurations are isomorphic, if one can be obtained by swapping rows and columns of the other. 

We are interested in enumerate the possible configurations up to isomorphism.

Find integer $\set{0,1}$ matrices and then modulo the action of a permutation group (that defines the isomorphism) can be done in Sage. There is a function that generates all integer vectors modulo a permutation group, and the vectors has length $\ell$ with elements in $\set{0,\ldots,n}$. See the reference on [Integer vectors modulo the action of a permutation group](https://doc.sagemath.org/html/en/reference/combinat/sage/combinat/integer_vectors_mod_permgroup.html). 
    
    IntegerVectorsModPermutationGroup(P, max_part=1)

Once we have the output, we filter the vectors to maintain the required properties.

The input of the sage function, however, has to take the desired permutation group $P$. 

One might think we just take $S_{k-1}\times S_k$ to obtain $P$. Unfortunately, this is not correct. This gives you the action on the rows and columns, but what we really need, is what happens to each element in the matrix. 

The correct way is to use the wreath products. We want $S_{k-1} \wr S_k$ intersect with $S_k \wr S_{k-1}$. However, we have to make sure the mapping of the elements are correct. The following is the code in Sage, and the elements are numbers $1$ to $k(k-1)$.

    a = SymmetricGroup(k-1)
    b = SymmetricGroup(k)
    
    matrix = [[k*x+y+1 for y in range(k-1)] for x in range(k)]
    pi = list(itertools.chain.from_iterable(map(list, zip(*matrix))))
    
    GG = gap.WreathProduct(gap(a),gap(b))
    HH = gap.WreathProduct(gap(b),gap(a))
    
    G = PermutationGroup(gap_group = GG.AsPermGroup())
    Hbad = PermutationGroup(gap_group = HH.AsPermGroup())
    H = PermutationGroup([perm_replace(pi,g) for g in Hbad.gens()])
    
    P = H.intersection(G)

At this point, we obtain all possible configurations. In particular, for $k=3,4,5,6$, the number of configurations are $13, 87, 1053, 28576$.

If we restrict to configurations where there is at least a $1$ in each row and at least a $1$ in each column, it cuts down the configurations to $5,42,633,20755$.
We can further observe that for some configurations, it implies $\mathcal{X}\lhd \mathcal{Y}$ already, so we do not have to check such configurations.
Let's call configurations after the above filtering good configurations. The number of good configurations for $k=3,4,5,6$ are $3, 23, 353, 12828$. 

# Test if there is a noncrossing $\mathcal{Y}'$ for a configuration $M$

We now have a configuration $M$, and now we are interested in creating a linear program to solve it. For a given $M$, we can use it to recover $\mathcal{X}$ and $\mathcal{Y}$.

Let $V$ be the set of vertices, which equals to the index of $1$s in $M$. Let $P_k(V)$ be the set of $k$ partitions of $V$. For each $U\subseteq V$, we create a variable $x_U$. It represents the value of $f(x_U)$.
We also create a variable $z$.

For a partition $\mathcal{S}$, $x_\mathcal{S}$ is just $\sum_{S\in \mathcal{S}} x_S$.

Consider the following linear program. 

\[
\begin{aligned}
& \max        & & z &\\
& \text{s.t.} & & x_{S\cup \{a\}} + x_{S\cup \{b\}} \geq x_{S\cup \{a,b\}} + x_{S} & \forall S\subseteq V, a,b\in V\setminus S\\
&             & & x_\mathcal{X} \leq x_{\mathcal{X}'} & \forall \mathcal{X}'\in P_{k-1}(V)\\
&             & & x_\mathcal{Y} \leq x_{\mathcal{Y}'} & \forall \mathcal{Y}'\in P_{k}(V)\\
&             & & x_\mathcal{Y} = 1 & \\
&             & & z \leq x_{\mathcal{Y}'} & \forall \mathcal{Y}'\in P_{k}(V), \mathcal{X} \lhd \mathcal{Y}'\\
\end{aligned}
\]

If the objective value $z=1$, then this shows there exists a $\mathcal{Y}'$ satisfies the requirement of the conjecture. 

# Results

Using the above, it can easily prove the result for $k=3,4$, and it runs into difficulty with $k=5$, but seems to be still in the realm of possbility for a single computer and more optimized code. $k=6$ is still impossible unless we get a huge boost in computational power. 