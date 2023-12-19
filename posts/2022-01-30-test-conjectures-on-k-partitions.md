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

We want to show some $\mathcal{Y}'$ is a min $k$-partition, where each partition class of $\mathcal{Y}'$ has to be the union of some $Z_{i,j}$s. 

It doesn't matter if $Z_{i,j}$ has $100$ or $1$ vertices, only the emptyness matters. Consider a matrix $M_{i,j} = \max(1,|Z_{i,j}|)$. Such a matrix is called a _configuration_. 
Define two configurations are _isomorphic_, if one can be obtained by swapping rows and columns of the other. 

We are interested in enumerate the possible configurations up to isomorphism.

Find integer $\set{0,1}$ matrices and then modulo the action of a permutation group (that defines the isomorphism) can be done in Sage. There is a function that generates all integer vectors modulo a permutation group, and the vectors has length $\ell$ with elements in $\set{0,\ldots,n}$. See the reference on [Integer vectors modulo the action of a permutation group](https://doc.sagemath.org/html/en/reference/combinat/sage/combinat/integer_vectors_mod_permgroup.html). 
    
    IntegerVectorsModPermutationGroup(P, max_part=1)

Once we have the output, we filter the vectors to maintain the required properties, for example, has at least a $1$ on each row.

The input of the Sage function has to take the desired permutation group $P$. 

One might think we just take $S_{k-1}\times S_k$ to obtain $P$, where $S_k$ is the symmetric group of order $k$. Unfortunately, this is not correct. This gives you the action on the rows and columns, but what we really need, is what happens to each element in the matrix. 

The correct way is to use the wreath products. We want to consider $S_{k-1} \wr S_k$ intersect with $S_k \wr S_{k-1}$. However, we have to make sure the mapping of the elements are correct. The following is the code in SAGE, and the elements are numbers $1$ to $k(k-1)$. Note we made sure the mapping has the same labels through permutation `pi`.

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

At this point, we obtain all possible configurations. In particular, for $k=3,4,5,6$, the number of configurations are $13, 87, 1053, 28576$.

If we restrict to configurations where there is at least a $1$ in each row and at least a $1$ in each column, it cuts down the configurations to $5,42,633,20755$. We can further observe that for some configurations, it implies $\mathcal{X}\lhd \mathcal{Y}$ already, so we do not have to check such configurations.
Let's call configurations after the above filtering good configurations. The number of good configurations for $k=3,4,5,6$ are $3, 23, 353, 12828$. 

# Test if there exists a noncrossing $\mathcal{Y}'$ for a configuration $M$

We now have a configuration $M$, and now we are interested in creating a linear program to decide if there always exists a minimum $k$-partition $\mathcal{Y}'$ that is non-crossing with the minimum $(k-1)$-partition $\mathcal{X}$. For a given $M$, one can always recover $\mathcal{X}$ and $\mathcal{Y}$.

Let $V$ be the set of vertices, which equals to the number of $1$s in $M$. Let $P_k(V)$ be the set of $k$-partitions of $V$. For each $U\subseteq V$, we create a variable $x_U$. It represents the value of $f(U)$. We also create a variable $z$.

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

The first set of constraints are the submodular inequalities. The second shows $\mathcal{X}$ is a minimum $k-1$-partition, and the third shows $\mathcal{Y}$ is a minimum $k$-partition. We also set the value of the minimum $k$-partition to be $1$. Finally, we consider every $k$-partition that is non-crossing with $\mathcal{X}$, and $z$ is a lower bound of their value.

If the objective value $z=1$, then this means there is at least $1$ $k$-partition $\mathcal{Y}'$ that is non-crossing with $\mathcal{X}$ has the value same as the minimum $k$-partition. Therefore the conjecture is true if and only for every configuration $M$, the above linear program has optimum $1$.

# Redundant constraints

One can easily prove the conjecture for $k=3,4$ by directly using the above linear program over all good configurations. However, it runs into difficulty with $k=5$. This is because the linear program is way too large. The number of $k$ partitions for $n$ elements is the Stirling number of the second kind $\left\{{n\atop k}\right\}$.
$\left\{{20\atop 5}\right\} = 749206090500$ and $\left\{{20\atop 4}\right\} = 45232115901$. However, it seems many constraints are redundant due to symmetry. It be interesting to see if we can cut it down to a manageable size. If so, maybe the conjecture for $k=5$ or even $k=6$ might be solvable.

To do this, we consider the following definition. Consider a vector $s=(s_1,\ldots,s_k)$. We say a family of $k$-partitions $\mathcal{F}$ is $s$-complete, if $f(\mathcal{P})\geq f(\mathcal{Y})$ for all $\mathcal{P}\in \mathcal{C}$, then $f(\mathcal{P})\geq f(\mathcal{Y})$ for all $k$-partition $\mathcal{P}$. Here $\mathcal{Y}=\{Y_1,\ldots,Y_k\}$ is a $k$-partition such that $|Y_i|=s_i$. Let $\lambda(s)$ to be the size of the minimum $s$-complete family. So instead of considering all $k$-partitions for the constraints, we just have to consider the $s$-complete family for some $s$. If $\lambda(s)$ is very small, then there is hope to solve the problem quickly.

Let $t$ be a $k$-tuple consists of only $d$'s, then we define $\lambda_k(d) = \lambda(t)$. It would be very interesting to see how large $\lambda_k(d)$ is.

As an example, we show a simple result on $\lambda(a,b)$.

::: Theorem
$\lambda(a,b)\leq 2^a+2^b-3$.
:::
::: Proof
Indeed, let $\mathcal{Y}=(Y_1,Y_2)$ consists of $a$ and $b$ elements.

Consider the following family $\mathcal{F}$: For each non-empty set $S$ such that either $S\subsetneq Y_1$ or $S\subsetneq Y_2$, $(S,V-S)\in \mathcal{F}$. Also, let $\mathcal{Y}\in \mathcal{F}$.

So there are $2^a-2 + 2^b-2 + 1$ sets in $\mathcal{F}$.

Next, we show $\mathcal{F}$ is an $(a,b)$-complete family. Consider any $(X_1,X_2)$ not in $\mathcal{F}$ and consider the intersection with $(Y_1,Y_2)$. Let $Z_{i,j} = X_i\cap Y_j$. 

Let $|Z_{1,1}|=x$ and $|Z_{2,1}|=y$, then we have $|Z_{1,2}|=a-x$ and $|Z_{2,2}|=b-x$. Consider when all $Z_{i,j}$ is non-empty.

\begin{align*}
\sum_{i=1}^2 \sum_{j=1}^2 f(X_i)+f(Y_j) &= 2(f(X_1)+f(X_2)+f(Y_1)+f(Y_2))\\
                                        &\geq \sum_{i=1}^2 \sum_{j=1}^2 f(Z_{i,j}) + f(X_i\cup Y_j)\\
                                        &=\sum_{i=1}^2 \sum_{j=1}^2 f(Z_{i,j})+f(\bar{Z_{i,j}})\\
                                        &\geq 4(f(Y_1)+f(Y_2))
\end{align*}

Which shows $f(X_1)+f(X_2)\geq f(Y_1)+f(Y_2)$.

Otherwise, if some $Z_{i,j}$ is empty, then we have $(X_1,X_2)=(Y_1,Y_2)\in \mathcal{F}$.
:::

In particular, $\lambda_2(d)\leq 2^{d+1}$.

::: Conjecture
$\lambda_k(d) = O(k^d)$.
:::
