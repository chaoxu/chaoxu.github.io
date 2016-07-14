---
title: Sum over products of weighted subset of certain size
tags: algorithm
---

Consider a commutative semiring $(R,+,\cdot)$. $\mathbb{0}$ is the identity for $(R,+)$, and $\mathbb{1}$ is the identity for $(R,\cdot)$. 
Let $f,g:V\to R$, $w:V\to \mathbb{N}$ and $Z\subset \mathbb{N}$. It is common that we are interested in computing expressions of the following form.

\[
\sum_{S\subset V, \sum_{x\in S} w(x) \in Z} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x)
\]

Examples:

  1. If $w(x)=1$ for all $x$, and $f(x)$ be the probability that event $x$ occurs, $g=1-f$, we find the probability that the number of event occurs $t$ times, where $t\in Z$. In probability, this is computing the Poisson distribution.

  2. If $(R,+,\cdot) = (\N,+,\cdot)$, $f=g=1$, for all $x$ and $w(x)=x$ and $V\subset \N$ and $Z=\{t\}$, then we find the number of subsets that have element sum $t$.

  3. If $(R,+,\cdot) = (\N,\max,+)$, $V\subset \N$, $g=0$ and $Z=\{0,\ldots,W\}$, then this solves the knapsack problem with knapsack size $W$, value $f$ and cost $w$.

  4. An actual application inspired this post: An automated test suite that runs $n$ subtests, and it is allowed to rerun a subtest if it fails the first time. A subtest passes if first run passes or the rerun passes. The test is successful if all the subtests passes and the number of total reruns is at most $k$. Assume probability of passing is independent for each subtest. One want to estimate the probability of a successful test given the probability a run passes for a specific subtest.

Let $\max Z = k$ and $|V| = n$. The naive algorithm runs in $O(n2^n)$ time (assuming semiring operation takes $O(1)$ time). There is a common transformation that turns this problem that sum over all subsets to a problem that sums over $Z$. So it runs in $O(nk)$ time.

Let $V=\{v_1,\ldots,v_n\}$ and $V_j = \{v_1,\ldots,v_j\}$. Define 
 	\[
 		D(i,j) = \sum_{S\subset V_j, \sum_{x\in S} w(x) = i} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x)
	\].

Certainly, 
	\[
		\sum_{S\subset V, \sum_{x\in S} w(x) \in Z} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x) = \sum_{i\in Z} D(i,n)
	\]

We only incur a $O(k)$ number of semiring operations once we compute all $D(i,n)$ for $0\leq i\leq k$.

Let $[P]$ be the [Iverson bracket notation](http://en.wikipedia.org/wiki/Iverson_bracket), namely

\[
[P] = \begin{cases} \mathbb{1} & \text{if } P \text{ is true;}\\
      \mathbb{0} & \text{otherwise.} \end{cases}
\]

{Theorem}
     1. $D(i,0) = [i \neq 0]$

     2. For $j\geq 1$, $D(i,j) = [i\geq w(v_j)] f(v_j)D(i-w(v_j),j-1) + g(v_j) D(i,j-1)$.

{Proof}
	The base case can be verified easily, we show part of a inductive step.

	\begin{align*}
	f(v_j)D(i-w(v_j),j-1) + g(v_j)D(i,j-1) &= f(v_j) \sum_{S\subset V_{j-1}, \sum_{x\in S} w(x) = i-w(v_j)} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x)  +
	g(v_j) \sum_{S\subset V_{j-1}, \sum_{x\in S} w(x) = i)} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x) \\
	&=   \sum_{v_j\in S\subset V_j, \sum_{x\in S} w(x) = i} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x) + \sum_{v_j\not\in S\subset V_j, \sum_{x\in S} w(x) = i} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x)\\
	&= \sum_{S\subset V_j, \sum_{x\in S} w(x) = i} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x)
	\end{align*}

