---
title: Divide and conquer over cyclic groups
---

Recently we found a divide and conquer algorithm over $\Z_m$. We have an efficient algorithm for the case where all the elements $S$ are in $\Z_m^*$, the set of units. 

Our divide and conquer algorithm basically partition the numbers by throwing them into different subgroups. Assume $m$ has distinct prime factors $p_1 < \ldots < p_k$, and for simplicity, define $p_0=1$. We partition $S$ to $S_0,\ldots,S_k$, where $S_i$ contain all the elements that's divisible by $p_i$ but not $p_j$ for any $j>i$. We recursively apply our algorithm to each $S_i/p_i$ in $\Z_{m/p_i}$ and combine the solution.

This gives us a recurrence relation, and the crucial part of the recurrence involves the following function.

Let $p_i$ be the $i$th smallest prime number, and $p_0$ defined as $1$. If we know that $f_0(x) = x$ and $f_k(x) = \sum_{i=0}^k f_i(x/p_i)$, then we can show that 
\[
f_k(x) = x \prod_{i=1}^{k} \left(1+\frac{1}{p_j-1}\right)
\]
by induction.

First, we would need a small lemma.

{Lemma}

	Let $a_1,\ldots,a_k$ be real numbers such that non of them is $0$ or $1$, then
	\[
	1+\sum_{j=1}^k \frac{1}{p_j} \prod_{i=1}^j \left(1+\frac{1}{p_i -1}\right) = \prod_{j=1}^k \left(1+\frac{1}{p_j-1}\right)
	\]

{Proof}

	Proof by induction, basically $\frac{1}{x} (1+\frac{1}{x-1})=\frac{1}{x-1}$ for any $x\neq 0,1$, so this is true when $k=1$.

	\begin{align*}
	1+\sum_{j=1}^k \frac{1}{p_j} \prod_{i=1}^j \left(1+\frac{1}{p_i -1}\right) &= \prod_{j=1}^{k-1}\left(1+\frac{1}{p_j-1}\right) + \frac{1}{p_k}\prod_{j=1}^{k}\left(1+\frac{1}{p_j-1}\right)\\
	&= \left(1+\frac{1}{p_k} \left(1 + \frac{1}{p_{k-1}}\right) \right)\prod_{j=1}^{k-1}\left(1+\frac{1}{p_j-1}\right)\\
	&= \left(1+\frac{1}{p_k -1}\right)\prod_{j=1}^{k-1}\left(1+\frac{1}{p_j-1}\right)\\
	\end{align*}

{Theorem}
	
	\[
		f_k(x) = x \prod_{i=1}^{k} \left(1+\frac{1}{p_i-1}\right)
	\]

{Proof}
	\begin{align*}
		f_k(x)            &= \sum_{i=0}^k f_i(x/p_i)\\
		f_k(x)-f_k(x/p_i) &= \sum_{i=0}^{k-1} f_i(x/p_i)\\
		                  &= x \sum_{i=0}^{k-1} \frac{1}{p_i} \prod_{j=1}^i \left(1+\frac{1}{p_j-1}\right)\\
		                  &= x \left(1 + \sum_{i=1}^{k-1} \frac{1}{p_i} \prod_{j=1}^i \left(1+\frac{1}{p_j-1}\right)\right)\\
		                  &= x \prod_{i=1}^{k-1} \left(1+\frac{1}{p_i-1}\right)\\
	\end{align*}

	We can substitute $f_k(x) = x \prod_{i=1}^{k} \left(1+\frac{1}{p_i-1}\right)$, and see the result matches.

	\begin{align*}
		x\left(1-\frac{1}{p_i}\right) \prod_{i=1}^{k} \left(1+\frac{1}{p_i-1}\right) = x \prod_{i=1}^{k-1} \left(1+\frac{1}{p_i-1}\right)
	\end{align*}

It's also useful to bound $f_k(x)$.

{Theorem}
	
	\[
		f_k(x) = O(x \log k)
	\]

{Proof}
	
	\begin{align*}
		f_k(x) &= x \prod_{i=1}^{k} \left(1+\frac{1}{p_i-1}\right)\\
		       &\leq x \exp \left( \sum_{i=1}^k \frac{1}{p_i-1}\right)\\
		       &\leq x \exp \left( 1 + \sum_{i=1}^k \frac{1}{p_i}\right)\\
		       &\leq x \exp \left( \log \log (k \log k) + A \right)\\
		       &= O(x \log k)
	\end{align*}

	It uses the facts on [sum of reciprocals of the primes](https://en.wikipedia.org/wiki/Divergence_of_the_sum_of_the_reciprocals_of_the_primes).

Apply this to the actual algorithm running time analysis, we would get a $O(\log \log m)$ blow up of the running time for the $\Z_m^*$ algorithm.