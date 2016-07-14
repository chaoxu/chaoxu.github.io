---
title: Minimum of submodular function over family of subsets
tags: submodular
---

{Theorem}
	Let $L$ and $L'$ are two lattices. If $f:L \to \R$ is a submodular function and $P:L'\to 2^{L}$ is a function with the property that if $X\in P(A)$ and $Y\in P(B)$, then $X\wedge Y\in P(A\wedge B)$ and $X\vee Y\in P(A\vee B)$. $f_P:L'\to \R$ defined as 
	\[
		f_P(X) = \min_{Y\in P(X)} f(Y)\\
	\]
	is submodular. 

{Proof}
	Let $X^* = \argmin_{Y\in P(X)} f(Y)$,
	note since $X^*\in P(X)$ and $Y^*\in P(Y)$, we have $X^*\vee Y^* \in P(X\vee Y)$ and $X^*\vee Y^* \in P(X\wedge Y)$.
	\begin{align*}
	f_P(X) + f_P(Y) &= f(X^*) + f(Y^*)\\
	                &\geq f(X^* \vee Y^*) + f(X^*\wedge Y^*)\\
	                &\geq f((X\vee Y)^*) + f((X\wedge Y)^*)\\
	                &= f_P(X\vee Y) + f_P(X\wedge Y)
	\end{align*}

This is quite useful, for starters, it proves that we can create a monotone submodular function from any submodular function.

{Theorem}
	
	Let $f:2^V\to \R$ be a submodular function, then $f_*,f^*:2^V\to \R$ defined as 
	\[
		f_*(X) = \min \{f(Y)|Y\subset X\}\\
		f^*(X) = \min \{f(Y)|X\subset Y\}
	\] 
	are monotone and submodular.

A practical application is to generalize the cut function. Consider for a directed graph graph $G=(V,E)$. We would define $\delta^+(A)$ to be the set of out going edges from $A$ to $V\setminus A$. $f=|\delta^+|$ is a submodular function. An alternate definition for $f$ is the minimum number of edges to be removed so there is no path from $A$ to $V\setminus A$.

A simple generalization is when we only care about $T\subset V$. We can define $f_T(A)$ to be the minimum number of edges to be removed so there is no path from $A$ to $T\setminus A$. Amazingly(or not not surprisingly, depending on your intuition), $f_T$ is also a submodular function by invoking the next theorem, which is a direct corollary of our first theorem.

{Theorem}
	Let $f:2^V\to \R$ be a submodular function, then $f_T:2^T\to \R$ defined as 
	\[
		f_T(X) = \min \{f(Y)|Y\subset X, T\setminus X\subset V\setminus Y\}\\
	\] 
	is submodular.