---
title: Isotonic function preserving grid in $[0,1]$
tags: analysis
---

{Definition}
	A function $\varphi:X\to Y$ is isotonic if $\|x-y\| < \|w-z\| \implies \|\varphi(x)-\varphi(y)\| < \|\varphi(w)-\varphi(z)\|$ for all $x,y,z,w\in X$.

A sequence of points $x_0,x_1,\ldots,x_n$ is called $\delta$-grid if $\delta<\frac{1}{5n}$, $x_0=0$, $x_n=1$ and for all $1 \leq i\leq n-1$, we have $x_i-x_{i-1} \in (\frac{1}{n}-\frac{\delta}{2^{i-1}},\frac{1}{n}-\frac{\delta}{2^i})$. Note this imply $x_n-x_{n-1}>\frac{1}{n}$.

{Theorem}
	Let $X$ be a $\delta$-grid of $n$ points where $\delta<2^{-n}$. $\varphi: X\to [0,1]$ is a isotonic function such that $\varphi(0)=0$ and $\varphi(1)=1$, then $|x_i - \varphi(x_i)|<1/n$

{Proof}
	It's easy to see that $\varphi$ is a increasing function. Let the points in $X$ ordered as $x_0,x_1,\ldots,x_n$. 
	Let $l_i = |\varphi(x_i)-\varphi(x_{i-1})|$. Note that 

	1. 
	\[
		\sum_{i=1}^n l_i = 1
	\]

	2. 
	\[|x_i-x_{i-1}|<\frac{1}{n}-\frac{\delta}{2^i}<|x_{i+1}-x_i|\]
	thus by isotonic function 
	\[|\varphi(x_i)-\varphi(x_{i-1})|<|\varphi(x_{i+1})-\varphi(x_i)|\].
	This is just $l_i<l_{i+1}$.

	3. $\sum_{i=1}^{m} l_i > \sum_{i=n-(m-2)}^{n} l_i$ for all $m$, because 
	$\frac{m}{n}\geq |x_m-x_0| > \frac{m}{n}-\delta > \frac{m-1}{n}+2\delta>|x_n - x_{n-(m-2)}|$, thus 
	\[|\varphi(x_m)-\varphi(x_{0})|<|\varphi(x_{n})-\varphi(x_{n-(m-2)})|\].
	
	Combine the relations above, we have
	\[
		\frac{m}{n} \geq \sum_{i=1}^m l_i > \sum_{i=n-(m-2)}^{n} l_i \geq \frac{m-1}{n}
	\]

	
	But $\sum_{i=1}^m l_i = \varphi(x_m)$, so $m/n\geq x_m>m/n-\delta$, $m/n\geq \varphi(x_m)>(m-1)/n$. Since $\delta$ is small, we have $|x_i-\varphi(x_i)|<1/n$.