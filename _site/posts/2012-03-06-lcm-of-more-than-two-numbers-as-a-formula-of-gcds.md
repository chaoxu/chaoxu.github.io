---
title: $\lcm$ of more than two numbers as a formula of $\gcd$s
tags: number theory
---

It is a common elementary number theory exercise to prove that $\lcm(a,b) = \frac{ab}{\gcd(a,b)}$.

A student might ask what is the $\lcm$ of three numbers. Some might think that
\[
\lcm(a,b,c) = \frac{abc}{\gcd(a,b,c)}
\]
It isn't. 

Still, one might want a formula for the $\lcm$ of three numbers. Of course one can say $\lcm(a,\lcm(b,c))$. In fact this is the common algorithm for computation. Are they ways to relate $\lcm$ and $\gcd$ without nesting those functions together?

Yes, but the formula is not so pretty.
\[
\lcm(a,b,c) = \frac{abc \gcd(a,b,c)}{\gcd(a,b)\gcd(b,c)\gcd(a,c)}
\]

This article shows how we can prove this result, and easily infer a more general theorem.
First, we see there is a group isomorphism from the naturals to it's prime factors $f:\mathbb{N}->\mathbb{N}^\infty$, $f(p_1^{e_1} \ldots p_n^{e^n}) = (e_1,\ldots,e_n,0,0,\ldots)$, where $p_n$ is the $n$th prime.

It's easy to show
\begin{align*}
\lcm(a_1,a_2,\ldots,a_n) &= f^{-1} (\max(f(a_1),\ldots,f(a_n)))\\
\gcd(a_1,a_2,\ldots,a_n) &= f^{-1} (\min(f(a_1),\ldots,f(a_n)))
\end{align*}
where $\max$ and $\min$ are defined coordinate-wise. In fact we only need to concern with one single coordinate. So the problem become proving
\[
\max(a,b,c) = a+b+c+\min(a,b,c)-(\min(a,b)+\min(b,c)+\min(a,c))
\], then the formula for $\lcm$ of 3 numbers holds.

This look familiar to the inclusion-exclusion principle, and certainly we can use it to prove it and generalize! Let $\mu$ be the Lebesgue measure, then for a finite sequence of non-negative reals $\{a_i\}$,
\[\max(a_1,\ldots,a_n) = \mu(\bigcup_{i=1}^n [0,a_i]).\]
It's just some standard arguments to show $\max$ does have the inclusion-exclusion structure. It generalize to allow negative reals by simply add a large enough constant to make them positive, and subtract the constant from the result. Formulas for $\min,\gcd,\lcm$ follows similarly.