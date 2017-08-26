---
title: Small $L_1$ norm solution to a linear Diophantine equation 
tags: number theory
---

Let $a=(a_1,\ldots,a_n)$ be $n$ integers with $\gcd(a_1,\ldots,a_n)=1$. It is known that there exist a integral vector $x=(x_1,\ldots,x_n)$, such that $\sum_{i=1}^n x_ia_i = 1$.

It is well known that by [Bézout's lemma](https://en.wikipedia.org/wiki/Bézout%27s_identity), that in the case $n=2$, we can obtain that $\|x\|\leq \|a\|$. Here $\|\cdot \|$ is the $L_1$ norm. 

However, I could not a general bound of $\|x\|$. Here we prove that the bound on $\|x\|$ is true in general. Before that, we first introduce a lemma from [@Ford1996].

{Lemma}

    Let $a=(a_1,\ldots,a_n)$ be a vector of positive integers with at least $2$ elements, does not contain $1$ and $\gcd(a)=1$. If $g_k = \gcd(a_k,\ldots,a_n)$, then there exist a solution to 
    \[
    \sum_{i=1}^n x_i a_i = 1
    \] 
    such that for all $1\leq i\leq n-1$,
    \[
    |x_i|\leq \frac{g_{i+1}}{2g_i}
    \]
    and $|x_n|\leq \frac{\max(a_1,\ldots,a_{n-1})}{2}$.

{Theorem}
    Let $a$ be a vector of positive integers such that $\gcd(a)=1$, then there exist a integral solution to $x \cdot a=1$ such that $\|x\|$ is at most the sum of the smallest and the largest element in $a$.

{Proof}

    Let $a=(a_1,\ldots,a_n)$. Let $a_n=m=\min(a)$ and $M=\max(a)$. We can assume $1$ is not in $a$, otherwise we can find $x$ such that $\|x\|=1$. We define $g_i = \gcd(a_i,\ldots,a_n)$. Hence $g_n = m$. We consider a solution to $\sum_{i=1}^n x_ia_i = 1$ satisfies [Lemma 1]. 
    Let $I = \set{i | g_{i+1}\geq 2g_{i}, i\leq n-1}$. Using the fact $m\geq 2$, we have $|I|\leq \lfloor \log_2 m \rfloor \leq m/2$.

    Define $I_1 = \set{ i| i\in I, |x_i|=1}$, so $|x_i|\geq 2$ for all $i\in I\setminus I_1$.
    \[
    \sum_{i\in I\setminus I_1} |x_i| \leq \prod_{i\in I\setminus I_1} |x_i| \leq \prod_{i\in I} |x_i| \leq \prod_{i\in I} \frac{g_{i+1}}{2g_i} \leq \prod_{i\in I} \frac{g_{i+1}}{g_i}  \leq \prod_{i=1}^{n-1} \frac{g_{i+1}}{g_i} = \frac{g_n}{g_1} = g_n = m.
    \]

    Together this shows 
    \[\|x\| = \sum_{i=1}^n |x_i| = |x_n| + \sum_{i\in I_1} |x_i| + \sum_{i\in I\setminus I_1} |x_i| \leq \frac{M}{2} + \frac{m}{2} + m \leq m+M.\]

{Corollary}
    
    Let $a$ be a vector of integers such that $\gcd(a)=1$, then there exist a integral solution to $x \cdot a=1$ such that $\|x\|\leq \|a\|$.