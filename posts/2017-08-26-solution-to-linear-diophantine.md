---
title: Small $L_1$ norm solution to a linear Diophantine equation 
tags: number theory
---

Let $a=(a_1,\ldots,a_n)$ be $n$ integers with $\gcd(a_1,\ldots,a_n)=1$. There exists a integral vector $x=(x_1,\ldots,x_n)$, such that $\sum_{i=1}^n x_ia_i = 1$. How large is the solution $x$? By [Bézout's lemma](https://en.wikipedia.org/wiki/Bézout%27s_identity), when $n=2$, we can obtain that $\|x\|\leq \|a\|$. Here $\|\cdot \|$ is the $L_1$ norm. 

However, I could not find a general bound of $\|x\|$ anywhere. Here we prove that the bound on $\|x\|$ is true in general. Before that, we first introduce a lemma from [@Ford1996].

{Lemma}

    Let $a=(a_1,\ldots,a_n)$ be a vector of positive integers with at least $2$ elements, it does not contain $1$ and $\gcd(a)=1$. If $g_k = \gcd(a_k,\ldots,a_n)$, then there exist a solution to 
    \[
    \sum_{i=1}^n x_i a_i = 1
    \] 
    such that for all $1\leq i\leq n-1$,
    \[
    |x_i|\leq \frac{g_{i+1}}{2g_i}
    \]
    and $|x_n|\leq \frac{\max(a_1,\ldots,a_{n-1})}{2}$.

{Theorem}
    Let $a$ be a vector of positive integers such that $\gcd(a)=1$, then there exists a integral solution to $x \cdot a=1$ such that $\|x\|\leq \frac{1}{2}(\min(a)+\max(a))$.

{Proof}

    Let $a=(a_1,\ldots,a_n)$. Let $a_n=\min(a)$. We can assume $1$ is not in $a$, otherwise we can find $x$ such that $\|x\|=1$. Let $g_i = \gcd(a_i,\ldots,a_n)$. Hence $g_n = \min(a)$. We consider a solution to $\sum_{i=1}^n x_ia_i = 1$ satisfies [Lemma 1]. 
    Let $I = \set{i | g_{i+1}\geq 2g_{i}, i\leq n-1}$ and $j=\min(I)$. 
    One can algebraically check that $a/b\leq a-b$ holds if both $a\geq 2b$ and $b\geq 2$. In particular, we have $\frac{g_{i+1}}{g_i} \leq g_{i+1}-g_i$ for all $i\in I\setminus \set{j}$.
    \[
    \sum_{i\in I} |x_i| \leq \frac{1}{2} \sum_{i\in I} \frac{g_{i+1}}{g_i} \leq \frac{g_{j+1}}{2} + \frac{1}{2} \sum_{i\in I, i\neq j} g_{i+1} - g_i \leq \frac{g_{j+1}}{2} + \frac{1}{2} \sum_{i=j+1}^{n-1} g_{i+1}-g_i = \frac{1}{2}g_n = \frac{\min(a)}{2}.
    \]

    \[
        \|x\| = \sum_{i=1}^n |x_i| = |x_n| + \sum_{i\in I} |x_i| \leq \frac{\min(a)+\max(a)}{2}
    \]

{Corollary}
    
    Let $a$ be a vector of integers such that $\gcd(a)=1$, then there exists a integral solution to $x \cdot a=1$ such that $\|x\|\leq \|a\|$.