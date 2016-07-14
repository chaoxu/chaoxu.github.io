---
title: Proof that binomial coefficients are integers 
tags: math
---

[Eagle Fantasy](http://www.eaglefantasy.com/) asked how to prove that binomial coefficients are integers without induction or counting. He seek a purely number theoretical proof.

One common way to prove a number is a integer is to show it has a integer factorization. Thus one try to demonstrate that ${n \choose k}$ has such factorization.

Let's start by figure out what is the factorization of $n!$. Consider any prime $p$. Certainly all numbers from $1$ to $n$ that is divisible by $p$ contribute to the exponent by $1$, numbers that divisible by $p^2$ contribute to the exponent by $1$, as we already counted one of the $p$ factor already. Similarly, one apply this for all $p^i$ and result $p^{\sum_{i=1}^\infty \lfloor \frac{n}{p^i} \rfloor} | n!$, and by unique factorization theorem, we get that 
\[
n! =\prod_{k=1}^\infty p_k^{\sum_{i=1}^\infty \lfloor \frac{n}{p_k^i} \rfloor} 
\]
where $p_k$ is the $k$-th prime.

Of course, since we are going to manipulate the sum and the product, it is better to replace $\infty$ to some large $N$ so we don't need to define what we mean by infinite product or why we can switch the order of summation, etc.

Now consider the following inequality, left as an exercise to the reader $\lfloor a+b\rfloor \geq \lfloor a \rfloor + \lfloor b\rfloor$. Then everything follows by easy algebraic manipulation.
\begin{align*}
{n\choose k} &= \frac{n!}{(n-k)!k!}\\
&= \frac{\prod_{k=1}^N p_k^{\sum_{i=1}^N \lfloor \frac{n}{p_k^i} \rfloor}}{\prod_{k=1}^N p_k^{\sum_{i=1}^N \lfloor \frac{n-k}{p_k^i} \rfloor} \prod_{k=1}^N p_k^{\sum_{i=1}^N \lfloor \frac{k}{p_k^i} \rfloor} }\\
&= \prod_{k=1}^N p_k^{\sum_{i=1}^N \lfloor \frac{n}{p_k^i} \rfloor - \lfloor \frac{n-k}{p_k^i} \rfloor - \lfloor \frac{k}{p_k^i} \rfloor}
\end{align*}
Using the previous inequality, we see every exponent is a integer greater or equal to $0$. This shows ${n\choose k}$ is a integer.

{Remark}
    This also derive the following result: $k!|(m+1)\ldots (m+k)$ for all $m\geq 0$. One can use this result to prove that multinomial coefficients are integers.