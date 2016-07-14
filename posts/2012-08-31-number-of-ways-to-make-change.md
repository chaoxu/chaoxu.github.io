---
title: Number of ways to make change
tags: enumerative combinatorics, math
---

A famous dynamic programming problem ask one to find how many ways to make change of a certain value. Formally, a program that take input $d_1,\ldots,d_m$ and $n$, and output 
\[
 \left|\{ (c_1,\ldots,c_m) | c_i\in \N , \sum_{i=1}^m c_i d_i = n\} \right|.
\]

The dynamic programming solution can solve this problem in $O(nm)$ time and $O(\min(n,\max(d_1,\ldots,d_m))$ space. 

Is it efficient? It's a very efficient pseudo-polynomial time algorithm. 

However if we fixed the denominations, this runs in $O(n)$ time, and it is no longer the fastest algorithm. Since if denominations are fixed, we can find the solution in $O(1)$ time (assuming addition and multiplication of integers can be done in constant time.)

So we want to come up with a closed formula by generating functions.

\[
C(x) = \prod_{i=1}^m \sum_{j=0}^\infty x^{j d_i}
\]

The coefficient for $x^n$ is our solution. Let $l = \lcm(d_1,\ldots,d_m)$.

\begin{align*}
C(x) &= \prod_{i=1}^m \sum_{j=0}^\infty x^{j d_i}\\
&= \prod_{i=1}^m (1-x^{d_i})^{-1}\\
&= \left( \prod_{i=1}^m \sum_{j=0}^{l/d_i - 1} x^{j d_i} \right)(1-x^l)^{-m}\\
&= \left( \prod_{i=1}^m \sum_{j=0}^{l/d_i - 1} x^{j d_i} \right)
\sum_{k=0}^\infty { k+m-1 \choose m-1  } x^{lk}\\
\end{align*}

Now, notice the first part can be precomputed as some polynomial with finite degree. Let it be $P(x)$, then we get that we need to find the coefficient of $x^n$ in the following expression.
\[
\sum_{k=0}^\infty P(x) { k+m-1 \choose m-1  } x^{lk}
\]

This is of course easy as we only need to test $k$'s where $n-lk \leq \deg(P)$. There are only constant number of them. 

Also notice those binomial coefficient need at most $m-1$ multiplications.
Thus we can find the solution in $O(1)$ time. 