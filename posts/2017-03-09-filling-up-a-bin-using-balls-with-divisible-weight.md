---
title: Filling up a bin using balls with divisible weights
tags: optimization, integer
---

This post shows how the special case for [this problem](http://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves) where there is exactly one bin, and each ball have weight a power of $2$. It is one of the [most popular unanswered problem on cs.stackexchange](http://cs.stackexchange.com/unanswered) as of writing.

{Problem}

    We are interested in solving the following integer program, 
    \begin{align*}
    \text{Minimize:} & \sum_{i=1}^n |x_i-a_i| \\
    \text{subject to:} & \sum_{i=1}^n w_i x_i = c\\
    & 0\leq x_i \leq b_i \text{ for all } 1\leq i \leq n\\
    \end{align*}

    where each $w_i$ is a power of $2$ for all $1\leq i\leq n$. Assume $w_i\leq w_{i+1}$.

In fact, we don't require power of $2$s, we can still establish polynomial time as long as $w_{i+1}/w_i$ is bounded by a polynomial in terms of the input size for all $i$. However, for simplicity of exposition, assume it's power of $2$. If $w_{i+1}/w_i$ it is not unbounded, then we are not sure.

Let's consider a more natural problem without the absolute values.

{Problem}($0$-$1$ exact knapsack problem with divisible weights)
    We are interested in solving the following integer program, 
    \begin{align*}
    \text{Minimize:} & \sum_{i=1}^n c_i x_i \\
    \text{subject to:} & \sum_{i=1}^n w_i x_i = t\\
    & x_i \in \{0,1\} \text{ for all } 1\leq i \leq n\\
    \end{align*}

    where $w_i|w_{i+1}$ for all $1\leq i\leq n$. $w_i$ can be negative.


There are two things we have to show. We must show [Problem 1] reduces to [Problem 2] with polynomial blow up, and [Problem 2] can be solved in polynomial time.

# Reduction

The reduction goes through a few steps. We start with the integer program in [Problem 1], and let $y_i = a_i-x_i$, and we get

\begin{align*}
\text{Minimize:} & \sum_{i=1}^n |y_i| \\
\text{subject to:} & \sum_{i=1}^n w_i y_i = \sum_{i=1}^n w_i a_i - c\\
& a_i-b_i\leq y_i \leq a_i \text{ for all } 1\leq i \leq n\\
\end{align*}

Hence, let $c' = \sum_{i=1}^n w_ia_i -c$, and $l_i = a_i-b_i$ and $u_i = a_i$.

\begin{align*}
\text{Minimize:} & \sum_{i=1}^n |y_i| \\
\text{subject to:} & \sum_{i=1}^n w_i y_i = c'\\
& l_i \leq y_i \leq u_i \text{ for all } 1\leq i \leq n\\
\end{align*}

Now, let $y_i=y_i^+ - y_i^-$, where $y_i^-,y_i^+\geq 0$, we can remove the absolute value.

\begin{align*}
\text{Minimize:} & \sum_{i=1}^n y_i^+ + y_i^- \\
\text{subject to:} & \sum_{i=1}^n w_i y_i^+ + \sum_{i=1}^n -w_i y_i^- = c'\\
& l_i \leq y_i^+- y_i^- \leq u_i \text{ for all } 1\leq i \leq n\\
& y_i^-, y_i^+\geq 0 \text{ for all } 1\leq i \leq n\\
\end{align*}

Observe we can separate the inequalities involving $y_i^+ - y_i^-$, because there is always an optimal solution where $y_i^-$ or $y_i^+$ is $0$.

{Remark}
    This observation fails when the number of bins is more than $1$.

\begin{align*}
\text{Minimize:} & \sum_{i=1}^n y_i^+ + y_i^- \\
\text{subject to:} & \sum_{i=1}^n w_i y_i^+ + \sum_{i=1}^n -w_i y_i^- = c'\\
& 0 \leq y_i^+ \leq u_i \text{ for all } 1\leq i \leq n\\
& 0 \leq y_i^- \leq -l_i \text{ for all } 1\leq i \leq n\\
\end{align*}

This is an integer program is in the following form as a bounded exact knapsack problem.

\begin{align*}
\text{Minimize:} & \sum_{i=1}^n x_i \\
\text{subject to:} & \sum_{i=1}^n w_i x_i = c\\
& 0 \leq x_i \leq b_i \text{ for all } 1\leq i \leq n\\
\end{align*}

Finally, apply the standard technique that rewrites a bounded knapsack problem to $0$-$1$-knapsack problem (say, see Section 7.1.1 of [@9783540402862]). Note the blow up is at most a factor of $O(\log \max_i b_i)$. We can get the integer program in [Problem 2], and also the weights are all powers of $2$. The reduction runs in polynomial time with respect to input size.

# Solving [Problem 2]

[Yuzhou Gu](http://sevenkplus.com/) noted that the integer program in [Problem 2] could be solved by dynamic programming.

Let $D[m,k]$ to be the optimal value to the following problem

\begin{align*}
\text{Minimize:} & \sum_{j=1}^m c_j x_j \\
\text{subject to:} & \sum_{j=1}^m w_j x_j = k |w_m| + t \bmod |w_m|\\
& x_j \in \{0,1\} \text{ for all } 1\leq j \leq m\\
\end{align*}

The claim is that $D[m,k]$ can be expressed by the following recurrence relation.

\[
D[m,k] = \min_{x_m\in \{0,1\}} D\left[m-1,\frac{|w_m|k- w_m x_m+(t\bmod |w_m| - t\bmod |w_{m-1}|)}{|w_{m-1}|}\right]
\]

Note that $|k|$ is at most $m$. Therefore the table has at most $O(n^2)$ entries. To obtain the solution to the original equation, we just have to find the minimum overall $D[n, k]$. Clearly, this runs in polynomial time.