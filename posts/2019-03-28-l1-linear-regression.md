---
title: $L_1$ linear regression
tags: combinatorial optimization
---

I read an article on the [errors in visualization](https://medium.economist.com/mistakes-weve-drawn-a-few-8cdd8a42d368). 
The example of forcing a relationship by cherry-picking scales is delightful. I recommend reading it. 

I am interested in how to mislead people as much as possible, while being absolutely correct.
The article inspires the following problem.
Given 2 vectors $\bm{x},\bm{y}\in \R^n$. 
Let $\bm{1}$ be the all $1$ vector in $\R^n$.
We are interested in finding $a,b\in \R$, such that we minimizes $\|\bm{y}-(a\bm{x}+b\bm{1})\|_p$. Here $p$ is either $1,2$ or $\infty$.

Note the problem is precisely the same as the linear regression problem.
In the linear regression problem, we are given points $(x_1,y_1),\ldots,(x_n,y_n)$ and we are interested in find a line $f(x) = ax+b$, such that it minimizes the _error_, defined as 

\[
\sum_{i=1}^n \|y_i - f(x_i)\|_p
\]

For $p=2$, there is a $O(n)$ time algorithm because there is a closed formula.
For $p=\infty$, the problem can be rewritten as a linear program with $3$ variables and $n$ constraints. Using Megiddo's result [@Megiddo84], there is a $O(n)$ time algorithm to solve this problem.

Strangely, I could not find just how hard is it to solve the problem when $p=1$. This case is called the _least absolute deviations_. Statisticians just don't care about worst case running time as CS people do. 

There are a few methods I found. One is to write it as a linear program on $n+2$ variables and $n$ constraints and solve it using the simplex method. There are a bunch of other algorithms that specializes the simplex algorithm on this particular problem. 
   
One can realize in the optimal solution, it is a line that goes through two of the points. The native algorithm can try all possible $O(n^2)$ lines. For each line, the algorithm can compute the error in $O(n)$ time. Together it is a $O(n^3)$ time algorithm. There is a smarter algorithm. The optimal line that contains the point can actually be found in $O(n)$ time. 
Indeed, consider the line passes through the point $(x,y)$. We consider changing the slope of the line, while maintaining it still contain $(x,y)$. One can see a minimum will be reached at some line. Indeed, assume we reorder the points, so $\frac{y_i-y}{x_i-x}\leq \frac{y_{i+1}-y}{x_{i+1}-x}$ (namely, increasing slope). Let $k$ be the smallest value such that the sum of $\sum_{i=1}^k |x_i-x|\geq \sum_{i=k+1}^n |x_i-x|$ indicate the point $(x_k,y_k)$. The line determined by $(x,y)$ and $(x_k,y_k)$ is the desired line. This can be computed in linear time by finding weighted median. Hence one can show the running time is $O(n^2)$. This is the idea of [@BloomfieldS80]. As far as I know, this seems to be the state of the art. There are many iterative methods, which of course depends on the input values. 


After discussing with [Qizheng He](https://sites.google.com/site/qizhenghe96/home), he suggested the following approach.

Given a line $f(x)=ax+b$, can one compute the error quickly? It is possible to decompose it to few halfspace counting queries (allowing weights).
In halfspace counting queries problem, we are given $n$ points with weights, we can preprocess it and obtain a data structure. Each query to a data structure is a halfspace, the output is the sum of all elements in the halfspace. In $2$D, there exists a preprocessing time $\tilde{O}(n^{4/3})$ and query time $\tilde{O}(n^{1/3})$ data structure [@Matousek93]. 
Let $S^+$ be the set of points above $f$, and $S^-$ be the set of points below $f$. The result is precisely the following.

\[
\sum_{(x,y)\in S^+} y - ax-b + \sum_{(x,y)\in S^-} ax+b - y
\]

Let's consider the second part, $\sum_{(x,y)\in S^-} ax+b - y = a\sum_{(x,y)\in S^-}x + |S^-|b -\sum_{(x,y)\in S^-}y$. Note the $3$ terms can each be solved with a halfspace counting query, consider all points lies below $f$. This shows in $6$ halfspace counting queries.

Consider the function $g_p(s)$ defined for a point $p$ to be the error for the line with slope $s$ that contains $p$. The function is bitonic, therefore we can do a ternary search to find the minimum. The only problem is that we do not have a space of the slopes for $p$ that we can quickly go through. 

Halfspace counting data structure is built on top of partition tree. So we can look into partition tree directly. I'm not that familiar with partition tree, so I'm just writing this out to check my basic intuition. Say the partition tree has a branching factor of $r$, then we can spend $\tilde{O}(r)$ time to generate one testing line for each of the $r$ partitions. We can do ternary search on those lines in $O(\log r)$ error computations. Once we find a few adjacent lines. Together it would cut $O(r^{1/2})$ partitions each with $O(\frac{n}{r})$ elements. Now we just have to recurse. I did not completely verify. But setting $r$ to be $n^{1/3}$ is fine. This would imply a $\tilde{O}(n^{4/3})$ running time algorithm. 
