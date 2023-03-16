---
title: $L_1$ linear regression
tags: combinatorial optimization
---

I read an article on the [errors in visualization](https://medium.economist.com/mistakes-weve-drawn-a-few-8cdd8a42d368). 
The example of forcing a relationship by cherry-picking scales is delightful. I recommend reading it. 

I am interested in misleading people while being completely honest. 
The article inspires the following problem.
Given 2 vectors $\bm{x},\bm{y}\in \R^n$. 
Let $\bm{1}$ be the all $1$ vector in $\R^n$.
We are interested in finding $a,b\in \R$, such that $\|\bm{y}-(a\bm{x}+b\bm{1})\|_p$ is minimized. Here $p$ is either $1,2$ or $\infty$.

Note the problem is precisely the same as the linear regression problem.
In the linear regression problem, we are given a point set $S\subset \R^2$ of size $n$ and we are interested in find a line $f(x) = ax+b$, such that it minimizes the _error_, defined as 

\[
\sum_{(x,y)\in S} \|y - f(x)\|_p
\]

For $p=2$, there is a $O(n)$ time algorithm because there is a closed formula.
For $p=\infty$, the problem can be rewritten as a linear program with $3$ variables and $n$ constraints. Using Megiddo's result [@Megiddo84], there is a $O(n)$ time algorithm to solve this problem.

It is hard to find the worst case complexity when $p=1$. This case is called the _least absolute deviations_. Statisticians just don't care about worst case running time as CS people do. 

There are a few methods I found. One is to write it as a linear program on $n+2$ variables and $n$ constraints and solve it using the simplex method. The linear program is as follows.

\[
\begin{aligned}
& \min_{a,b,t_1,\ldots,t_n}
& & \sum_{i=1}^n t_i & \\
& \text{s.t.} & &  t_i \geq (ax_i+b)-y_i & \forall 1 \leq i \leq n \\
& & &  t_i \leq y_i-(ax_i+b) & \forall 1 \leq i \leq n \\
\end{aligned}
\]

There are a bunch of other algorithms that specializes the simplex algorithm on this particular problem. There are also some iterative methods. Unfortunately, those algorithms depends on the actual numbers in the input. I want a running time that only depends on $n$.

There exists an optimal solution that contains two points in $S$. The native algorithm is to try all possible $O(n^2)$ lines. For each line, the algorithm can compute the error in $O(n)$ time. The naive algorithm's running time is $O(n^3)$. There is a smarter algorithm. The optimal line that contains the point can actually be found in $O(n)$ time. 
Indeed, consider the line passes through the point $(x,y)$. We consider changing the slope of the line, while maintaining it still contain $(x,y)$. One can see a minimum will be reached at some line. Indeed, assume we reorder the points, so $\frac{y_i-y}{x_i-x}\leq \frac{y_{i+1}-y}{x_{i+1}-x}$ (namely, increasing slope). Let $k$ be the smallest integer such that the sum of $\sum_{i=1}^k |x_i-x|\geq \sum_{i=k+1}^n |x_i-x|$. The line determined by $(x,y)$ and $(x_k,y_k)$ is the desired line. This can be computed in linear time by finding weighted median. Hence one can show the running time is $O(n^2)$. This is the idea of [@BloomfieldS80]. That is all I can find through an hour of search. 

After discussing with [Qizheng He](https://sites.google.com/site/qizhenghe96/home), he suggested the following approach.
Consider the function $g_p(s)$ for $p\in S$. It is defined as the error for the line of slope $s$ that contains $p$. The function is bitonic, therefore we can do a ternary search to find the minimum. There are only $n-1$ possible slopes, hence the ternary search will take $O(\log n)$ queries, where each query asks for the error of the line that goes through $p$ and some other point.

Given a line $f(x)=ax+b$, can one compute the error quickly? It is possible to decompose it to few halfspace range counting queries (allowing weights).
In halfspace counting queries problem, we are given $n$ points with weights, we can preprocess it and obtain a data structure. Each query to a data structure is a halfspace, the output is the sum of all elements in the halfspace. In $2$D, there exists a preprocessing time $\tilde{O}(n^{4/3})$ and query time $\tilde{O}(n^{1/3})$ data structure [@Matousek93]. 
Let $S^+$ be the set of points above $f$, and $S^-$ be the set of points below $f$. The result is precisely the following.

\[
\sum_{(x,y)\in S^+} y - ax-b + \sum_{(x,y)\in S^-} ax+b - y
\]

Let's consider the second sum, $\sum_{(x,y)\in S^-} ax+b - y = a\sum_{(x,y)\in S^-}x + |S^-|b -\sum_{(x,y)\in S^-}y$. Note the $3$ terms can each be solved with a halfspace counting query, consider all points lies below $f$. This shows in $6$ halfspace counting queries.

How can one do ternary search? This would need us to be able to pick the point that gives us the $i$th largest slope with $p$. We need a data structure such that it can return the $i$th largest point in the radial ordering of the points in $S$ around $p$. It is equivalent to [halfspace range counting up to polylog factors](https://cstheory.stackexchange.com/questions/42609/data-structure-for-radial-orderings-of-points-on-the-plane). 

Thus, the total running time after building the data structure in $\tilde{O}(n^{4/3})$ is $n$ times ternary search over $n$ elements, where each decision process takes $\tilde{O}(n^{1/3})$ time. Therefore the final running time is $\tilde{O}(n^{4/3})$ time.

Qizheng mentioned the problem to [Timothy Chan](http://tmc.web.engr.illinois.edu), who gave us some references. There is an easy solution that obtains $O(n\log^2 n)$ time algorithm using simple parametric search [@MegiddoT83]. 
Consider the following linear program. Let $k$ be a constant. We are given $a_1,\ldots,a_k,b_1,\ldots,b_n$, $k$D vectors $\beta_1,\ldots,\beta_m$ and reals $\alpha_1,\ldots,\alpha_m$. Sets $J_1,\ldots,J_n$ a partition of $[m]$. 

\[
\begin{aligned}
& \min_{w_1,\ldots,w_k,x_1,\ldots,x_n}
& & \sum_{i=1}^k a_iw_i + \sum_{i=1}^n b_ix_i & \\
& \text{s.t.} & &  x_i \geq (\sum_{d=1}^k \beta_{j,d} w_d) - \alpha_j & \forall 1 \leq i \leq n, j\in J_i
\end{aligned}
\]

Zemel showed such linear program can be solved in $O(m)$ time for constant $k$ [@Zemel84]. The idea is a similar algorithm to Megiddo's linear time constant dimension LP algorithm [@Megiddo84]. 
For linear regression problem in $L_1$ with $n$ data points. The linear program we derived is a special case of the above linear program when $k=2$ and $m=O(n)$. In fact, Zemel use the same linear program to show constant dimension $L_1$ regression can be solved in linear time.

# Open problem

One can also define another metric, the lexicographical minimum. Such idea was already present in fairness related linear regression [@KoeppenYO14]. Once we sort the values of $|y - f(x)|$ for $(x,y)\in S$, say obtaining $a_1,\ldots,a_n$, where $a_1\geq a_2 \geq \ldots \geq a_n$. We are interested in finding a $f$ that minimizes the sequence $a_1,\ldots,a_n$, lexicographically. Can this problem be solved in $O(n)$ time? 
