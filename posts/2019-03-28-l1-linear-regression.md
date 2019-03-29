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

One can realize in the optimal solution, it is a line that goes through two of the points. The native algorithm can try all possible $O(n^2)$ lines. For each line, the algorithm can compute the error in $O(n)$ time. Together it is a $O(n^3)$ time algorithm. There is a smarter algorithm. The optimal line that contains the point can actually be found in $O(n)$ time. Indeed, it should be a line with the same number of points above as the same number of points below. Hence one can show the running time is $O(n^2)$. This is basically the idea of [@BloomfieldS80], but their algorithm design is closer to the simplex algorithm. As far as I know, this seems to be the state of the art. There are many iterative methods, which of course depends on the input values. 

Does a faster algorithm exist?