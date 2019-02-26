---
title: Find the square root of an integer with trigonometry and Lagrange's four-square theorem
tags: math
---

My friend was solving the following problem during a interview for Citigroup's IT department. 

{Problem}
    Find the square root of a integer n, without using the built in sqrt function. (The range of the result was not specified, I assume it's double)
   
This is a common interview question.

There are [many ways to do it](http://en.wikipedia.org/wiki/Methods_of_computing_square_roots). I want to come up with a way no one else would think of, something that could amaze the interviewer. I mean, she might interviewed enough people to get bored with the standard answers. 

I present the following highly inefficient but somewhat creative solution. [The code is here](https://github.com/chaoxu/mgccl-haskell/blob/master/random/sqrtOfInteger.hs).

How does it work?

We know $n$ is an integer. By [Lagrange's four-square theorem](http://en.wikipedia.org/wiki/Lagrange's_four-square_theorem), $n=a^2+b^2+c^2+d^2$ for integer $a,b,c,d$. $\sqrt{n} = \sqrt{a^2+b^2+c^2+d^2}$. Thus $\sqrt{n}$ is the magnitude of the vector $[a,b,c,d]$. $a,b,c,d$ can be calculated by brute force search(therefore runs in $O(n^2)$ time). 

Note a simple improvement of the naive algorithm can reduce the computation time to $O(n^\frac{3}{2} \log n)$ by doing a binary search for the last square.

[A much smarter randomized algorithm](http://onlinelibrary.wiley.com/doi/10.1002/cpa.3160390713/abstract) by Michael O. Rabin and Jeffrey Shallit have a running time of $O(\log^2 n)$.

A recursive algorithm using the following relation can find the magnitude of any vector(assume $a_i\neq 0$)
$|[a_0,...,a_{n-1},a_n]| = \frac{a_n}{\sin(\tan^{-1}(\frac{a_n}{|[a_0,...,a_{n-1}]|}))}$
It's easy to see, this breaks a $n$-dimension vector into orthogonal vectors of $n-1$-dimensions and $1$-dimension. We get a right triangle. Trigonometry comes in handy. 