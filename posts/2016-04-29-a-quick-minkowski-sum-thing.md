---
title: Minkowski sum of special kind of sets
tags: math, algorithm, fft
---

We consider the following problem.

{Problem}
    Given integer sets $A,B\subset [0,u)$,$a$,$b$,$k$ and $u$ such that $A,B,A+B\subset \bigcup_{i=0}^k [ia,ib]$, find $(A+B)\cap [0,u)$.

Recently someone asked me about the pseudopolynomial time algorithm we devised, so I actually write some pseudocode for this.

First, we assume the existence of function Convolution($A$,$B$), which computes $A+B$ given $A,B \subset \mathbb{N}^2$. If for all $(x,y)\in A\cup B$, we have $x\leq n$ and $y\leq m$, then the running time of Convolution($A$,$B$) is $\tilde{O}(nm)$. 

We define $Convolution(A,B)$ for $A,B\subset \mathbb{N}$ to be $Convolution(A\uparrow,B\uparrow)\downarrow$ where $X\uparrow=\{(0,x)|x\in X\}$ and $Y\downarrow = \{x|(y,x)\in X\}$. 

Here is the algorithm

1. Input: $A,B,a,b,k,u$
2. $n\gets \min\{\lfloor u/a\rfloor,k\}$
3. if $a\leq n(b-a)$, then $h(x)=(0,x)$, otherwise $h(x)=(\lfloor x/a \rfloor, x \pmod a)$
4. Output: $h^{-1}(Convolution(h(A),h(B)))\cap [0,u)$

$h$ is bijection, and if $(x,y)\in h(A)\cup h(B)$, then $x\leq n, y\leq n(b-a)$.

The running time of this algorithm is $\tilde{O}(\min\{u, bk, \min\{\lfloor u/a\rfloor,k\}^2(b-a)\})$.