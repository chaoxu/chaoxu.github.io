---
title: Open Problems
---

Here are some problems that are either open to the best of my knowledge, or asked by me but unable to answer. If you know a solution, or can offer me more reference on the material, please comment. I appreciate it greatly. :)

# Probably NP-hard

{Problem}

    [This one](http://cs.stackexchange.com/questions/12441/is-it-np-hard-to-fill-up-bins-with-minimum-moves).

{Problem}
    Given $a_1,\ldots,a_n$ and $b_1,\ldots,b_n$. Find a permutation such that
    \[ 
    \frac{\sum_{i=1}^n a_{\pi(i)}a_{\pi(i+1)}}{\sum_{i=1}^n b_{\pi(i)}b_{\pi(i+1)}}
    \]
    is minimized, where we define $\pi(n+1)=\pi(1)$.
    Is this problem NP-hard? Can we find approximation algorithm for this? 

Motivation: One want to permute the axis of a radar chart to maximize the ratio of the areas of two data sets.

{Problem}

    There are $n$ people sitting on a circle of $n$ seats. Out of the $n$ people, there are $k$ couples($2k$ people). You can swap any two people. Find an algorithm that swap the least amount of times such that all the couples are sitting with his/her partner.


{Problem}
    Given $n$ sets $S_1,\ldots,S_n$. For a set $T\subset [n]$, Define $f_T(i) = \max(0,|\{j| i\in S_j, j\in T\}|)$. Find a set $T$ that maximizes $\sum_{i} f_T(i)$ under the constraint that $|T|=k$. 

{Problem}
    Let $d_1,\ldots,d_n$, $p_1,\ldots,p_n$ and $q$ and $c$ be the input. 
    For $a_1,\ldots,a_n$, we define the following:

      1. $A_i = \{a_i+m d_i| m\in \N\}$. 
      2. $g(k) = \sum_{k\in A_i} p_i$.
      3. 
     $$f(k)  = \begin{cases} q g(k) &\text{if } |\{i|k\in A_i\}|\geq c\\
    g(k) & \text{if } |\{i|k\in A_i\}| < c \end{cases} $$
    Find $a_1,\ldots,a_n\geq 0$ so $$\lim_{t\to \infty} \frac{\sum_{i=1}^t f(i)}{\sum_{i=1}^t g(i)}$$ is minimized. 

This problem comes from trying to save money using amazon subscribe and save. 

# In P

Let $k$-shortest path from $s$ to $t$ to be $k$ edge disjoint paths with minimum total weight.

{Problem}
    For a constant $k$, find the $k$-shortest paths from $s$ to every vertex $v$ in a directed graph.

Let $T(m,n)$ be the running time of Dijkstra's algorithm on graph of $m$ edges and $n$ vertices. For $k=1,2$, it can be done in $O(T(m,n))$ time, see [this](http://www.eecs.yorku.ca/course_archive/2007-08/F/6590/Notes/surballe_alg.pdf). For arbitrary $k$, it can be solved through $O(n)$ times min-cost flow of value $k$, which can be done in $O(nk T(m,n))$ time using successive shortest path algorithm for min-cost flow. The question is can one do it in $O(f(k)T(m,n))$ for some function $f$. It's interesting even for $k=3$.

{Problem}
    Solve the [The gas station problem](http://www.cs.umd.edu/projects/gas/) faster? 

There are many weakening of the problem that might be doable. I need some time to organize the notes I already have on it. 

# Not algorithms

{Problem}
    Define $AB = \{ab|a\in A, b\in B\}$. Let $[\ell]=\{1,\ldots,\ell\}$. 
    Define $f(n,\ell)$ to be the smallest number $k$, such that there exist a size $k$ set $X\subset \mathbb{Z}_n$, such that $[\ell]X = \mathbb{Z}_n$. What are nice bounds on $f(n,\ell)$, and how do we find $X$ with size close to $f(n,\ell)$?

$\sigma_0(n)$ is the number of divisors of $n$ and $\sigma_1(n)$ is sum of the divisors of $n$. Current we have $f(n,\ell)\leq \frac{\sigma_1(n) \ln n}{\ell}+\sigma_0(n)$. See [this](https://www.overleaf.com/read/hnqpncgrhpbv).

# Probably very difficult open problem

{Problem}
    Let $G$ be a directed graph with a source vertex $s$, how fast can one find $\min_{t\in T} \lambda(s,t)$? 

{Problem}
    Let $G$ be a directed graph with a source vertex $s$, how fast can one [find a $r$-rooted-$k$-sparsifier](http://cstheory.stackexchange.com/questions/25268/sparse-subgraph-preserving-rooted-edge-connectivity-up-to-k)? 

One way to do it is find flow of value $k$ from $r$ to $v$. Delete all in-edges incident to $v$ but not in the flow. Repeat. But this is slow. Anything with running time $O(km)$ is good. For undirected graphs, $O(m)$ time is possible.

{Problem}
    [Maximum local edge connectivity](http://cstheory.stackexchange.com/questions/25531/maximum-local-edge-connectivity)
This problem is not important at all, but looks fairly difficult. 