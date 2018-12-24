---
title: strings with hamming distance exactly $1$
tags: algorithms, subset sum
---

[Lin Yang](http://darktef.github.io/) asked me a the complexity for the following problem, which is the day 2 part 2 of the [advent of code 2018](https://adventofcode.com/2018). It is an elegant programming exercise, and also a clever algorithmic exercise. The problem can be summarized below. 

{Problem}
  Given a set $S$ of $n$ length $m$ strings. Decide if there are two of them that differs by exactly one position.

The naive algorithm would have running time $O(n^2m)$.

The complexity of the problem have gathered a lot of attention a while ago, for example a [post in dev.to]( https://dev.to/conectado/advent-of-code-day-2-part-2-complexity-556l), and [on reddit](https://www.reddit.com/r/adventofcode/comments/a2damm/2018_day2_part_2_a_linear_time_solution/). Some of them had a running time of $O(nm^2)$, and one with expected running time $O(nm)$ through hashing. Of course, it would require $m$ to be small for it to work. 

Currently, we are not aware of any $O(nm)$ time algorithm in existence. Here we present an $O(nm)$ time algorithm. 

# The algorithm

First, we define some equivalent classes. 

1. $x\equiv^i y$, if $x[1..i-1]=y[1..i-1]$. Namely, if the first $i-1$ elements of $x$ and $y$ match.
2. $x\equiv_i y$ if $x[i+1..m]=y[i+1..m]$. Namely, if the last $m-i+1$ elements of $x$ and $y$ match.

Let $\dist$ be the [hamming distance](https://en.wikipedia.org/wiki/Hamming_distance). So we are interested in finding two strings $x$ and $y$ such that $\dist(x,y)=1$.

The algorithm uses the following idea. For each $i$, decide if there are any strings $x$ and $y$ such that differs in precisely position $i$. 

{Lemma}
  For distinct $x$ and $y$, they differ only in position $i$ if $x\equiv^i y$ and $x\equiv_i y$.

The running time is $m$ times the running time of checking the above condition. Now we consider how to check the condition.
Let $\mathcal{P}_i$ and $\mathcal{S}_i$ be the set of equivalent classes of $\equiv^i$ and $\equiv_i$. Checking the condition is finding $x$ and $y$, such that $x,y\in A\in \mathcal{P}_i$ and $x,y\in B\in \mathcal{S}_i$. 
We take the union(with multiplicity) of $\mathcal{P}_i$ and $\mathcal{S}_i$, say we obtain sets $C_1,\ldots,C_k$.

For each $x$, we associate it with the set $I_x = \set{i | x\in C_i \text{ for some } i}$.
The problem reduces to find $x$ and $y$, such that $|I_x \cap I_y|\geq 2$.  One can see $|I_x|=2$ for each $x$, which gives us a simpler looking problem. Find $x$ and $y$ such that $I_x=I_y$. This problem can be solved in $O(k)$ time. 
Indeed, we can encode $I_x=\set{a,b}$ as a pair $(a,b)$, where $a<b$. We see this as a base $k$ number with $2$ digits. We can apply radix sort with running time $O(k)$, and then it is easy to check if any number appears twice. 

Therefore, if we are given the equivalent classes $P_i$ and $S_i$, there is an $O(n)$ time algorithm to test if there are two strings $x$ and $y$ differs in precisely position $i$.

To find the equivalent classes, just build two tries for the strings. $\mathcal{P}$ for strings in $S$ and $\mathcal{S}$ for reversed strings in $S$. Look at the nodes at depth $i-1$ in $\mathcal{P}$ and nodes at depth $m-i+1$ in $\mathcal{S}$ to recover $P_i$ and $S_i$ in $O(n)$ time. 

Together, this gives us an algorithm that runs in $O(nm)$ time. This is the best possible, as the input size is already $O(nm)$.

# Remarks

We do assume the alphabet size is constant. If the alphabet size is $\sigma$ and ordered. there is an extra factor of $\log \sigma$ in building tires. The the final running time will be $O(nm\log \sigma)$. 

The problem can reduce to finding the closest pair of elements by hamming metric [@MinKZ09]. It does not get us the desired running time though. 

The problem we try to solve need to solve a subproblem of the following form, where $k=2$.

{Problem}
  Given sets $S_1,\ldots,S_n$, let $U=\bigcup_{i=1}^n S_i$. Decide if there exists $S_i$ and $S_j$ such that $|S_i\cap S_j|\geq k$.

I have wrote about [this problem before](/posts/2015-02-08-two-problem-related-to-sequence-of-sets.html).



