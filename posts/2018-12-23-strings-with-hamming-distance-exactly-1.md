---
title: Strings with hamming distance exactly $1$
tags: algorithms, strings, tries
---

[Lin Yang](http://darktef.github.io/) asked me about the complexity for the following problem, which is the day 2 part 2 of the [advent of code 2018](https://adventofcode.com/2018). It is an elegant programming exercise, and also a clever algorithmic exercise. The problem can be summarized below. 

::: Problem
  Given a set $W$ of $n$ length $m$ strings. Decide if there are two of them that differs at exactly one position.
:::
In other words, we want to find two strings in $W$ with [hamming distance](https://en.wikipedia.org/wiki/Hamming_distance) $1$.

The naive algorithm would have running time $O(n^2m)$. The complexity of the problem have gathered a lot of attention a while ago, for example a [post in dev.to]( https://dev.to/conectado/advent-of-code-day-2-part-2-complexity-556l), and [on reddit](https://www.reddit.com/r/adventofcode/comments/a2damm/2018_day2_part_2_a_linear_time_solution/). Some of them had a running time of $O(nm^2)$ instead. Some require hashing to get the _expected_ running time of $O(mn)$. Here we are interested in an algorithm with _worst case_ $O(mn)$ time.

# An $O(mn)$ time algorithm

First, we define some equivalent classes on the strings in $W$.

1. $x\equiv^i y$, if $x[1..i-1]=y[1..i-1]$. Namely, if the first $i-1$ elements of $x$ and $y$ match.
2. $x\equiv_i y$ if $x[i+1..m]=y[i+1..m]$. Namely, if the last $m-i+1$ elements of $x$ and $y$ match.

The algorithm uses the following idea. For each $i$, decide if there are any strings $x$ and $y$ such that differs in precisely position $i$. 

::: Lemma
  For distinct $x$ and $y$, they differ only in position $i$ if and only if $x\equiv^i y$ and $x\equiv_i y$.
:::
Let $\mathcal{P}_i$ and $\mathcal{S}_i$ be the collection of equivalent classes of $\equiv^i$ and $\equiv_i$, respectively. We show a result related to the meet of partitions.  

::: Lemma
  Let $\mathcal{A}$ and $\mathcal{B}$ be partitions of $[n]$. There is an $O(n)$ time algorithm to test find the sets in $\set{ A\cap B | A\in \mathcal{A}, B\in \mathcal{B}}$.
:::
::: Proof
  Let $\mathcal{A}=\set{A_1,\ldots,A_k}$ and $\mathcal{B} = \set{B_1,\ldots,B_\ell}$.
  We define $I_i = (a,b)$ such that $i\in A_a$ and $i\in B_b$.
  Then we know $i$ is in $A_a\cap B_b$ if $I_i=(a,b)$. Hence we are interested in find the largest set of elements such $S$ such that for $i,j\in S$, $I_i=I_j$. The simplified problem can be solved in $O(n)$ time. Indeed, the pair is just a base $n$ number with $2$ digits. We can apply radix sort with running time $O(n)$ and group by the result. 
:::

Note one can also directly use a [partition refinement data structure](https://en.wikipedia.org/wiki/Partition_refinement) to get the same result. 

As a corollary, consider $\mathcal{A}=\mathcal{P}_i$ and $\mathcal{B}=\mathcal{S}_i$, then we obtain the following lemma. 

::: Lemma
  Given the collections $\mathcal{P}_i$ and $\mathcal{S}_i$, there is an $O(n)$ time algorithm to test if there are two strings $x,y\in W$ that differs in precisely position $i$.
:::
::: Lemma
  Finding $\mathcal{P}_1,\ldots,\mathcal{P}_m$ and $\mathcal{S}_1,\ldots,\mathcal{S}_m$ can be done in $O(mn)$ time.
:::
::: Proof
  To find the equivalent classes, build two tries for the strings. Trie $T_\mathcal{P}$ for strings in $W$ and trie $T_\mathcal{S}$ for the reversal of strings in $W$. Building the tries takes $O(mn)$ time. Inspect the nodes at depth $i-1$ in $T_P$ and nodes at depth $m-i+1$ in $T_S$ to recover $\mathcal{P}_i$ and $\mathcal{S}_i$ in $O(n)$ time. 
:::
::: Theorem
  There is an $O(mn)$ time algorithm that solves [Problem 1].
:::
::: Proof
  Finding the sequence of equivalent classes takes $O(mn)$ time by [Lemma 4]. For each $i$, checking if there exists $x,y\in W$ differs in precisely position $i$ takes $O(n)$ time by [Lemma 3]. Since $i$ ranges from $1$ to $m$, we obtain the final running time is $O(mn)$.
:::
# Remarks

[Ruosong Wang](https://dblp.uni-trier.de/pers/hd/w/Wang:Ruosong) communicated another $O(mn)$ solution. It is much easier to describe. 
Let $\diamond$ be a symbol not in the alphabet. Build a [generalized suffix tree](https://en.wikipedia.org/wiki/Generalized_suffix_tree) over the set of strings $S'=\set{x\diamond x| x\in W}$. Traverse the suffix tree, up to level $m$, and output `true` if a path that contains $\diamond$ was traversed, and can lead to more than $2$ leaves. Indeed, this means the substring $x\diamond y$ appears at least twice. Hence there are at least two strings of the form $yax$ and $ybx$ in $W$. 
This definitely hits the optimal running time, but implementing a generalized suffix tree is fairly hard.  

We do assume the alphabet size is constant. If the alphabet size is $\sigma$ and ordered, then there is an extra factor of $\log \sigma$ in building the tries. The the final running time will be $O(mn\log \sigma)$. 

[Problem 1] also reduces to finding the closest pair of elements by hamming metric [@MinKZ09]. It does not get us the desired running time though. 

# An implementation in Haskell

The implementation is mostly faithful to the presentation in the article. We did not implement counting sort nor radix sort. 

<script src="https://gist.github.com/chaoxu/a4a60408a069edf3889e8328e685f700.js"></script>

