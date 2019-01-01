---
title: Strings with hamming distance exactly $1$
tags: algorithms, strings, tries
---

[Lin Yang](http://darktef.github.io/) asked me about the complexity for the following problem, which is the day 2 part 2 of the [advent of code 2018](https://adventofcode.com/2018). It is an elegant programming exercise, and also a clever algorithmic exercise. The problem can be summarized below. 

{Problem}
    Given a set $S$ of $n$ length $m$ strings. Decide if there are two of them that differs at exactly one position.

In other words, we want to find two strings in $S$ with [hamming distance](https://en.wikipedia.org/wiki/Hamming_distance) $1$.

The naive algorithm would have running time $O(n^2m)$. The complexity of the problem have gathered a lot of attention a while ago, for example a [post in dev.to]( https://dev.to/conectado/advent-of-code-day-2-part-2-complexity-556l), and [on reddit](https://www.reddit.com/r/adventofcode/comments/a2damm/2018_day2_part_2_a_linear_time_solution/). Some of them had a running time of $O(nm^2)$ instead. Some require hashing to get the _expected_ running time of $O(mn)$. Here we are interested in an algorithm with _worst case_ $O(mn)$ time.

# An $O(mn)$ time algorithm

First, we define some equivalent classes on the strings in $S$.

1. $x\equiv^i y$, if $x[1..i-1]=y[1..i-1]$. Namely, if the first $i-1$ elements of $x$ and $y$ match.
2. $x\equiv_i y$ if $x[i+1..m]=y[i+1..m]$. Namely, if the last $m-i+1$ elements of $x$ and $y$ match.

The algorithm uses the following idea. For each $i$, decide if there are any strings $x$ and $y$ such that differs in precisely position $i$. 

{Lemma}
    For distinct $x$ and $y$, they differ only in position $i$ if and only if $x\equiv^i y$ and $x\equiv_i y$.

Let $\mathcal{P}_i$ and $\mathcal{S}_i$ be the collection of equivalent classes of $\equiv^i$ and $\equiv_i$, respectively.

{Lemma}
    Given the collections $\mathcal{P}_i$ and $\mathcal{S}_i$, there is an $O(n)$ time algorithm to test if there are two strings $x,y\in S$ that differs in precisely position $i$.

{Proof}
    We want to finding $x$ and $y$, such that $x,y\in A\in \mathcal{P}_i$ and $x,y\in B\in \mathcal{S}_i$. 
    Let $\mathcal{P}_i = \set{P_1,\ldots,P_t}$ and $\mathcal{S}_i=\set{S_1,\ldots,S_k}$.
    For each $x\in S$, we associate it with the tuple $I_x = (a,b)$ such that $x\in P_a$ and $x\in S_b$.
    We obtain a simpler looking problem: find $x$ and $y$ such that $I_x=I_y$. The simplified problem can be solved in $O(n)$ time. The pair is just a base $n$ number with $2$ digits. We can apply radix sort with running time $O(n)$, and then it is easy to check if any number appears twice. 

{Lemma}
    Finding $\mathcal{P}_1,\ldots,\mathcal{P}_m$ and $\mathcal{S}_1,\ldots,\mathcal{S}_m$ can be done in $O(mn)$ time.

{Proof}
    To find the equivalent classes, build two tries for the strings. Trie $T_\mathcal{P}$ for strings in $S$ and trie $T_\mathcal{S}$ for the reversal of strings in $S$. Building the tries takes $O(mn)$ time. Inspect the nodes at depth $i-1$ in $T_P$ and nodes at depth $m-i+1$ in $T_S$ to recover $\mathcal{P}_i$ and $\mathcal{S}_i$ in $O(n)$ time. 

{Theorem}
    There is an $O(mn)$ time algorithm that solves [Problem 1].

{Proof}
    Finding the sequence of equivalent classes takes $O(mn)$ time by [Lemma 4]. For each $i$, checking if there exists $x,y\in S$ differs in precisely position $i$ takes $O(n)$ time by [Lemma 3]. Since $i$ ranges from $1$ to $m$, we obtain the final running time is $O(mn)$.

# Remarks

[Ruosong Wang](https://dblp.uni-trier.de/pers/hd/w/Wang:Ruosong) communicated another $O(mn)$ solution. It is much easier to describe. 
Let $\diamond$ be a symbol not in the alphabet. Build a [generalized suffix tree](https://en.wikipedia.org/wiki/Generalized_suffix_tree) over the set of strings $S'=\set{x\diamond x| x\in S}$. Traverse the suffix tree, up to level $m$, and output `true` if a path that contains $\diamond$ was traversed, and can lead to more than $2$ leaves. Indeed, this means the substring $x\diamond y$ appears at least twice. Hence there are at least two strings of the form $yax$ and $ybx$ in $S$. 
This definitely hits the optimal running time, but implementing a generalized suffix tree is fairly hard.  

We do assume the alphabet size is constant. If the alphabet size is $\sigma$ and ordered, then there is an extra factor of $\log \sigma$ in building the tires. The the final running time will be $O(mn\log \sigma)$. 

[Problem 1] also reduce to finding the closest pair of elements by hamming metric [@MinKZ09]. It does not get us the desired running time though. 

# An implementation in Haskell

The implementation is mostly faithful to the presentation in the article. We did not implement counting sort nor radix sort. 

<script src="https://gist.github.com/chaoxu/a4a60408a069edf3889e8328e685f700.js"></script>
