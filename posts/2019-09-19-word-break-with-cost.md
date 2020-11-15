---
title: Word break with cost
tags: Algorithm
---


::: Problem

  Given a set of strings $W$, a cost function $c:W\to \R$ and a string $s$.
  Find elements $w_1,\ldots,w_k\in W$ such that $s=w_1\ldots w_k$, and $\sum_{i=1}^k c(w_i)$ is minimized.

:::

This problem is a generalization of the [word break](https://leetcode.com/problems/word-break/) problem on leetcode. Many algorithms you see online assumes that string in $W$ has constant length, checking the hash table takes $O(1)$ time, and obtain an $O(n^2)$ time algorithm. It is not as easy. Here we show an algorithm that considers the strings in $W$ have arbitrary length.

Consider the following graph $G=(V,E)$, where $V=\set{0,\ldots,n}$, and there is an edge from $i$ and $j$, if $s[i+1..j]=w\in W$, and the label of the edge $(i,j)$ is the string $w$, and the cost is $c(w)$. 
Let $z$ be number of substrings in $s$ matches some element in $W$. The graph has $z$ edges. Note $z=O(n\sqrt{L})$. Indeed, the sum of the length of the labels of all outgoing edges cannot be more than $L$, and the length of each label is different. Hence each vertex can have at most $O(\sqrt{L})$ outgoing edges. The graph is a DAG, so we can find the shortest path from $0$ to $n$ in linear time with respect to the number of edges. 
This shows if we can compute the graph in $O(z+L)$ time, then we solve the problem in $O(z+L)$ time.

We can build the Aho–Corasick automaton for $W$ in $O(L)$ time. It can be used to find all substrings of $s$ that matches something in $W$ by traversing the automaton once. The running time is the total number of substrings matched, which is $O(z)$. Hence building the graph takes $O(z+L)$ time.
$z$ is clearly no more than $nm$, where $m=|W|$. Also, it is also clear $z=O(n\sqrt{L})$. Indeed, there can be at most $O(\sqrt{L})$ edges start from $i$, since each edge has a label of different length, and sum of those length labels is no larger than $L$. 

If we only want to know if there exists a solution, then there is a $\tilde{O}(nL^{1/3}+L)$ time algorithm [@BringmannGL17]. The algorithm is close to optimal assuming the algorithm is combinatorial and the alphabet can be arbitrarily large. A recent progress showed the version with minimum cost is also solvable in $\tilde{O}(nL^{1/3}+L)$ time [@ChanH20].