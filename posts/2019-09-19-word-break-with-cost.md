---
title: Word break with cost
tags: Algorithm
---


::: Problem

  Given a set of strings $W$, a cost function $c:W\to \R$ and a string $s$.
  Find elements $w_1,\ldots,w_k\in W$ such that $s=w_1\ldots w_k$, and $\sum_{i=1}^k c(w_i)$ is minimized.

:::

This problem is a generalization of the [word break](https://leetcode.com/problems/word-break/) problem on leetcode.

There is a simple dynamic programming algorithm with running time $O(nm)$ that generalizes the dynamic programming algorithm for word break. Here $n$ is the length of the string $s$ and $m=\sum_{w\in W}|w|$.

There is a faster algorithm. We consider the following graph $G=(V,E)$, where $V=\set{0,\ldots,n}$, and there is an edge from $i$ and $j$, if $s[i+1..j]=w\in W$, and the label of the edge $(i,j)$ is the string $w$, and the cost is $c(w)$. The graph has size $O(n\sqrt{m})$. Indeed, the sum of the length of the labels of all outgoing edges cannot be more than $m$, and the length of each label is different. Hence each vertex can have at most $O(\sqrt{m})$ outgoing edges. The graph is a DAG, so we can find the shortest path from $0$ to $n$ in linear time with respect to the number of edges. 
This shows if we can compute the graph in $O(n\sqrt{m}+m)$ time, then we solve the problem in $O(n\sqrt{m}+m)$ time.

We can build the Aho–Corasick automaton for $W$ in $O(m)$ time. It can be used to find all substrings of $s$ that matches something in $W$ by traversing the automaton once. The running time is the total number of substrings matched, which is $O(n\sqrt{m})$. Hence building the graph takes $O(n\sqrt{m}+m)$ time.
Actually, let $p=|W|$, then what we proved is actually a running time of $O(np+m)$, it is just in the worst case $p=\Omega(\sqrt{m})$. 

If we only want to know if there exists a solution, then there is a $\tilde{O}(nm^{1/3}+m)$ time algorithm [@BringmannGL17], and it is optimal assuming the algorithm is combinatorial, and the alphabet can be arbitrarily large. 

Can we also obtain similar running time for the word break with cost problem? There are evidence against it. If the alphabet is unary, this problem is equivalent to the unbounded knapsack problem, which likely does not have an algorithm with running time $O((n+p)^{2-\e})$ for any $\e>0$ [@CyganMWW19] and $p$ can be as large as $\Omega(m)$. Of course, this does not mean there might not be a $O(nm^{1/3}+m)$ time algorithm, since the reduction involved in the paper might not hold when we require $p=\Omega(\sqrt{m})$. 
