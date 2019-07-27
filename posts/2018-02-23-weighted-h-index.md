---
title: Computing the weighted h-index
tags: algorithms
---

A common algorithm problem is that given a sequence of numbers, find a h-index. Where h-index is the largest integer $h$ such there are at least $h$ integers in the sequence is at least as large as $h$.

Formally, we have the following problem. 

::: Problem
  Given $a_1,\ldots,a_n$, find the largest $h$, such that $|\set{i \mid a_i\geq h}|\geq h$. 
:::
The [h-index problem is featured in leetcode](https://leetcode.com/problems/h-index/description/).

If we the numbers are sorted, then a trivial $O(n)$ time algorithm exists. If it is not sorted, then note that we can solve the problem on $\min(a_1,n),\ldots,\min(a_n,n)$. In this case, the input numbers is at most $n$, therefore can be sorted in $O(n)$ time. Hence the total running time is $O(n)$.

Consider a weighted version of the problem where the above algorithm does not work.

::: Problem
  Given a sequence of pairs of non-negative positive reals $(w_1,a_1),\ldots,(w_n,a_n)$. Find the largest $h\in \R$, such that $\sum_{i:a_i\geq h} w_i \geq h$.
:::
An $O(n)$ time algorithm still exists. 
For simplicity, we assume all $a_i$'s are distinct, so the input is a set. The case where $a_i$'s are not distinct is left as an exercise to the reader.

Define $f(t) = \sum_{i:a_i\geq t} w_i$. We want to find the largest $t$ such that $f(t)\geq t$. First, we can find the median of $a_1,\ldots,a_n$, say $t$. 
If $f(t) < t$, then we recurse on $\set{(w_i-f(t),a_i) \mid a_i< t}$. Assume the optimum in the recursed solution is $t'$, we return $t'+f(t)$ as the solution.
If $f(t)\geq t$, then we recurse and output the solution with input $\set{(w_i,a_i) \mid a_i\geq t}$.
The running time satisfies $T(n)=T(n/2)+O(n)$, which is $O(n)$.
