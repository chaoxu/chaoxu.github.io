---
title: Find the period of a nice eventually periodic sequence
tags: algorithms, infinite sequence
---

A sequence is periodic if $s_i = s_{i+p}$ for all $i$, where $p>0$ is called a period.
A sequence is eventually periodic if there exists a $n$ and $p$, such that $s_i = s_{i+p}$ for all $i>n$. The sequence with index above $n$ is the *periodic part*.

A sequence is called *$u$-normal*, if there exists $s_i=s_{i+p}$ for some $p>0$ for all $i$ in a interval of length $u$, then the sequence starting at $s_i$ is part of the periodic part. 

When does $u$-normal sequence comes up? Consider we have a recurrence relation that produces a sequence. Say it is of the form $a_n = f(a_{n-1},a_{n-2},\ldots,a_{n-u})$. The sequence $a_1,\ldots$ is $u$-normal.

::: Problem
Given a oracle that can take input $i$ and return the $i$th element in a $u$-normal eventually periodic sequence $a$. Find the smallest lexicographic pair $(n,p)$ where $p>0$, such that $a_i=a_{i+p}$ for all $i>n$.
:::

One can solve this problem in $O(u \log \frac{n}{u})$ time. 
First, consider the subsequence $a_u,a_{2u},\ldots$. We guess an upper bound on $n$ through exponential search in the subsequence. There is a $O(u)$ time algorithm to decide if $n'\geq n$. For example using the [KMP algorithm](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm). We can quickly locate a $n'$ such that $n\in [n'-u,n']$.

Let's take the sequence $a_{n'-u},\ldots,a_{n'+3u}$. We just need to solve the following problem. Given a $O(u)$ length string, find the longest suffix that appears at least twice in the sequence. Let such suffix be $s$. We know $|s|\geq 3u$, which $s$ has to overlap with any other occurrence of the sequence. The claim is that the partial match table in the KMP algorithm would give us such information. Hence we can obtain $n$. $p$ can also be obtained in the same time. Note that KMP algorithm only uses the fact one can check equality of two elements. So the sequence can contain elements from very general space.

The total running time is $O(\log \frac{n}{u})$ calls to $O(u)$ time string matching. The total running time is therefore $O(u\log \frac{n}{u})$.

In many applications, we do not get oracle access to $i$th index of the sequence. But we can read the sequence from the beginning as a list. In that case, we don't do binary search, but linear search. Advance the index by $u$ and obtain $n'$, and test if $n$ is no larger than the current point. If so, again we have $n\in [n'-u,n']$ and reduce to the previous problem. If not, advance the index by $u$ again and repeat. This gives us a $O(n+u)$ time algorithm.
