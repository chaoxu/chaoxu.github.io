---
title: Arrays and permutation
tags: Permutation
---

# Permutation, functional form

It is often where we are tasked with permuting an array. We can abstract out and ask what exactly is an array. In functional form, an array just have to support the following two operations in constant time.

1. $get(A,i)$: returns $A[i]$.
2. $set(A,i,x)$: update the array $A$ such that $A[i]$ returns $x$.

So in some sense, array is just encoding a function $f:[n]\to X$.
A permutation would be a bijective function $\pi:[n]\to [n]$.
If we are interested in applying a permutation $\pi$, then to program it is easy, we need to output a new function $g$ such that $g(i) = f(\pi(i))$. 

This allows us to apply permutations pretty easily by composing functions and cache outputs. In the purely functional view, the layout of the array in memory and the indexing can be different. 

# Permutation, physically

Sometimes one might ask to physically apply the permutation to an array. That is, the $i$th position in the array contains the element in $\pi(i)$. This is helpful because it helps with cache locality: accessing consecutive elements would be in the same location. Although if the ordering of loops does not matter, there is no harm considering the functional view. 

Often, one is tasked to apply permutation to an array physically. It usually ask for $O(n)$ running time and $O(1)$ space. Unfortunately, there is no way to obtain this running time for all permutations. There are some permutations where this is impossible. [cite]

# Mix and match

[Lingyu Xu](https://www.linkedin.com/in/lingyu-xu-9b87a565/) asked me about [Wiggle Sort](https://leetcode.com/problems/wiggle-sort-ii/), where the actual **physical layout** has to be changed, but it takes both the functional and physical view of the array. 

Let's consider the simple case where every element is distinct.  One simple solution is the following. Partition the numbers into the median, elements smaller than median, and elements larger than the median. We map the smallest $n/2$ elements into even positions, and remaining elements into the odd position. The algorithm has $O(n)$ time. The problem is how to get constant extra place. Because it is unclear how to apply the following permutation in place. The permutation $\sigma(i) = (1+2i) \% (n|1)$. 

However, one we take the functional view, the problem can be solved, physically too. 
The [answer by](https://leetcode.com/problems/wiggle-sort-ii/discuss/77677/ono1-after-median-virtual-indexing/81756) [Stefan Pochmann](http://www.stefan-pochmann.info/) shows one can simply do the mapping in place. 

What is happening in the physical location. If we applied index transform $\pi$, then apply physical permutation $\sigma$ on the index, what happens for the physical array? It is the same as applying $\pi^{-1}\sigma\pi$ to the physical array.

