---
title: Pattern in Labeled Ordered Rooted Trees 
tags: algorithm
---

{Problem}
    Let $T$ be a rooted ordered labeled tree. Find all the vertices where all it's subtrees are equal.

Let $T(v)$ to denote the subtree rooted at $v$. The two trees are equal if they have the same shape and the same label. 

# Reduce to a string problem

This problem is interesting because one solution can demonstrate the technique of linearize the ordered tree to a string, and apply string algorithms.

First, we replace every edge in the tree with two directed edges $uv$ and $vu$, where $u$ is closer to the root than $v$. We label $uv$ with $($ and $vu$ with $)$. This will be the new tree we work with.

Let $s(T)$ be a string defined by concatenating the labels on the path by traverse the tree with an euler tour by following the edges in a DFS like manner starting from the root of $T$.

Note $s(T)$ would be a balanced set of parenthesis when $l$ maps vertices to the empty string. Indeed, there is a bijection between unlabeled ordered trees and balanced parenthesis. It's not hard to see this generalizes to the labeled setting.

{Theorem}
    If $s(T)=s(T')$, then $T=T'$.

A run in a string is a maximal string of the form $a^nb$, where $b$ is a prefix of $a$ and $n\geq 2$. The runs theorem states there are at most $O(n)$ runs, and all of them can be found in $O(n)$ time[@Crochemore_computinglongest]. Let $a^nb$ be a run in a string, then we call the $a^n$ part the complete repetitions.

Define the vertices with at least $2$ child and all it's subtrees are equal as good vertices. It's easy to $s(T(v))$ for some good vertex $v$ is going to be a complete repetition! 

Now if we found a run, it's easy to check if it actually correspond to a good vertex in $O(1)$ time once we did a $O(n)$ time preprocessing.

This allows us to solve the problem in $O(n)$ time.

Alternatively, there is a paper with a linear time algorithm to find all subtree repeats inside a tree[@subtreerepeat].

# Reference
