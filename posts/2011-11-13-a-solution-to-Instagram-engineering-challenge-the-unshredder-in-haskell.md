---
title: A solution to Instagram Engineering Challenge, The Unshredder in Haskell
tags: Haskell
---

I have done many Haskell exercises, however, I have never done anything that have a engineering flavor, i.e. useful in real life.

I saw this challenge and believe it's the perfect chance to practice my real life Haskell skills AND get a t-shirt. [The code on github](https://github.com/chaoxu/mgccl-haskell/blob/master/random/unshredder.hs).

The problem is not well defined. Therefore I gave the following abstraction:

Each image is of a vertex. One construct a weighted directed graph and want to find a certain kind of path that visit all vertices.

What do we want to find?

The shortest path that visit every vertex? That would be TSP and NP-Hard.

However I would believe the better idea is:

Find a path that go though every vertex and minimize the maximal weight in the path.

I'm pretty sure this is also NP-complete too. Thus I will just use a greedy algorithm and assume real life data are easy.

I used a simple scheme to calculate the closeness between two strips: compare the last column of a strip with the first column of another strip. This decision come from my familiarity with lists instead of arrays. However, smarter move would be taking the average of a larger set of surrounding and see how each side deviates from it.

The function I used to calculate the difference is 

\[
\sum_{i=1}^n (\sum_{j=0}^3 |a_{i,j}-b_{i,j}|)^{1/4}
\]

Where $a_{i,j}$ is the value of the $j$th channel of $i$th pixel in the first strip's last column.

Which strip is the left most strip? Try all of them!

Running time of my algorithm is $O(n^3)$, as it cost $O(n^2)$ time to find a path with minimum weight starting from a specific vertex. Of course it can be improved to $O(n^2)$ if I pick any vertex and expand it both ways instead of just connecting things to the right.

To find the optimal strip width, it is easy if one have the following assumption:

If the strip width is $k$, the average difference between strips will be larger than strip width of $m$, where $m\neq kn$ for some $n$. Note it is possible that for strip width of $2k$ to have a higher average than $k$.

One can implement an algorithm on a DAG to figure this out. I was too tired so I just pick the top $i=5$ strip widths and computed the $\gcd$ instead.

The code is slow, I have no idea why because I'm not experienced in how to reason about efficiency in Haskell.