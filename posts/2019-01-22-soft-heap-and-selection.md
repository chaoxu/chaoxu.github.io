---
title: Soft heap and selection
tags: algorithm, data structure
---

I enjoyed the workshop [SOSA](https://simplicityinalgorithms.com/) a lot (Disclaimer: I have a paper in SOSA). The papers are simple and fun. I liked the paper [@KaplanKZZ18] the most, because I learned some really interesting tricks with [soft heap](https://en.wikipedia.org/wiki/Soft_heap).

Here I give a teaser of two things soft heap can do.

We consider a minimalistic soft heap, as there are soft heap with more operations. 

- `soft-heap(ε)`: creates an empty soft heap with error parameter $\e$.
- `insert(e)`: insert an element into the heap.
- `extract-min()`: return the element of minimum key in heap, and set of newly corrupted elements since last extraction.

The operation `insert` takes $O(1)$ time, `extract-min()` takes $O(1/\e)$ time. In this article, we can assume $\e=1/4$.

During each insertion, the key of some elements might be increased. An element is corrupted if its key in the heap is strictly greater than the original key. If there are $I$ insertions into the soft heap, then at most $\eI$ elements can be corrupted. 

Although `extract-min()` might return an element that is not necessarily the minimum, but there is a bound on the amount of possible errors. Before [@KaplanKZZ18], I don't know of any application of soft heap other than vaguely knowing about it was used for minimum spanning tree.

# Linear time selection in unordered list

We insert all elements into the soft-heap, and then we apply `extract-min` $(1-\e)n/2$ times, and find the maximum element $e$. One can see the rank of $e$ lies between $(1-\e)n/2$ and $(1+\e)n/2$. Now we can use this to remove at least $(1-\e)n/2=\frac{1+\e}{2} n$ elements, and recurse on the remaining. Once there is only a constant number of elements, use brute force. This gives us an running time $T(n) = O(n) + T(\frac{1+\e}{2} n) = O(n)$. 

# Linear time selection in heap

We are given a min heap $H$, and interested in find the $k$th smallest element in the heap.
We first insert the min element $e$ into the soft heap. 

Whenever we apply `extract-min` to the soft heap, we obtain $e$ and a set of newly corrupted elements $C$.
For each element $e'\in C$, we add the children of $e'$ in $H$ into the soft heap. If $e$ is not corrupted, we also add the children of $e$ in $H$ into the soft heap.
Once we apply `extract-min` $k-1$ times we stop. Let $S$ be the set of all elements that was inserted into the soft heap.
There are two important claims: $|S|=O(k)$ and the rank $k$ element has to be in $S$. We can use a linear time selection algorithm on $S$ once we prove the two claims. 

# Other useful results

There are some other nice results in the paper. Getting optimal results for selecting the rank $k$ element in $m$ sorted lists, and selecting $k$th element in $X+Y = \set{x+y | x\in X, y\in Y}$.
