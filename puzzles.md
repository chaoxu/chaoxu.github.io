---
title: Puzzles
---
This collection has some hard problems with optimal solutions.

 - Let $x=p/q$ be an rational number, where $p$ and $q$ are integers. There is a function $f_x$, such that $f_x(y)$ tells you if $x < y$, $x > y$ or $x=y$. What is the maximum number of queries of $f_x$ needed to find $x$? http://stackoverflow.com/questions/5440688/the-guess-the-number-game-for-arbitrary-rational-numbers

 - You have a data stream of a permutation of $\{1,\ldots,n\}$. However you know $k$ of the elements will be missing because the communication channel has fault. How can you detect which numbers are missing?  http://stackoverflow.com/questions/3492302/easy-interview-question-got-harder-given-numbers-1-100-find-the-missing-numbe

 - Data structure for loaded dice? http://stackoverflow.com/questions/5027757/data-structure-for-loaded-dice

 - http://stackoverflow.com/questions/5739024/finding-duplicates-in-on-time-and-o1-space

 - When does the greedy algorithm work for the coin change problem http://graal.ens-lyon.fr/~abenoit/algo09/coins2.pd0f

 - How many distinct substrings/subsequences can there be for a string of length n over alphabet of size m.

 - In a tournament, sum of the square of number of wins are sum of square of number of loses.

 - How to test if a point is inside a polygon? What if it's a convex polygon?

 - Can one put a $a\times b\times c$ box inside a $d \times e\times f$ box, if $d+e+f < a+b+c$ ?

 - Show that the distributive law cannot distribute things for ever.

 - Randomly pick a set of $n$ elements that sum to $m$.

 - Given a inorder and postorder traversal of a Binary Tree, construct the tree using these two traversals.

 - Longest Palindromic Substring

 - There are two sorted arrays A and B of size m and n respectively. Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).

 - Given two sorted arrays A, B of size m and n respectively. Find the k-th smallest element in the union of A and B. You can assume that there are no duplicate elements.

 - Input: A long array A[], and a window width w
   Output: An array B[], B[i] is the maximum value of from A[i] to A[i+w-1]

 - There are n coins in a line. (Assume n is even). Two players take turns to take a coin from one of the ends of the line until there are no more coins left. The player with the larger amount of money wins.

 - Given a set T of characters and a string S, find the minimum window in S which will contain all the characters in T in complexity O(n).

 - Detecting a Loop in a Singly Linked List

 - Test power of 2: x && !(x & (x-1));

 - Searching an Element in a Rotated Sorted Array

 - Rotate an array in place

 - Find the k smallest elements from the set of n numbers in O(n) time complexity

 - Consider an array A such that each number appears twice. How many swaps are required such that A[2i] = A[2i+1] for all i?

 - 2D array, O(1) time find the closest element with linear big memory.

 - Putnam B3. Tournament, 2n-1 days, can one pick a winning team such that no team is picked twice?

 - A vertex is a celebrity if it has only incoming edges and no outgoing edges. Find the celebrity in a graph if know of the existence.

 - A subset of vertices is a celebrity clique, if the subset induces a clique, and only have incoming edges. 

 - Pipes, horizontal must go, n balls drop. 1 to 1. 

 - Helly property on trees.

 - Given $k$-edge-disjoint path between $u$ and $v$ and $v$ and $w$, total of $m$ edges. Find $k$-edge-disjoint path between $u$ and $w$ in $O(m)$ time.

 - Prove that there exist a $k$-edge-disjoint path in a simple graph with total length $O(\sqrt{k}n)$.

 - Given a tree, in linear time preprocess the tree, such that we can answer the following query given pointers to vertices $u,v$ and $w$: "Is the vertex $v$ on the unique path between $u$ and $w$?"

 - Least common ancestor of a tree

 - level ancestor of a tree

 - rmq

 - find center of a tree

 - diameter of a tree

 - all kind of tree/subtree isomorphisms

 - find the most common subtree (rooted ordered and rooted labeled ordered) [See this](/posts/2014-06-05-pattern-in-labeled-ordered-rooted-trees.html)

 - [Lexicographic bottleneck shortest path in undirected graph](/posts/2014-05-10-lexicographical-bottleneck-path.html)

 -  Consider a commutative semiring $(R,+,\cdot)$. $\mathbb{0}$ is the identity for $(R,+)$, and $\mathbb{1}$ is the identity for $(R,\cdot)$. Let $f,g:V\to R$, $w:V\to \mathbb{N}$ and $Z\subset \mathbb{N}$. It is common that we are interested in computing expressions of the following form. 
 \[
 \sum_{S\subset V, \sum_{x\in S} w(x) \in Z} \prod_{x\in S} f(x) \prod_{x\in V\backslash S} g(x)
 \]

 - [bisect a circle with same number of points](/posts/2014-03-27-bisect-circle-even-point-set.html)

 - Given a weighted tree(weights are small, say at most up to 1000) and $k$, find the number of vertices with distance at most $k$ from another. http://poj.org/problem?id=1741

 - Given a weighted forest. Connect the forests with edges of weight $L$ to get a spanning tree such that the diameter of the spanning tree is minimized. (Dreaming, IOI 2013)

 - static partial sum in $O(\alpha(n))$ query and $O(n)$ space/preprocessing. Yao's result.

 - For any graph of $m$ edges, we want to create a data structure, so we can test if there exist edge $uv$ in $O(m/n)$ time. (the idea is we cannot number/hash the vertices, the only way to check if two vertices are the same is to call a special oracle on the pointers.)

 - Bottleneck shortest path in $O(m)$ time if given an list that orders the edges.

 - Stable matching bipartite multigraph with incomplete preference list.

 - Reservoir sampling with weights

 - You are given $k$ stacks $S_1,\ldots,S_k$. You know everything about the stacks(you can inspect all elements in the stacks without poping them). Assume the stacks have a total of $n$ elements. You want to pop all the elements from the stacks in such a way such that $S_1.peek()\leq S_2.peek()\leq \ldots \leq S_k.peek()$ is always true. Note that we define $S_i.peek()=\infty$ if $S_i$ is empty. Come up with an algorithm to decide if this is possible.

 - given n chords on the circle(no two chords share the same points on the boundary of the circle). Each chrod is assigned a value of either 1 or -1. A value of a intersection of two chords is the product of the values of the chords containing it. Compute the sum of the value of the intersections.

 - Given $n$ arrays $\{A_1,\ldots,A_n\}$ each of size $O(n)$, assume we can compute $r(x,A_i)$ for each $i$ in constant time for all $x$, where $r(x,A_i)$ is the number of elements less or equal to $x$. Let $A = sort(concat_{i=1}^k A_i)$. Find $A[k]$ in $O(n)$ time.

 - Given $a_1\leq \ldots \leq a_k$, partition a set of $n$ numbers into $k$ sets, such that each set contains numbers of the rank between $a_i$ and $a_{i+1}$.

 - Find the maximum k cover. namely disjoint k intervals that sums to the max value. http://arxiv.org/abs/1410.2847 (with $O(n)$ time preprocessing, you can do it in $O(k)$ time each!)

 - Find the shortest subarray such that the sum is at least $k$.

 - Implement a partition refinement data structure. 

 - Isotonic regression

 - Lipchitz regression

 - Find number of subarrays that sums between a and b

 - Prove that for a non-decreasing, submodular function $f$ such that $f(\emptyset)=0$. There is a greedy algorithm to maximize a linear function with submodular constraints.

 - Let $X_A,X_B,Y_A,Y_B$ be intervals, if $A=X_A \times Y_A$ and $B=X_B\times Y_B$. Define $A\oplus_0 B = (X_A\cup X_B) \times (Y_A\cap Y_B)$ and $A\oplus_1 B = (X_A\cap X_B) \times (Y_A\cup Y_B)$. Find an algorithm such that if we are given a set of axis-aligned rectangles $S$ and a axis-aligned rectangle $R$. Device an algorithm that either find an expression using $\oplus_0$, $\oplus_1$ and rectangles in $S$, such that it covers the rectangle $R$, or return it is impossible.

 - Given a set of integers (given as a list), find the smallest natural number not in the set. (linear time, functional code, no arrays)

 - Given Integers $a_1,\ldots,a_n$, each one is either a power of 2 or a negative of a power of 2. Associated with each integer, a cost $c_1,\ldots,c_n$. Finally a integer $m$. Output $x_1,\ldots,x_n$, such that $x_i\in \{0,1\}$ such that $\sum_{i=1}^n a_i x_i = m$ and $\sum_{i=1}^n c_ix_i$ is minimized.

 - sliding window maximum

 - selection in $k$ sorted arrays

 - maintain a data structure that allows insert (object,frequency), and query a random object based on frequency. O(log n) update time, better than O(log n) query time.

 - Discrete tomography 2D

 - Given $a_1,\ldots,a_{2n}$, pair the numbers to maximize the sum of min of pairs.
 
 - Find a matching that saturates $S$, running time should only depend on $|S|$.

 - http://cstheory.stackexchange.com/questions/33857/is-two-or-zero-matching-in-a-bipartite-graph-np-complete

 - minimum cost path with gas stations

 - Given a bipartite graph G=(A\cup B, E) with weight function w on the vertices, in strongly polynomial time, one can find S a non-empty subset of B such that w(N(S))/w(S) is minimized.

 - Find minimum number of edges such that the removal increase the weight of the minimum spanning tree
.

 - Given a directed graph, find the minimum number of edges to remove so there is no arborescence.
 
 - compute minimum double cut. Namely find nonempty disjoint $Z_1$, $Z_2$ such that number of in-edges of $Z_1$ and $Z_2$ is minimized. 

 - find the minimum cut under the constraint that each side has at least $2$ vertices.

 - find the minimum cut under the constraint that each side is connected and has at least $2$ vertices.

 - $f$ is a symmetric function, find the value of min-$st$ separating set for all $s,t$, using the oracle for min-$ST$ separating set $n-1$ times.

 - Find maximum perimeter triangle.