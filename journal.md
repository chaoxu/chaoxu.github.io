---
title: Life Journal
---

# 02/13/2014

1. Matrix multiplication over a finite field seems as hard as matrix multiplication over reals.
2. If $\sum_{i=1}^n p_i/q_i=1$, $gcd(p_i,q_i)=1$, then $\lcm(q_1,\ldots,q_n) \leq \max(q_1,\ldots,q_n)^{n/2}$.
3. Matrix rounding can be solved as fast as a capacitated unweighted bipartite $b$-matching. If all numbers are non-integers then it can be solved in linear time by an greedy algorithm.

# 02/14/2014

1. When can we "contract" a set of points in a partial order and still remains a partial order.

# 02/16/2014

1. Think about how to automatically create abbreviation. *
2. Max-flow with min number of edges is NP-hard. Garey and Johnson [ND32]
3. Generalized distributive law. Are the algorithms currently the best algorithms?

# 02/28/2014

1. Convinced there is an algorithm to solve subset sum in $O(C^\frac{5}{3}\log C)$ time.

# 03/01/2014

1. http://math.stackexchange.com/questions/684992/convert-curves-to-monotone-curves-without-introduce-intersections

# 03/02/2014

1. Check out 4-strands braid groups. *
2. Is swapping rows of TUM still TUM? Yes.
3. Nearly TUM stuff

# 03/04/2014

1. Improved subset sum algorithm to $O((C\log C)^\frac{3}{2})$ time.
2. Element connectivity has a divide and conquer reduction approach. 

# 03/08/2014

1. Actually completed the Nearly TUM stuff.
2. Can finger tree replace [segment tree](http://letuskode.blogspot.com/2013/01/segtrees.html)? *
3. Global maximum min-cut can be found by modify Stoer-Wagner algorithm slightly.
4. For a simple graph $G=(V,E)$, $\max_{u,v \in V} \lambda(u,v) \geq 3$ then $\max_{u,v \in V}  \kappa(u,v)\geq 3$? $\lambda$ and $\kappa$ are the local edge/vertex connectivity.

# 03/15/2014

1. Understood Dinic's algorithm.
2. Algebraic algorithm for string reconstruction. 
3. Understood partially, the $O(nm)$ randomized algorithm for GH Tree
4. TDI and matching.
5. Vertex Connectivity takes $O(n^4)$ time to compute, but there are algorithm that runs in $O(n^{15/4})$ time for undirected graph.

# 03/16/2014

1. Complement of proper circular arc graph is a graph with circular one adjancency matrix.
2. A complete understanding of the nearly totally unimodular matrix paper by Gijswigt.

# 03/18/2014

1. Randomized algorithms are hard.

# 03/24/2014

1. Understood Hao-Orlin algorithm (kinda).
2. Figures the current polytime algorithm for 4-strand braid group conjugacy problem is just the common algorithm with a new quadratic bounds on the super summit set.

# 03/25/2014

1. Reduces [Higman's lemma](http://en.wikipedia.org/wiki/Higman's_lemma) (the string version) to Robertson–Seymour Theorem.

# 04/09/2014

1. Understood KMP

# 04/11/2014
1. Implemented KMP algorithm in Haskell

# 04/24/2014
1. Gained a little knowledge about max flow approximation through electric flows

# 04/25/2014
1. Implemented selection algorithm in a sorted matrix
2. Implemented AC automata algorithm

# 04/27/2014
1. Considered the algorithm for uniquely decodable code recognition.

# 04/28/2014
1. Thinking about algorithms on comparability/cocomparability graphs.

# 05/10/2014
1. What is the complexity to compute bottleneck linear programs?
2. How to find a s-t-path that is lexicographic the smallest? The edges are ordered, and each path is represented by the sorted list of the edges.
3. Can we generalize the above to find the lexicographic smallest flow?

# 05/11/2014
1. Figured out how to find a st-path that is lexicographic smallest on undirected graphs with one maximum spanning tree computation.
2. Can lexicographic flow modeled by dynamic flow?
3. Min-cost flow on directed, unit length, unit capacity graph can be solved in $O(\min(m^{1/2},n^{2/3})m\log n)$.
4. Maybe I can improve things in the applied fields? say search "a polynomial time algorithm" on IEEE Xplorer.

# 05/12/2014
1. Review randomized algorithm.

# 05/13/2014
1. $co-NEXP \subset NEXP/(n+1)$.
2. undirected simple unit capacity graph with some vertices with vertex capacity $1$ and remaining with infinite capacity. Does there exist a max flow algorithm for this problem that runs in $O(m\sqrt{n})$ time? For the case where all vertex capacity is $1$ or all vertex capacity is infinity this is achievable, but through different means.

# 05/17/2014
1. A graph have a length $k$ path if and only if it has a length $k$ path as a minor.
2. Minor containment problem can be solved in $O(n^3)$ time, maybe there exist a faster algorithm. 

# 05/22/2014
1. realized one can check if any prefix of $c$ is a suffix of $u$ and find all the suffix of $u$ where $c$ is a prefix to by building only a suffix tree for $u$.