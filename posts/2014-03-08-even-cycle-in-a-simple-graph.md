---
title: Even cycle in a simple graph
---

{Problem}
    $G=(V,E)$ is a simple graph, decide if there exist a cycle of even length in $G$.

The problem comes from 2014 Spring UIUC Theory Qualify Exam. 

# Characterizations
Let $\lambda(u,v)$ denote the local edge connectivity of vertices $u$ and $v$. It's the same as the largest number of edge disjoint paths connecting $u$ and $v$. $\overline{\lambda}(G) = \max_{u,v\in V} \{\lambda(u,v)\}$. Similarly, $\kappa(u,v)$ is defined as local vertex connectivity, and $\overline{\kappa}(G) = \max_{u,v\in V} \{\kappa(u,v)\}$

{Theorem}
    
    If $\kappa(u,v) \geq 3$ for some $u,v\in V$, then there exist an even cycle.

{Proof}
    There are 3 vertex disjoint paths $p_1,p_2,p_3$ from $u$ to $v$, thus it contains cycle $p_1 p_2^{-1}$,$p_1 p_3^{-1}$ and $p_2 p_3^{-1}$. One of them will have even length. 

{Theorem}
    
    If $\overline{\lambda}(G)\geq 3$, then $\overline{\kappa}(G)\geq 3$.

{Proof}
    
    If $\lambda(u,v)\geq 3$, we consider the 3 edge paths between $u,v$. Let them be $p_1,p_2,p_3$. Notice if none of them intersect at a vertex, $\kappa(u,v)\geq 3$.

    If $p_1,p_2$ intersects and has their first intersection at $v'$. Let $p_1',p_1''$ and $p_2'$ be the paths following $p_1$ from $u$ to $v'$ and $v'$ to $v$, and the paths following $p_2$ from $u$ to $v'$. $p_1' p_2'^{-1}$ is a cycle that contains $u$ and $v'$. $\lambda(u,v')\geq 3$ because there is an additional edge disjoint path $p_3 p_1''^{-1}$ from $u$ to $v'$.

    So now we can consider $u,v'$, where $\lambda(u,v')\geq 3$ and $u,v'$ is in some cycle $C$. Consider another path $P$ from $u$ to $v'$ that is edge disjoint from the cycle. Let $t$ be the first vertex where $P$ intersects $C$. Clearly, $\kappa(u,t)\geq 3$. 

It should be easy to show that

{Theorem}

    If $\overline{\lambda}(G)=2$, then all cycles in the graph are edge disjoint.

# A complicated yet obvious algorithm

1. Compute if $\overline{\lambda}(G)>2$, if so, return true.
2. Compute if there is an even cycle in linear time.

We first sparsify the graph with Nagamochi and Ibaraki's linear time algorithm that preserves the edge connectivity up to $3$. After spending $O(n+m)$ time, the graph has at most $3n$ edges. 
We want to find a global maximum min-cut in the resulting graph. Either $s-t$-min-cut is the global maximum min-cut, or $s,t$ are in the same component of the global maximum min-cut. One can modify Stoer-Wagner algorithm to find $\overline{\lambda}(G)$ in $O(nm+n^2\log n)=O(n^2\log n)$ time. Since our graph is unweighted we can do it better in $O(n^2)$ time.

Another approach is to show the graph cannot contain a [diamond graph](http://en.wikipedia.org/wiki/Diamond_graph) as a minor and use any general graph minor recognition algorithm. The fastest I know is the $O(n^3)$ one from the original Robertson–Seymour paper. There are some [discussion on cstheory](http://cstheory.stackexchange.com/questions/7928/the-complexity-of-determining-if-a-fixed-graph-is-a-minor-of-another) on what is know about this algorithm.

However we can do it better, because we can easily check graphs without a diamond as a minor has treewidth two. So we can first check if the graph have a fixed treewidth in linear time[@fixedparametertreewidth]. Next, we can then apply a linear time minor testing algorithm on graphs with bounded branch width(which is implied by bounded treewidth)[@fixedsurfaceminor]. 

The second step is computationally easy. Do a DFS, whenever we find a cycle, check if the cycle is even, delete all those edges, and keep going. The number of steps we take is at most $O(n+m)$ time. In fact, we don't really need to delete the edges once we find a cycle, as we know they will never get used by another cycle.

# A much simpler algorithm
Our algorithm above is pretty general and uses some heavy machinery. That's the kind of solution I would give during a qualify exam due to time constraints(well, always use the most powerful technique possible). 

The theorems we proved implies one nice corollary

{Corollary}

    If a graph has no even cycles, then all cycles in the graph are edge disjoint.

This kind of graph has a name, a [cactus graph](http://en.wikipedia.org/wiki/Cactus_graph). It is so special we can recognize it in linear time. A graph is a cactus if once we build a DFS tree, every vertex has at most one back edge.

This implies a extremely simple algorithm

1. Run a DFS to build a DFS tree. Let $d(v)$ be the depth of a vertex $v$ in the tree.
2. Assume there is a back edge between $x$ and $y$, check if there is any back edge end at some node in the unique path from $x$ to $y$.
3. For every vertex $v$, if it has a back edge to some vertex $u$, then check if $d(v)-d(u)$ is odd. If it is, return true.
4. Return false.

