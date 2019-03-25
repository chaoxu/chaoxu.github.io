---
title: Induced subgraph with constant diameter
---

Let $T$ be a rooted tree on vertices $V$. $T_v$ to be the subtree of $T$ rooted at $v$. Let $\mathbb{G}_T^k$ contains all graphs $G$ on $V$ with the property that for every $v$, the diameter of $G[V(T_v)]$ is at most $k$.

Let $G_T^k$ to be the graph in $\mathbb{G}_T^k$ with minimum number of edges(break ties arbitrarily), what can we say about the number of edges in $G_T^2$?

{Theorem}
	
	If $T$ has $n$ vertices, then $G_T^2$ have $O(n \log n)$ edges. 

{Proof}

	Let $|G|$ to be the number of vertices in $|G|$. 

	We construct a graph $G\in \mathbb{G}_T^2$ recursively. For each $T_v$, we construct a $G_v$ on the same vertex set such that it has the property there exist a vertex that connects to all other vertex in $G_v$. Call such vertex a golden vertex of $G_v$. $G_v$ would be a union of all $G_u$, where $u$ is a child of $T_v$ with some extra edges. Let $u$ to be the child of $v$ such that $G_u$ has the maximum number of vertices (break ties arbitrarily), and $u'$ is the golden vertex of $G_u$, then we add edges from $u'$ to all other vertices in $G_v$. Let $G=G_r$, where $r$ is the root of $T$.

	The distance between any two vertex is at most $2$ in $G[V(T_v))]=G_v$ for all $v$, because any two vertex is connected by a golden vertex in the induced subgraph.

	The number of edges is at most $O(n\log n)$. Label $v$ with $|T_v|$. The number of edges added at the step to build $G_v$ is $|T_v|-|T_u|$, where $G_u$ contains the golden vertex, and it also have the most number of vertices among all $G_c$ where $c$ is a child of $v$ in $T$. Sum of all labels over the tree $T$ except the heaviest child is $O(n\log n)$ using analysis akin to heavy-light decomposition. 

This bound is in fact best possible by considering a complete binary tree and the following lemma:

{Lemma}

	If $G=(V,E)$ is a graph with diameter $2$, then for any cut $\delta(U)$, we have $|\delta(U)|\geq min(|U|,|V\setminus U|)$.

{Proof}
	
	wlog, let $|U|\leq |V\setminus U|$. If all vertex in $U$ has an neighbor in $V\setminus U$, then $|\delta(U)|\geq |U|$ and we are done. Otherwise, there is an vertex $u\in U$ has no neighbor in $V\setminus U$, then for any $v\not\in U$, there is an $u'\in U$, and there is a path $uu'v$. This implies there are at least $|V|$ edges in $\delta(U)$. 

Is this the best possible for general $G_T^k$ too? Not at all. To show this, we introduce an algorithmic problem.

{Problem}
	Preprocess a rooted tree $T$ where the elements in the tree $T$ has weights from a semigroup $(S,+)$, such that we can answer the following query in $k$ semigroup operations. 

	**Query Input**: $x$ and $y$ where $x$ is an ancestor of $y$.

	**Query Output**: The sum of weights of the unique simple path between $x$ and $y$.

Alon and Schieber showed this problem can be solved by precompute $O(n\lambda(k,n))$ path sums [@Alon87optimalpreprocessing]. Where $\lambda(k,n)$ is related to the inverse Ackermann function, and $\lambda(4,n)=O(n\log^*n)$. 

{Theorem}
	
	If $T$ has $n$ vertices, then $G_T^k$ have $O(n \lambda(k,n))$ edges.

{Proof}
	A simple reduction from our tree problem by input our rooted tree $T$ into Alon and Schieber's algorithm. For each path sum they preprocess, we create an edge in our $G$. The diameter of all $G_v$ is at most $2k$, since for any 2 vertices $x$ and $y$, we find it's lowest common ancestor $w$ in $T$, then there is a path of length $k$ from $x$ to $w$ and another of length at most $k$ from $w$ to $y$. 

Their paper argued an lower bound of preprocess $\Omega(n\lambda_k(n))$ path sums for a single rooted path. In our model, we can add $O(n)$ edges is sufficient. An edge from the leaf to every other vertex, making the diameter only $2$. This is because we have a lot more freedom in our diameter problem. 

In their problem, their path sum is actually divide the path into $k$ subpaths, our path of length $2$ would imply there is an overlap in the sum.

One might to think the reason we can do it in $O(n)$ edges comes directly from in the undirected model we can traverse back from the leaf and allow overlaps. Maybe the following problem might give us an lower bound. 

{Problem}
	Preprocess a rooted tree $T$ where the elements in the tree $T$ has weights from a semigroup $(S,+)$, such that we can answer the following query in $k$ semigroup operations. 

	**Query Input**: $x$ and $y$ where $x$ is an ancestor of $y$ and $y$ is a leaf.

	**Query Output**: The sum of weights of any walk lies strictly in between $xy$ and that start at $x$ and ends at $y$.

Unfortunately, this does not give us an lower bound. Following the proof in Alon's and Schieber's paper, we can show any algorithm would need to precompute at least $O(n\lambda_k(n))$ paths in order to output the solution using $k$ semigroup operations on this particular tree: $T$ is a rooted tree consist of one single path of length $n$, and for each vertex, we add one single dangling leaf. 

Again, for this graph, we can add at most $n$ edges and force diameter to be at most $2$. Take the lowest leaf and add edge to all other vertices. Here would be an formulation that could result an lower bound.

{Problem}
	Preprocess a rooted tree $T$ where the elements in the tree $T$ has weights from a semigroup $(S,+)$, such that we can answer the following query in $k$ semigroup operations. 

	**Query Input**: $x$ and $y$ where $x$ is an ancestor of $y$.

	**Query Output**: The sum of weights of any walk from $x$ to $y$ that lies strictly inside the subtree rooted at $x$.

Finally, here is a conjecture. Instead of using tree, we use Laminar family to simplify the wording of the conjecture. 

{Conjecture}
	For every constant $k$, there exist a Laminar family $X$ over ground set $V$ of $n$ vertices, such that for any graph $G=(V,E)$ such that $G[A]$ has diameter at most $k$ for all $A\in X$, then $|E|=\Omega(n\lambda_k(n))$.

If diameter is $3$, then we can show there is a $\Omega(n\log \log n)$ lower bound by consider the Laminar family such that a set of size $n$ gets partitioned into $\sqrt{n}$ children each with $\sqrt{n}$ vertices, and it is applied recursively.

The idea is at any level, say has $n$ elements and $\sqrt{n}$ subsets. A vertex is called interior if it does not connect to any vertex outside the subset, and exterior otherwise. A subset is shy if it contains a interior vertex. We show there is at least $1/5 n$ edges in between the subsets. If there are at least $1/2n$ exterior vertices, then we are done. Otherwise, there are at lease $1/2n$ interior vertices. We call the subsets with at least one interior vertex shy. At least $\sqrt{n}/2$ of those subsets are shy. Consider any two distinct shy subset and pick a interior vertex from each. In order to have a distance 3 path between two interior vertices, the path has to contain two exterior vertex, one in each shy subset. But this shows if we contract all the shy subsets to one vertex, we get a clique. This means there has to be at least $(\sqrt{n}/2-1)^2>1/5n$ edges in between the subsets (for large enough $n$). 

