---
title: Network Transformations and Applications
---

There are so many variations of what minimum cost flow means. Sometimes an algorithm might state it only works if the costs are all positive. Here are some of the transforms, many are just from section 2.4 of [@Ahuja:1993:NFT:137406].

In the most general formulation of the minimum cost flow problem

1. Each vertex has a balance constraint.
2. Each edge has lower bound(demand) and upper bound(capacity).

A flow is feasible if it's within lower bound and upper bound and balance constraint must be satisfied at all vertices. 

Min-cost flow problem finds the flow with minimum cost. Min-cost circulation is the min-cost flow problem with all balance $0$. Min-cost transshipment is the min-cost flow problem with no edge capacities. (But there is a lower bound of $0$ on all edges).

# Remove all balance
This shows min-cost flow is equivalent to min-cost circulation. A common way to remove balance is add an new vertex $s$. For each vertex $v$ with balance $b_v$, add edge $sv$ with lower and upper bound both $-b_v$.

However, this might make the graph no longer have certain structural properties. For example, the graph might no longer be planar. 

One way to do this is solve two problems. First compute a spanning tree $T$ of the graph. Add parallel edges from $T$ to $G$. Find flow only on those parallel edges(so it's just a tree) that satisfies all the balance constraints. Let this flow be $f$. Make sure the cost of the flow on the added edges are infinity, and consider the residue graph. Now, solve the min-cost circulation on the residual graph and then sum with the flow $f$ gives one the desired result. This idea was first used for max flow problem with balance constraints in [@Miller:1995:FPG:214022.214026].

# Remove upper and lower bounds
This shows min-cost flow is equivalent to min-cost transshipment problem.
All operations are local, therefore we establish the notation. There is an edge $st$ with lower bound $l$, upper bound $u$, cost $c$. $b_s$ and $b_t$ are the balances of $s$ and $t$, respectively. 

## Remove non-zero lower bound

The idea is we just send the flow of value $l$ along the edge! We transform so the new balance for $s$ is $b_s - l$, new balance for $t$ is $b_t+l$, and the new upper bound on the edge is $u-l$. The low The cost doesn't change. 

## Remove upper bounds
Here we assume $l=0$. If not, first remove the non-zero lower bound.
We create a new node $w$ with balance $-u$.
We remove the edge $st$, and split it into $sw$ and $tw$ instead. $sw$ has infinite capacity and cost $c$. $tw$ has infinite capacity and cost $0$. Finally, let the balance of $t$ be $b_t+u$.

The idea is instead of sending flow on $st$, now we send on $sw$. Because the balance is $-u$, we can't send more than $u$ unit of flow. To cover the remaining balance, some flow is sent from $t$ to $w$. 

# Applications

Let $C$ be a class of graphs. Let $Sub(C)=\{G| G\subset H, H\in C\}$ be the set of subgraphs of graphs in class $C$. If there is an algorithm that solves min-cost circulation/transshipment problem for $C$, then there is an algorithm that solves min-cost flow problem on $Sub(C)$. The running time of the algorithm is only the extra time spent on complete the graph to some graph in $C$ by adding edges of $0$ capacity. 

This directly implies min-cost flow on general series-parallel graphs can be solved in $O(n\log n)$ time using the algorithm for min-cost circulation on $2$-terminal series-parallel graphs(Note the article solves the min-cost maximum flow problem, one can see it also works for min-cost circulation)[@Booth1993416]. As a corollary, one can solve min-cost flow problem on outerplanar graph in the same running time.

