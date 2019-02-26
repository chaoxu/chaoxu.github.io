---
title: A Reviewer Assignment Problem
tags: algorithm, matching
---

Consider there are some reviewers and some papers. Each reviewer can review exactly one paper, and each reviewer is qualified to review some subset of papers. 
We are interested in maximizing the number of papers reviewed by at least $k$ reviewer, then under that constraint, maximize the paper reviewed by $k+1$ reviewer, etc. This make sure we are being fair in evaluating papers. It would try to avoid the case where most paper getting small number of reviews and a few papers getting unreasonable number of reviews. 

Formally, we are given a bipartite graph $G=(A,B,E)$ of $n$ vertices and $m$ edges. A subset of edges $M$ is called a semi-matching, if $\deg_M(v)=1$ for all $v\in A$.
For a subset of edges $M$, let $g_M(i)$ to be the number of vertices in $B$ with degree at least $i$. We want to find a semi-matching $M$, such that $(g_M(k),g_M(k+1),\ldots,g_M(n))$ is lexicographically maximum.

When $k=1$, if $M$ minimizes the sum of the function $\sum_{v\in B} f(\deg_M(v))$ for any strictly convex increasing function $f$, then $(g_M(1),g_M(2),\ldots,g_M(n))$ is lexicographically maximum. The problem therefore can be reduced to min-cost flow can be applied here directly, and obtain a polynomial time algorithm [@HarveyLLT06]. 

When $k=3$, the problem is NP-hard, since it would imply we have to maximized $g_M(3)$, and this is already NP-hard because exact cover by $3$-sets. That is, given a collection of sets of size $3$ each. Decide if there exists a subcollection that forms a partition of the universe. 

The only unresolved case is $k=2$. Interestingly, we can show it is also polynomial time solvable. First, one can see that maximizing $(g_M(2),g_M(3),\ldots,g_M(n))$ is equivalent to minimize $\sum_{v, \deg_M(v)\geq 2} f(\deg_M(v))$ for some strictly convex increasing function $v$, and $M$ range through all semi-matchings so each vertex in $B$ has degree exactly $2$ (Assuming it exists). 

Apollonio and Sebő shown the following problem is polynomial time solvable [@ApollonioS09].

{Problem}
    Given a graph $G=(V,E)$, a integer $k$, convex functions $f_v:\N \to \R$ for each $v\in V$, and an edge cost function $c:E\to \R$. One can find the following in polynomial time.
    $$\min \left\{   \sum_{v\in V} f_v(\deg_M(v)) + \sum_{e\in M} c(e) \middle| M\subseteq E, |M|=k  \right\}$$

It's not hard to generalize it a bit further by requiring $M$ to respect some upper and lower bound on the vertices. Indeed, we can let $f_v:\N\to \R\cup \set{\infty}$, and set $f_v(x)=\infty$ if $x$ is not between the upper and lower bounds. 

Now, we are going to reduce the problem. The reduction is similar to the [one for $2$-or-$0$ matching](https://cstheory.stackexchange.com/questions/33857/is-two-or-zero-matching-in-a-bipartite-graph-np-complete/33859). 

For each vertex $v\in B$, split into two vertices $v_1$ and $v_2$. Define $B_i=\set{v_1|v\in B}$.
The new input graph consists of vertices $B_1\cup B_2 \cup A$.
$v_1$ and $v_2$ connects to the same vertices as $v$. We add an edge between $v_1$ and $v_2$, with very large cost $C$. Say $C=mn^2+1$.
$v_1$ has both an upper and lower bound of $1$. $v_2$ has a lower bound of $1$. For each vertex in $A$, add an upper and lower bound of $1$.
We have a strict convex function $f_{v_2}(x)=x^2$ on each vertex $v_2$. 

Let $r=|A|$, $p=|B|$. We solve [Problem 1] repeatedly for each $k$ from $r$ to $r+p$. 

Say there exists an optimal solution to the original problem with exactly $t$ vertices in $B$ with degree smaller than $2$. Find the optimal solution to the new problem with $k=r+t$. Let it be $M'$.
We obtain $M$ from $M'$ by identify pairs of vertices $v_1$ and $v_2$. $M$ would be the solution to the original problem.

# Extensions

I first heard of the problem from [@YesilcimenY19], where they focused on $k=1$ case, but the reviewer can review more than one paper. This case can be handled easily. 
We can add degree upper and lower bounds to all vertices, and only look at subgraphs in $M$ that satisfies the upper and lower bounds. That is, we can also make sure no reviewers review too many papers too. Under that constraint, find $(g_M(k),\ldots,g_M(n))$ lexicographically.
This is possible but more tricky, as we have to do some reduction from capacitated $b$-matching to $b$-matching. 

There is a little more generalization. Assume for each paper, we have a lower bound of reviews $d_v$. That is, it has to be reviewed by at least $d_v$ person to be useful. So translating to the graph case, we can impose the constraint that $\deg_M(v)=0$ or $\deg_M(v)\geq d_v$. 
One can see maximizing $(g_M(2),g_M(3),\ldots,g_M(n))$ is equivalent to maximizing $(g_M(1),g_M(2),\ldots,g_M(n))$ where $d_v=2$ for all vertices. Again, one can modify the reduction to handle the case when $d_v$ is either $1$ or $2$.