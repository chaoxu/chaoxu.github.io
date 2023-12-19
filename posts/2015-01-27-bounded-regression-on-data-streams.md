---
title: Bounded regression on data streams
tags: flow, circulation, algorithms
---

# Bounded Regression on Data Streams

[Hsien-Chih](http://web.engr.illinois.edu/~hchang17/) sent me [this problem](http://www.careercup.com/question?id=5207197178920960). [Similar problem has been asked on Quora](http://www.quora.com/Given-an-integer-array-what-is-the-algorithmic-approach-to-find-minimum-adjustments-such-that-the-absolute-difference-between-the-adjacent-elements-is-within-target-value). He noticed it might be solved in near linear time using min-cost circulation. Here we show a generalization.

::: {.Problem title="Bounded Regression on Data Stream"}
  Given 

  1. $(a_1,\ldots,a_n)\in \R^n$,
  2. $(w_1,\ldots,w_n)\in \R^n_+$,
  3. $(l_1,\ldots,l_{n-1})\leq (u_1,\ldots,u_{n-1}) \in \R^n$.

  Output $(x_1,\ldots,x_n)\in \R^n$ such that $l_i \leq x_{i+1}-x_i\leq u_i$ for all $1\leq i<n$, and minimize $\sum_{i=1}^n w_i |a_i-x_i|$.
:::
# Reduce the problem to min-cost circulation

It's natural to model this problem as variations of min-cost circulation problem on a graph.

The graph $G=(V,E)$ with vertices $V=\{s,v_0,\ldots,v_n\}$.

Edges:

1. Edge $v_iv_{i+1}$ for all $0\leq i <n$.
2. Edge $sv_i$ for all $0\leq i\leq n$.

Edge Capacity:

1. $sv_i$ has lower bound $l_i$, upper bound $u_i$ for all $1\leq i\leq n-1$.
2. All other edges are uncapacited. Namely lower bound and upper bound are $-\infty$ and $\infty$ respectively.

Edge Costs: $v_{i-1}v_i$ has cost function $c_i(x)=w_i |a_i-x|$. Cost function on other edges are $0$.

A function $f$ is called a circulation if $\sum_{e\in \delta^+(v)} f(e)-\sum_{e\in \delta^-(v)} f(e)=0$ for all vertex $v$. It is feasible if $f(e)$ is within the capacity. It is min-cost if $\sum_{e} c_e(f(e))$ is minimized. 

Solving the min-cost circulation problem would give us the desired $x_i$ by setting $x_i=f(v_{i-1}v_i)$.

# min-cost circulation on series-parallel graphs

The constructed graph is a two terminal [series-parallel graph](http://en.wikipedia.org/wiki/Series-parallel_graph). There is a simple procedure to solve min-cost flow problem on series-parallel graphs. Consider a series connection of two edges, each with cost function $f$ and $g$. We can replace it with an edge with cost function $f + g$. If it is a parallel connection, then we can replace it with one edge and a cost function $f~\square~g$, where $\square$ is the infimal convolution: $(f~\square~g)(x)= \inf_y f(x-y) + g(y)$. 

Once we have a good data structure to represent the costs, we can reduce the graph to one single edge easily, and find the minimum cost circulation. In particular, if the cost are continuous, convex and piecewise linear in a interval and $\infty$ everywhere else, and the total number of breakpoints is $n$, then Booth and Tarjan has an algorithm that runs in $O(n\log n)$ time [@Booth1993416].

Because all edge has a cost function with at most $1$ breakpoint. The bounded regression problem can be solved in $O(n\log n)$ time. 

# Isotonic regression

We can try to minimize $\sqrt{\sum_{i=1}^n w_i (a_i-x_i)^2}$ instead ($L_2$ error). It is a generalization of the lipschitz isotonic regression problem [@ISI:000279661700033] when $l_i=0$ and $u_i=u$ for some constant $u$. We can also ask to minimize the $L_\infty$ error.

If the upper bounds are $\infty$ and all lower bounds are $0$, then the problem is called the isotonic regression problem. I have solved [a interesting problem using isotonic regression](http://cs.stackexchange.com/questions/41519/efficient-algorithm-for-this-optimization-problem-dynamic-programming/).

We can express all the problems as min-cost circulation problem on a appropriate graph. If the min-cost circulation algorithm on those graphs have the same running time as current best algorithm, it would imply something more general is acting in the background. 

Here is what we know.

1. $L_1$ error: This post shows it can be solved in $O(n\log n)$ time using the min-cost circulation formulation. It matches the running time of specialized algorithms.
2. $L_2$ error: It can be solved in $O(n)$ time, but doesn't come from the quadratic cost min-cost circulation formulation. 
3. $L_\infty$ error: It can be solved in $O(n)$ time. However, it doesn't come from the minimax circulation problem. (In the minimax circulation, the cost is the largest edge cost incurred by the circulation).
4. $L_0$ error: It can be solved in $O(n\log n)$ time. This is equivalent to the longest non-decreasing subsequence problem. 

This prompt the following two natural problems: 

1. *Can min-cost circulation with quadratic cost on series parallel graph have $O(n)$ time solution?*
   This is in fact possible when all edges have no capacity[@Zohar2007691]. But with capacity, even for a edge with a lower bound of $0$ and $0$ cost, we don't know.

2. *What about minimax circulation?* We can't find any study of minimax circulation on series-parallel graphs. 
