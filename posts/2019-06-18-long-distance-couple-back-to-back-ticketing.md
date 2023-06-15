---
title: Long distance couple back-to-back ticketing
tags: Optimization, algorithm, airline
---

The reason I explored [an algorithm for covert back-to-back ticketing ](https://chaoxu.prof/posts/2019-06-15-covert-back-to-back-ticketing.html) in the previous blog post, was because as someone currently in a long distance relationship. It is important to save money on travel. 

For long distance couples, two people can fly. Consider if the couple decides be together each weekend, one can fly to the city of the other on Friday night, and comes back on Sunday night. Again we can take advantage of the back-to-back ticketing.

Let $M$ be a matching of a graph $G$ where the edges are two colored. $M$ is called _valid_ if vertex $2i$ and $2i-1$ are contained in the same colored edge in $M$. 

::: {.Problem title="Couple back-to-back ticketing problem"}
 
**Input:** A multigraph $G=(V,E)$ where $V\subseteq [n]$, where each edge can be either red or blue, and there is an edge cost function $c:E\to \R^+$. 
    
**Output:** A valid perfect matching $M$ (allowing self-loops), such that the cost is minimized. 

:::

Unfortunately, I don't see how to solve this problem in polynomial time. I could convert this problem to something that might be solved in polynomial time in the future.

Note we can split the vertices. That is, for vertex $i$, we split it into $(i,R)$ and $(i,B)$ for its blue and red counter part. A matching is _paired_ if $(2i,C)$ and $(2i-1,C)$ both has to be matched, where $C\in \set{R,B}$. The matching is _split_ if $(i,R)$ and $(i,B)$ cannot be both in the matching.

If we just enforce one property -- paired or split -- then it is solvable in polynomial time. 
Indeed, both are matching under restrictions. We are interested in finding a matching $M$, such that the vertices covered by $M$ is in some special structure. Namely $\Delta$-matroid. 
Since the paired property is a very special $\Delta$-matroid [@KakimuraT14], which was first solved in [@HefnerK95]. The second property is partition matroid, which can be handled by [hierarchical $b$-matching](/posts/2019-04-27-maximum-weight-hierarchical-b-matching.html). Reader can see each individual restriction, the problem can be turned into a maximum weight perfect matching. 

If we enforce both property, the problem is open. Although the structure is still very special, it is an [even $\Delta$-matroid](https://en.wikipedia.org/wiki/Delta-matroid).
[Naonori Kakimura](http://www.math.keio.ac.jp/~kakimura/) communicated a more fundamental problem which can be solved by this problem.

::: {.Problem title="Matching with disjoint pair constraints"}

Given a graph where the edges are partitioned into pairs. Find a maximum matching where it uses at most one edge of each pair. 

:::

This problem can be used to solve special case of rainbow matching. A graph where each edge has a color, a matching is a rainbow matching if every edge in the matching has a different color. It is known that for graphs where each color appears twice, finding a rainbow matching is NP-hard [@LeP14]. The couple back-to-back ticketing problem is also NP-hard. 