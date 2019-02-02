---
title: No nice generalization of Gomory-Hu tree
---

[Gomory-Hu tree](https://en.wikipedia.org/wiki/Gomory%E2%80%93Hu_tree) of a graph $G$ is a weighted graph $H$ on the same set of vertices. It has the two nice properties.

1. It is a tree. 
2. For every $s,t$, there exist a min-$st$-cut in $H$ that is also a min-$st$-cut in $G$.

We could ask for a generalization. Given a graph $G$, can we find some _nice_ graph $H$ such that

For every $S,T$ such that $1\leq |S|,|T|\leq k$ and $S\cap T=\emptyset$, there is a min-$ST$-cut in $H$ that is a min-$ST$-cut in $G$.

We recover Gomory-Hu tree when $k=1$. There were some consideration of a problem similar to this [@2015arXiv151108647C]. 

There are various useful definition of _nice_ , for example sparse or bounded treewidth. 
Likely none of them would apply, because we can show that for $k=2$, there are graphs where $H$ is almost the complete graph. 

Let $G$ be the complete graph on $n$ vertices, and each edge has capacity $1$. Let $f(S)$ be the sum of the capacity of edges going across $S$. There is a unique $ST$-min cut for every $|T|=1$ and $|S|=2$. Hence this shows that $f(\{v\}) = n-1$ for every $v\in V$. If $|S|=|T|=2$, there are exactly two min $ST$-cuts, either $S$ or $T$ in $G$. Assume that $T$ is not a min $ST$-cut in $H$, then $S$ has to be the min $ST$-cut in $H$. Therefore for all $\{s_1,s_2\}$ such that $s_1,s_2\not \in T$, we have $f(\{s_1,s_2\})=2n-4$. Because $f(\{s_1\})=f(\{s_2\})=n-1$, the edge between $s_1$ and $s_2$ has capacity $1$. The complete graph on $V\setminus T$ has to be a subgraph of $H$.

# Reference