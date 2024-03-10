---
title: The related employer controlled group
tags: tax, 401(k)
---

Disclaimer: Not an accountant nor a lawyer.

For employees in the US, employers (not just the employee) can contribute to the employee's retirement fund 401(k). There is an maximum limit. It is $M = \$61,000$ for 2022.

If a person works for multiple *unrelated* employers, each employer, in theory, can contribute $M$ to their 401(k). This is because such limit is based on the plan, and not based on the person. 

See [Michael Kitces's excellent article about the problem](https://www.kitces.com/blog/coordinating-contributions-multiple-employer-sponsored-defined-contribution-plans-401k-defined-benefit/).

Kitce's article outlined there are two ways to determine if some entities are related. Here we focus on the easier one, the controlled group. Another good source for controlled groups is [betterment's article](https://www.betterment.com/401k/resources/controlled-groups). 

We will be very specific about the statement, what is *unrelated*? Unfortunately. What IRS defined as controlled group does not give us a equivalence relation. So here is an overview of what is actually happening.

The IRS produce rules that group companies into sets of what is called controlled groups. The idea is these companies are controlled by the same entity. So they could as well be made into a single company. So if they are a single company, they should not be contributing more than $M$ to a employers 401(k).

However, there are overlap of the groups. So a partition must be created. This partition, which I call it the controlled partition, need to have the property that each partition class is a subset of some controlled group. 

[The actual code states what to do with the overlaps](https://www.law.cornell.edu/uscode/text/26/1563).

> The determination as to the group of which such corporation is a component member shall be made under regulations prescribed by the Secretary which are consistent with the purposes of this part.

If a person works for companies that spans $k$ different classes of the controlled partition, then the employers in each partition class can contribute at most $M$ to the person's 401(k).

Now, we want to distill the information to a mathematically formal one.

Consider we have a directed graph. The edges has a weight function $w:V\times V \to [0,1]$. Note for simplicity of exposition, $w(a,b)$ for an edge that doesn't exists is defined as $0$. For each vertex, it satisfies sum of the weight of in edges is at most $1$.

The intuition is $w(a,b)$ means the proportion of $b$ that is owned by $a$. 

There is a special subset of vertices $U$. It correspond to individual, trusts and/or estates.

We define a few terms.

1. A set $S$ of entities is a P-set, if $S$ have the following property. 
  - there exists a special vertex $p\in S$, called the parent.
  - $\sum_{u\in S} w(uv) \geq 0.8$ for all $v\in S\setminus \set{p}$.
  - $w(pv)\geq 0.8$ for at least two distinct vertices in $S\setminus \set{p}$. 
2. A set $S$ is a B-set, if $|S|\geq 2$, and there exists a set $T\subseteq U$ such that $|T|\leq 5$, and 
  - $\sum_{t\in T} w(tv)\geq 0.8$ for each $v\in S$,
  - $\sum_{t\in T} \min_{v\in S} \set{w(tv)} \geq 0.5$.
3. A set $S$ is a C-set, if $|S|\geq 3$ and 
 - for each $v\in S$, $v$ is in a P-set contained in $S$, or a B-set contained in $S$.
 - at least one vertex $v\in S$ is a parent vertex of some P-set contained in $S$, and is also a vertex of a B-set contained in $S$.


The P-set, B-set and C-set reflects Parent-Subsidiary Controlled Group, Brother-Sister Controlled Group and Combined Controlled Group, respecitvely. Note I'm still simplfying here. There are some attribution problems. For example, someone could own a trust which owns a company, and this has to be taken into consideration for the controlled groups.

Here I found a few useful facts.

1. It's possible that two B-set $A$ and $B$ intersects, but $A\cup B$ is not a B-set:

Consider there are two sets of size $5$ in $U$, $T_1$ and $T_2$. $|T_1\cap T_2| = 1$.

Consider two sets $A = \set{a,c}$ and $B=\set{b,c}$.
For each vertex $v\in T_1\setminus T_2$, there is an edge $va$ with weight $7/40$, and edge $vc$ with weight $1/20$. 
For each vertex $v\in T_2\setminus T_1$, there is an edge $vb$ with weight $7/40$, and edge $vc$ with weight $1/20$. 
For $v\in T_1\cap T_2$, there is an edge $vc$ with weight $3/5$, $va$, $vb$ with weight $3/10$.

The claim is that $A$ and $B$ are both B-set, but $A\cup B$ is not. This is because no subset of $T_1\cup T_2$ with size no larger than $5$ can make sure incoming weights for each vertex in $A\cup B$ is at least $0.8$.


2. $A\subseteq B$ and $B$ is a B-set, then $A$ is a B-set if $|A|\geq 2$.

3. Two P-sets $A$ and $B$ such that $A\cap B\neq \emptyset$, then $A\cup B$ is a P-set. 

This mean there is a unique maximal P-set containing a particular vertex.

4. If any set is a C-set, then the union of all P-sets and B-sets is a C-set.

Looking at the letter of the law, any C-set together with some other P-set and B-set is still a C-set. This means two companies that is not related in any way can be classified to be related.