---
title: Is the gas enough?
---

I have been trying to use the more general frameworks when I encounter dynamic programming questions, so I can demonstrate the power of abstraction later on.

{Problem}
    
    Let $D=(V,A)$ be a directed acyclic graph. $w(e)$ is the weight of an arc. A $s-t$ path $e_1,\ldots,e_m$ is called a $\alpha$-deficient path if $\sum_{i=1}^k w(e_i) + \alpha \geq 0$ for all $1 \leq k\leq m$. Find the smallest $\alpha$, such that there is a $\alpha$-deficient $s-t$ path.

One can view this problem as how much gas would one require to travel from $s$ to $t$. If one knows how much gas one can gain or lose between arcs.


Let $D(v)$ be the value of the minimum deficiency path from $v$ to $t$, and $D(t)=0$.

\[
D(v) = \min_{(v,u)\in A} \max(D(u)-w((v,u)),0)
\]

Because the graph is acyclic, there is a nice ordering allowing us to compute this nicely. 

Note this is exactly the best weight problem[@Huang08advanceddynamic] under the semiring $(\mathbb{R}\cup \{\infty\},\min,\otimes,\infty,0)$, where $a \otimes b = \max(a-b,0)$.

One should prove it's a semiring before using it. Everything seems obvious except the distributive property of the $\otimes$ operation. Here is a proof for one of the two distributive laws, the other is left as an exercise to the reader.

\begin{align*}
(a\oplus b) \otimes c &= \max(\min(a,b)-c,0)\\
&= \max(\min(a-c,b-c),0)\\
&= \min(\max(a-c,0),\max(b-c,0))\\
&= (a \otimes c) \oplus (b \otimes c)
\end{align*}

Can we extend the problem to directed graph with cycles? Yes. This semiring has the property that it is $k$-closed for any graph $G$ for $k$ depending on $G$ and $w$. It means we can't keep going though a cycle and keep producing better solutions. We can run a generic single source shortest distance algorithm[@Mohri:2002].



