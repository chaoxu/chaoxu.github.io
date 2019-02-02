---
title: Partial order under contraction
---

# Real Life Problem
I want to make a program that allow users to maintain some information that has some inherent partial order structure. Each information can be represented by a node, and there are edges going between nodes. One of the operations is grouping multiple nodes and mark them as equivalent. Equivalent nodes can be shrink into one super node, and only expand it when the user want to inspect the inside. However, we still want a partial order in the global view. It would be nice to characterize when it is possible.

# Formal Formulation
$(P,\leq)$ be a partial order. Define $U(X) = \{ p| x\leq p, x\in X, p\in P\}\backslash X$, $D(X) = \{ p| p\leq x, x\in X, p\in P\}\backslash X$.

Let $Z\subset P$ and $Q=P\backslash Z \cup \{q\}$ where $q\not\in P$. We define an contraction operation $C(P,Z)=(Q,\preccurlyeq)$, such that

\[
a\preccurlyeq b \Longleftrightarrow a\leq b \text{ or } (a\in D(Z)\cup \{q\} \text{ and } b\in U(Z)\cup \{q\})
\] 

If we interpret $(P,\leq)$ as a directed graph, then this operation is contracting the vertices in $Z$ into one vertex $q$, and then take the transitive closure.

There is a cute characterization of when $(Q,\preccurlyeq)$ is also a partial order.

{Theorem}

    $C(P,Z)=(Q,\preccurlyeq)$ is a partial order if and only if $D(Z)\cap U(Z) = \emptyset$. In particular, the map \[ f(x) = \begin{cases} x  & x\in P\backslash Z \\ q & x \in Z\end{cases} \] is order-preserving.

{Proof}
    
    If $x\leq y$, then $f(x)\preccurlyeq f(y)$ by definition. We just have to prove $(Q,\preccurlyeq)$ is a partial order.

    If $D(Z)\cap U(Z) \neq \emptyset$, let $x\in D(Z)\cap U(Z)$. There is $z,z'\in Z$ such that $z\leq x\leq z'$. $x \preccurlyeq q$ and $q \preccurlyeq x$ but $x\neq q$. $(Q,\preccurlyeq)$ is not a partial order.

    If $D(Z)\cap U(Z)=\emptyset$,

    1. For all $x\in Q$, $x\preccurlyeq x$.

    2. If $x\preccurlyeq y$ and $y\preccurlyeq z$ then $x\preccurlyeq z$. This can be shown by a few case work from the definition.

    3. $x\preccurlyeq y$, $y\preccurlyeq x$, then $x=y$. 
       
        - $x\leq y$, then $y\leq x$. Assume not, then $y\in D(Z)\cup \{q\}$ and $x\in U(Z)\cup \{q\}$. $x\in D(z)$ because $x\leq y$. This implies $x\in D(Z)\cap U(Z)$, a contradiction. Thus $x\leq y$ implies $x=y$. 
        
        - If $x\in D(Z)\cup \{q\}$ and $y\in U(Z)\cup \{q\}$, and we also have $x\in U(Z)\cup \{q\}$ and $y\in U(Z)\cup \{q\}$. This shows $x=q=y$. 
        
        - By symmetry, it take care of the remaining cases.

Again, if we interpret this in a graph theoretical sense, a contraction of a set of vertices $X$ doesn't introduce directed cycles if and only if the set of vertices reachable from $X$ and can reach $X$ are a subset of $X$. 