---
title: Lexicographic Bottleneck Shortest Path in Undirected Graphs
tags: algorithm
---

# Lexicographic Bottleneck Ordering

Let $X$ be a totally ordered set. Let $l(S)$ be the sorted sequence of all the elements in $S$, where $S\subset X$. We can induce an total ordering on the subset of $X$.

{Definition}
    
    $S\preccurlyeq T$ for $S,T\subset X$ if $l(S)\leq l(T)$ in lexicographic ordering. $\preccurlyeq$ is called the lexicographic bottleneck ordering.

{Lemma}

    For nonempty sets $A$ and $B$, if $A\preccurlyeq B$ , then $A - min A \preccurlyeq B - min B$. Also if $A\preccurlyeq B$ then $A \preccurlyeq A\cup B \preccurlyeq B$.

{Theorem}

    $A\preccurlyeq A'$, $B\preccurlyeq B'$ then $A\cup B \preccurlyeq A'\cup B'$

{Proof}

    Let $C=A\cup B$ and $C' = A'\cup B'$. 

    Since $\preccurlyeq$ is a total order, we can prove it by showing if $C'\preccurlyeq C$ then $C' = C$.

    We prove it by structural induction on $C'$. The base case when $C' = \emptyset$ is trivial, since it must mean $A=B=A'=B'=\emptyset$.

    Consider $c' = \min C', c = \min C$. $c'\leq c$ in order for $C'\preccurlyeq C$. But we know $c\leq c'$. This shows $c=c'$. Given that $c=c'$, $A'\cup B' = A\cup B$ if and only if $(A'-c)\cup (B'-c) = (A-c) \cup (B-c)$.

    First, we show that $A-c \preccurlyeq A'-c$. 

     1. Assume $c\in A$, then $c\in A'$, so by previous lemma this is true. 
     2. If $c\not\in A$ and $c\not \in A'$, then $A-c=A\preccurlyeq A'=A'-c$.
     3. If $c\not \in A$ but $c\in A'$, then this implies $A$ is empty, and $\emptyset \preccurlyeq A'-c$.

    Similarly, $B-c\preccurlyeq B'-c$.

    Second, we need to show that $(A'-c)\cup (B'-c) \preccurlyeq (A-c) \cup (B-c)$. This is obvious because $C'\preccurlyeq C$ implies $C'-c \preccurlyeq C-c$.

    By the inductive hypothesis, $(A'-c)\cup (B'-c) = (A-c) \cup (B-c)$, thus completes the proof.

The theorem intuitively tells us how to partition a set into smaller sets.

# Lexicographic Bottleneck Path

Given a undirected graph $G=(V,E)$, and an ordering of the edges $e_1,\ldots,e_m$. Let $w(e_i)=i$.

{Problem}(Bottleneck Shortest Path)
    
    Find a $st$-path that maximizes the minimum edge weight on the path. 

Any $st$-path that maximizes the minimum edge weight over all $st$-paths is a $st$-bottleneck shortest path(BSP). We are interested in a more general version of this problem. Find a path from $s$ to $t$ that is maximum with respect to the lexicographic bottleneck ordering $\preccurlyeq$ of the path. 

{Problem}(Lexicographic Bottleneck Shortest Path)

    Find a $st$-path $P$ such that $P'\preccurlyeq P$ for all $st$-path $P'$.

The unique $st$-path that is maximum in lexicographic bottleneck order among all $st$-paths is called the $st$-lexicographic bottleneck shortest path(LBSP). 

In order to find a BSP, we can first compute the maximum spanning tree $T$ of $G$, as show in Lemma 4.1 of [@vertexbottle].

{Theorem}

    If $T$ is a maximum spanning tree of $G$(under the weight $w$), then the unique $st$-path in $T$ is a $st$-BSP in $G$.

It's interesting this theorem actually extends to LBSP.

{Theorem}

    If $T$ is a maximum spanning tree of $G$(under the weight $w$), then the unique $st$-path in $T$ is the $st$-LBSP in $G$.

Before proving the theorem, we consider a useful lemma.

{Lemma}
    
    $P$ is a $st$-BSP with bottleneck edge $xy$. If removing edge $xy$ result a $sx$-LBSP and $yt$-LBSP, then $P$ is a $st$-LBSP.

{Proof}
    
    $P$ is a bottleneck $st$-path implies the $st$-LBSP has to reach either $x$ or $y$ before $t$. 

    If it reaches $y$ before $x$, then the subpath from $s$ to $y$ then from $y$ to $t$ using the $yt$-LBSP would imply $xy$ is not in $st$-LBSP, a contradiction.

    Thus, we must have the $st$-LBSP is a concatenation of $3$ paths, a $sx$-path $P_{sx}$, edge $xy$ and a $yt$-path $P_{ty}$. Using [Theorem 3], we notice $P$ is a LBSP.

{Proof}([Theorem 7])
    
    We prove by induction on the distance between the two vertices on the maximum spanning tree $T$.

    **Base Case:** If the length of a $uv$ on $T$ is $1$, then the edge $uv$ is a BSP, and also a LBSP.
    
    **Inductive Step:** Consider two vertices $s$ and $t$. The tree induces a $st$-BSP with bottleneck edge $xy$. By the inductive hypothesis, removing $xy$ result a $sx$-LBSP and $yt$-LBSP in $G$. The previous lemma demonstrates that $st$-BSP in $T$ is a $st$-LBSP in $G$.

