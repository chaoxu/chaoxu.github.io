---
title: Recognize Uniquely Decodable Codes
---

A set of strings $C$ is called a uniquely decodable code, if $C^*$ has a unique factorization over $C$. Namely, for each element $s$ in $C^*$, there exist a unique finite sequence $\{s_i\}_{i=1}^m$ of elements in $C$, such that $s_1\ldots s_m = s$. We call the strings in $C$ a code string.

{Problem}(Uniquely Decodable Code Recognition)
    
    Let $C$ be a finite set of strings, decide if $C$ is a uniquely decodable code.

In general, the infinite version of the problem is undecidable. It is however decidable if [$C$ is a regular language](http://cs.stackexchange.com/questions/6114/represent-string-as-concatenations).

To solve this problem, we shall describe a formulation of [Sardinas–Patterson algorithm](http://en.wikipedia.org/wiki/Sardinas%E2%80%93Patterson_algorithm), which combines the description of [@jewelsofstringology] and [@rodeh].

In the entire article, we assume the alphabet size is fixed. 

For variable sized alphabet, let $\sigma$ be the number of distinct alphabet appeared in $C$. There is an extra factor of $\sigma$ or $\log \sigma$ depending on if there exist a comparator for the alphabet.

Define $S(C) = \{ v| xv = c, c\in C\}$, the set of suffixes of $C$. $I(C) = \{ v| c'v = c, c'\neq c, c',c\in C\}$, we call those initial suffix.

{Lemma}
    
    $C$ is uniquely decodable if and only if there is no $v\in I(C)$, such that $vs\in C^*$ and $s\in C^*$. 

{Proof}
    
    If there exist such $v$ so $vs,s\in C^*$. $c'v=c$ for some $c,c'\in C$. Consider the string $cs=c'(vs)$. It has at least two factorization, one start with $c$, the other start with $c'$, and $c'\neq c$.

    Let no such $v$ exists. Consider some $u\in C^*$. It can be written as $cs$ and $c's'$ for $c,c'\in C$, $s,s'\in C^*$. Assume there is more than one factorization, then $c'\neq c$ and wlog $c'v=c$. We arrive $vs=s'\in C^*$, a contradiction.

Consider a $G=(V,A)$ a directed graph. $V=S(C)$. There is an arc from $a$ to $b$ iff $ab=c$ or $cb=a$ for some $c\in C$.

{Theorem}

     $C$ is uniquely decodable code iff there is no path from a vertex in $I(C)$ to $\epsilon$.

{Proof}
    
    For any arc $uv$, if $uv=c\in C$, then label the arc $uv$ by $u$. Otherwise, label the arc with $c$ where $cv = u$. It's then easy to see concatenate the labels on any walk from $v$ to $\epsilon$ spells a string of the form $vs \in C^*$ for $s \in C^*$.

    By induction, one can show that for a vertex $v$, there exist string $vs\in C^*$, where $s\in C^*$, if and only if there is a path from $v$ to $\epsilon$.

In order to compute the arcs on the graph, we need to answer two questions. 

1. Is $u$ a prefix of $c$? For all $u\in S(C)$ and $c\in C$.
2. Is $c$ a prefix of $u$? For all $u\in S(C)$ and $c\in C$.

Let $k=|C|$, $n=\sum_{c\in C}|c|$. It is known that we can build a generalized suffix tree for $C$ in linear time.

For the first question, "Is $u$ a prefix of $c$?", consider we constructed a suffix tree $T$ for $C$. Transversing the suffix tree $T(C)$ with a code string $c\in C$.
Assume we have read a prefix $u$ of $c$, we can check if $u\in S(C)$ in constant time during the transversal. 
If $u\in S(C)$, then $c=uv$ where $v\in S(C)$. It will add an arc $uv$. 
We use $O(|c|)$ time to find all the arcs can be formed by answering the "Is $u$ a prefix of $c$".
In total, we can answer question 1 in $O(n)$ time.

For the second question, we can also use the same suffix tree. Transverse the suffix tree with a code string $c$. Find all the leaves in the subtree when the string $c$ ends. Those correspond to all strings $u\in S(C)$ such that $c$ is a prefix. Namely $u=cv$ where $v\in S(C)$. The algorithm add an arc $uv$. Note there might be many $u$ with this property, worst case $O(n)$. This step might run in $O(nk)$ time.

It's not clear we can construct arcs between the vertices in constant time.
The main difficulty lies in two suffix of two different code word might correspond to the same vertex in the graph.
If we number the code words and name the suffixes by their length, then we can construct a $O(n)$ size table that maps each pair $(i,j)$, which represents the suffix of length $j$ in the $i$th string, to a vertex that represent the string. Simply build a trie for the reverse of the strings, and do some bookkeeping. This map allow us to add an edge in constant time.

The graph $G$ has at most $O(nk)$ arcs. We add a new vertex that has an arc to each initial vertices and apply a DFS from the new vertex.
If it reaches the $\epsilon$ vertex, we return true, else we return false.

{Theorem}
    There exist an algorithm to test if $C$ is a uniquely decodable code in $O(nk)$ time, where $k=|C|$ and $n=\sum_{c\in C} |c|$.

I have an implementation in Haskell [here](https://github.com/chaoxu/haskell-algorithm/blob/master/SardinasPatterson.hs). Note it doesn't run in exactly $O(nk)$ time because of the `Map` takes $O(\log n)$ time. It can, however, be easily modified to run in $O(nk)$ time.


