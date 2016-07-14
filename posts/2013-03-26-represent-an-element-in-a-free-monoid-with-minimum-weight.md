---
title: Represent an element in a free monoid with minimum weight
tags: Haskell, monoid
---

Consider a rank $k$ free monoid $(M,\cdot)$ with free generators $G$. Sometimes there are ways to express them by writing a little less than write the whole string of generators. We can group some generators by powers.
For example, $aababababaaaa = a(ab)^4a^4$.

{Problem}

    Find the shortest way to write down an element in a free monoid.

There are problems on how long are the parentheses, exponents etc. Therefore we generalize it to allow weight to those operations. 

Formally. For any free monoid $M$ with free generators $G$, we can construct another free monoid $(M^*,\cdot)$,


1. $a\in G \implies Atom(a)\in M^*$.
2. $a\in M^*$, $n\in\N$, then $Power(a,n) \in M^*$. 

{Definition}

    Consider a homomorphism $w:M^*\to \N$. Such that for all $n$, it satisfy the following criteria: 
    
    1. $w(a)\leq w(b) \implies w(Power(a,n))\leq w(Power(b,n))$,
    2. $w(a)\leq w(Power(a,1))$.
    
    $w$ is a weight function.

Let $f:M^*\to M$, such that

- $f(ab) = f(a)f(b)$,
- $f(Atom(a)) = a$,
- $f(Power(a,n)) = a^n$.

{Problem}

    Given $a\in M$, we want to find $a'\in M^*$, such that $f(a') = a$ and $w(a')$ is minimized.

The input is $a_1\ldots a_n$.

Let $D(i,j)$ represent the minimum weight representation for $a_i\ldots a_j$. Let $P(i,j)$ represent the set of all possible $Power(x,k)$, such that $f(Power(x,k)) = a_i\ldots a_j$ for some $k\neq 1$. 

\begin{align*}
D(i,i) &= a_i\\
D(i,j) &= \min(P(i,j)\cup \{ D(i,k)+D(k+1,j)| i\leq k\leq j-1\})
\end{align*}

Here $\min$ return any of the expressions that achieves the minimum weight. This allows a $O(n^3)$ algorithm if one uses suffix tree for finding $P(i,j)$. One can naively try all possible $Power(x,k)$ instead, where $k|n$.

Here is an Haskell code for it. It is designed to show the algorithm instead of been efficient. This has real life usage to [compress regular expressions](/posts/2013-03-21-regular-expression-for-a-interval-of-non-negative-integers.html).
 
<script src="https://gist.github.com/chaoxu/72a82300b9750f9c0374.js"></script>