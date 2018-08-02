---
title: Garside Normal Form and Summit Sets
tags: braid group, BSU REU
---

Note: This is just some notes I organized after reading the survey on [computational problems in the braid group by Jonathan Boiser](http://campillos.ucmerced.edu/~jboiser/boiserMSThesis.pdf). 
 
# The Garside Normal Form

{Definition}(The Positive Braid Monoid)
    
    The positive braid monoid of $n$ strands is denoted as $B_n^+$. Such that $B_n^+\in B_n$ and $B_n^+$ contain only word in the form $\sigma_{a_1}\ldots\sigma_{a_l}$.

{Definition}(The order relation)
    
    - $a\leq b$ if $ac = b$, where $a,b,c \in B_n^+$. $a$ is called the left divisor of $b$.
    - $\gcd(a,b) = c$, if $cw = a$ and $cv = b$, such that $|c|$ is maximized, where $a,b,c,w,v\in B_n^+$. $c$ is called the greatest common divisor of $a$ and $b$.

{Definition}(The Fundamental Braid)
    
    The fundamental braid, or half-twist, on $n$ strands 
    \[
    \Delta_n = (\sigma_1\ldots \sigma_{n-1})(\sigma_1\ldots\sigma_{n-2})\ldots(\sigma_1\sigma_2)\sigma_1
    \].

    $\Delta$ is used for $\Delta_n$ if $n$ is obvious from the context.


$\sigma_i\Delta = \Delta\sigma_{n-i}$ and $\sigma_i^{-1} = x_i\Delta^{-1}$ for some positive word $x_i$ are two important theorems the reader should check. Define a function $R$, such that $w\Delta = \Delta R(w)$ is true for all $w\in B_n$.

{Definition}(The Garside Normal Form)
    The Garside normal form of a word $w\in B_n$ is $w = \Delta_n^m p$. $p\in B^+_n$, and $\Delta$ is not a factor of $p$. We define $\inf(w)=m$.

If $w\in B^+_n$, find the largest $k$, such that $\Delta^k \leq w$, then $w = \Delta^k p'$, and it's a unique form.

If $w\in B_n$, first rewrite every $\sigma_i^{-k}$ in $w$ by $(x_i)^k\Delta^{-k}$, move all the $\Delta$ to the front, and we will have $w = \Delta^{-n} p$, where $p$ is a positive word. Then $p$ must have a normal form, let it be $\Delta^{m}q$. $w = \Delta^{m-n}q$, and it is uniquely determined.

Note the length of the positive word $p$ in the garside normal form is a constant, because all equivalent elements in $B^+_n$ have the same length.

{Definition}(Simple elements)
    A positive braid $p$ is simple if $p\in\mathcal{D} = \{x | x \leq \Delta\}$.

Note $\mathcal{D}$ generates the braid group.

{Definition}
    $w$ has the Garside normal form $\Delta^m p$, then we define the following functions.
    - The infimum, or the delta exponent, of $w$, $\inf(w) = m$
    - The index of $w$, $\lambda(w) = |p| + m|\Delta|$.

The index of $w$ is also $h-g$, where $h,g$ are the number of positive and negative generators respectively. No matter how $w$ is presented, the index is a constant. This is obvious because all braid relations doesn't change the index.

# The Summit Set
Let $[w]$ be the conjugacy class of $w$, in other words, $w'\in[w]$ if and only if $w = c^{-1}w'c$ for some $c\in B_n$. One write $w\sim w'$ if $w'\in[w]$

How can one check if $w\sim w'$? This is the conjugacy problem. Note $[w]$ is not finite unless $w=1$. Search through the entire set is impossible. The solution is similar to the word problem, it's also about finding a ``normal form'' for $[w]$. There are special subsets of $[w]$, such that there exist an algorithm with input $w'$. The algorithm output the subset if and only if $w'\in [w]$.

One of the first set with this property is the summit set.

Define $\inf[w] = \sup \{\inf(x) | x \in [w]\}$.

{Definition}(The Summit Set)
    The summit set of $w$ is written as $SS(w)$. 
    \[
    SS(w) = \{x | \inf(x) = \inf[w], x\in [w] \}
    \]

The summit set exists if there is an upper bound on $m$. Manipulate the index formula, we have $m = \frac{\lambda(w) - |p|}{|\Delta|}$. $m \leq \frac{\lambda(w)}{|\Delta|}$, therefore $m$ is bounded above. 

The summit set of $w$ is finite, and the size is bounded by $n^{\lambda(w) - |\Delta|\inf[w]}$. $\inf[w] = \frac{\lambda(w) - |p|}{|\Delta|}$, or $|p| = \lambda(w) - |\Delta|\inf[w]$. There are only finite many positive words with length $|p|$. 

{Theorem}
    $w\sim w'$, $w,w'\in B_n$, then there exist a $c\in B_n^+$, such that $w=c^{-1}w'c$.

$w=(\Delta^m p)^{-1} w' (\Delta^m p)$, which means $w=q^{-1} w' q$, where 
$q = \Delta R(p)$ or $p$, both are positive.

{Theorem}
    $w\in B_n$ and $x\in B_n^+$. For $a \in SS(w)$, if $x^{-1}ax\in SS(w)$, then $c^{-1}ac \in SS(w)$, where $c=\gcd(x,\Delta)$.

The theorem implies that every word in the conjugacy class can be reached by repeat conjugation of simple elements.

{Theorem}
    For $w,w'\in B_n$, $w\sim w'$ if and only if $SS(w) = SS(w')$

# Complexity
## Complexity of the word problem
To solve the word problem with Garside normal form, one can always put it in the $\Delta^{-k}p$ form in linear time. Then factor out the $\Delta$ divisors in $p$, and test if the resulting word is 1. 

Factoring out the divisors can be done in quadratic time with respect to the word length by using a refined version of the Garside normal form, Thurston's left greedy normal form.

## Complexity of the conjugacy problem
Given the theorems, one can devise the following algorithm to solve the conjugacy problem:

1. Input $w$ and $w'$, present them in Garside normal form. 
2. Find $SS(w)$ and $SS(w')$. 
3. Test if $SS(w) = SS(w')$
 
The algorithm to find $SS(w)$

1. conjugate the input $w$ with every simple element.
2. Pick the set of produced elements, such that the $\Delta$ exponent is the largest.
3. For each one of those elements, go though 1 again.
4. If the process doesn't produce more elements with larger $\Delta$ exponent, the summit set has been found.

The main complexity depend on the size of $SS(w)$. It is known $SS(w)$ could be exponential in $l$ and $n$.
