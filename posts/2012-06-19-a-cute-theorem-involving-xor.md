---
title: A cute theorem involving xor
tags: math
---

Define $[a..b] = \{x|a\leq x\leq b, x\in \mathbb{N} \}$, and $k\oplus [a..b] = \{k\oplus x| x\in [a..b]\}$, where $\oplus$ is the bitwise xor function.
We want to know something about $k\oplus [a..b]$. 

{Lemma}
    $x-y \leq x\oplus y \leq x+y$
    
{Proof}
    $x\oplus y \leq x+y$ is easy to see as $\oplus$ is binary addition without carries.
    Assume $x\oplus y < x-y$ for some $x$ and $y$, then
    \[
    x = x\oplus(y\oplus y)
    = (x\oplus y) \oplus y <
    (x-y)\oplus y\leq (x-y)+y = x
    \]
    A contradiction. Therefore $x-y \leq x\oplus y$.
    
{Remark}
    $\oplus$, binary addition without carries, and binary subtraction without borrows are the same operation. The lemma is trivial by notice that equivalence. 

{Theorem}
    If $f:\mathbb{N}\to \mathbb{N}$ a surjection such that $x-n\leq f(x)\leq x+m$, then

    1. $\{f(x)|x\in [a..b]\} \supseteq [a+m..b-n]$
    2. $\{f(x)|x\in [0..b]\} \supseteq [0..b-n]$

{Proof}
    $f$ is a surjection implies all values in any integer interval get's taken. $f^{-1}(y)=\min(\{x|f(x)=y\})$
    \[
    x-n\leq f(x)\leq x+m \implies y+m\leq f^{-1}(y)\leq y-n.
    \]

    1. If $y\in [a+m..b-n]$, $f^{-1}(y) \in [(a+m)-m.. (b-n)+n] = [a..b]$.
    2. If $y\in [0..b-n]$, $f^{-1}(y) \in [\max(0-m,0).. (b-n)+n] = [0..b]$.


Let $f_k(x) = k\oplus x$, noting that $f_k$ is a bijection, we derive the result we want for xor.

{Corollary}
    1. $[a+k..b-k] \subseteq k\oplus [a..b]$.
    2. $[0..b-k] \subseteq k\oplus [0..b]$.