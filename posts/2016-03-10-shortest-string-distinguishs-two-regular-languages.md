---
title: Shortest string distinguishing two regular languages
tags: DFA
---

Let $D_0$ and $D_1$ be two DFAs with $n$ and $m$ states, such that $L(D_0)\neq L(D_1)$.

There exist a string of length at most $n+m$ in the symmetric difference of $L(D_0)$ and $L(D_1)$.

We construct the following DFA $D$ from $D_0$ and $D_1$. The start state $s$ has a transition $\delta(s,0) = s_0$ and $\delta(s,1) = s_1$, where $s_0$ and $s_1$ are start state of $D_0$ and $D_1$.

Now $D$ represents the language $\{0w|w \in L(D_0)\} \cup \{1w|w\in L(D_1)\}$. This language has at most $n+m+1$ equivalent classes in Myhill–Nerode theorem. There exist a string of length less than $n+m+1$ that differentiate state $s_0$ and $s_1$, since $s_0$ and $s_1$ can't correspond to the same equivalent class.