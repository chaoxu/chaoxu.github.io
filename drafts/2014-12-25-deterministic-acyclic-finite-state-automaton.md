---
title: Deterministic acyclic finite state automaton
---


# Deterministic acyclic finite state automaton
Let $L$ be a finite set of strings, and $D(L)$ to be the smallest finite state automaton recognize $L$. It's easy to see that $D(L)$ has a special state such that no transitions goes outside this state, and removing this state, the graph become acyclic.

Let's define a partial finite state automaton to be a automaton where all transitions that can only lead to failure state removed. Then the partial finite state automaton of $D(L)$ is a directed acyclic graph.

{Theorem}
    There exist a single sink state where there is no transitions to any other state. Call this state to be the failure state.

If we remove the failure state, then the remaining transition graph is a directed acyclic graph, and we can do many operations on this graph. 

This graph has a linear number of edges, and can be constructed in $O(n \log\sigma)$ time, where $\sigma$ is the size of the alphabet, thus in the worst case, $O(n\log n)$ time. We will assume the alphabet is finite.

Many problems become a DP problem on this directed acyclic graph. The idea all comes from one single observation: every unique substring of the strings in the set $L$ correspond to a path from the starting state. 

{Theorem}
    $D(L)$ can be built in $O(n)$ time, where $n$ is the sum of the length of all strings in $L$. The number of states and transitions in $D(L)$ are both $O(n)$.

# Suffix Automaton

Assume we are interested in answer questions about a single string, then likely a suffix automaton works well. Let $S(x)= D(Suffix(x))$ is the suffix automaton of $x$.

{Theorem}
    $S(x)$ can be built in $O(n\log \sigma)$ time, where $n=|x|$ and $\sigma$ is the size of the alphabet. The number of states and transitions are both $O(n)$.

## Suffix Link

One can extend the power of suffix automaton with suffix link. However I personally believe in that case, just use a suffix tree with suffix link instead, since we are not really taking the advantage of the directed acyclic graph structure.