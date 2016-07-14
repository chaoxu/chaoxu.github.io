---
title: Formal Definition of Sequence Alignment
---

Consider an alphabet $\Sigma$ and two sequences $s$ and $t$ on $\Sigma$. Let $\bar{\Sigma} = \Sigma\cup \{\diamond\}$ where $\diamond$ is some symbol not in $\Sigma$, it's called the gap symbol. $M:\bar{\Sigma}\times \bar{\Sigma}\to \Z$. We have a gap penalties functions $g_s,g_t:\N\to \Z$. The functions are monotonic and are $0$ at $0$. The four boundary gap penalty coefficient $b_s,b_t,e_s,e_t\in \{0,1\}$. We want to find the alignment score between the two sequences.

A gap is the maximal substring consist of only $\diamond$'s. The gap sequence of a string is a sequence of lengths of each gap. Define $s_\diamond$ be the gap sequence of string $s$.

$G(x,g,y,a) = xg(a_1) + \sum_{i=2}^{n-1} g(a_i) + yg(a_n)$, where $a$ is a sequence of length $n$.

\[
A(u,v) = \sum_{i=1}^{|u|} M(u_i,v_i) + G(b_s,g_s,e_s,u_\diamond) + G(b_t,g_t,e_t,v_\diamond)
\]

Define $S$ and $T$ be the set of all strings that can be formed by inserting $\diamond$ into $s$ and $t$ respectively.

The alignment score is defined as 
\[
\max \{A(u,v) : |u|=|v|, u\in S, v\in T\}
\]

Now, once one write an algorithm for this problem, it can be used for many sequence alignment problems on [Rosalind](http://rosalind.info/).

- [Hamming Distance](http://rosalind.info/problems/hamm/): $M(a,a)=-1$, and $0$ otherwise. All other function are $-\infty$.
- [Finding a Shared Spliced Motif](http://rosalind.info/problems/lcsq/), longest common subsequence. $M(a,a)=1$ for $a\in \Sigma$, $0$ otherwise. Everything else are $0$.
- [Edit distance](http://rosalind.info/problems/edit/): Levenshtein distance, $M(a,a)=-1$ for $a\in \bar{\Sigma}$, $0$ otherwise. All other are $-\infty$.
- [Global alignment](http://rosalind.info/problems/glob/): $b_s=e_s=g_s$, $b_t=e_t=g_t$.
- [Overlap alignment](http://rosalind.info/problems/oap/): $b_s=e_t=0$, $b_t=g_t$, $e_s=g_s$.
- [Fitting alignment](http://rosalind.info/problems/sims/): $b_s=e_s=0$, $b_t=e_t=g_t$.
- [Semiglobal alignment](http://rosalind.info/problems/smgb/): $b_s=e_s=b_t=e_t=0$.
- Substring Matching: $b_s=e_s=0$, $g_s,g_t,b_t,e_t=-\infty$.
