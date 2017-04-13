---
title: An algebra of genealogy and Chinese kinship terminology
---

Let $G=\{f,d,s,p\}$ be a 4 element set. The interpretation should be $f$ is for father, $d$ is for daughter, $s$ is for son and $p$ is for partner. We assume each person has exactly one partner.

The genealogy monoid $M =\langle G \mid R \rangle$ with relators $R$ contains exactly

1. $p^2=1$,
2. $sf=df$ (normalize when gender is irrelevant), 
3. $ddf=dp$,
4. $pd=d$ and $ps=s$,
5. $dfd=d$ and $dfs=s$,
6. $sdf=s$ and $fdf=f$.

The $9$ relators give us a confluent string rewriting system(always go from left to right)! The proof is to run Knuth-Bendix completion algorithm on it. It is also easy to see one would find the shortest word express a entity in linear time by just running the rewriting algorithm.

This is great, because this is evidence that we humans can reason with genealogy effectively! 

{Remark}
    Actually, there exist a $13$ relator version that also make sure the rewriting system is length reducing, namely the length of the string gets smaller.

Also, it is quite important to see the set of normal forms is a regular language. 

Unfortunately(fortunately?), people don't really refer to someone else using words for this genealogy monoid. For example, who would use "father's partner" in place of "mother"? Luckily, Read has captured the American kinship terminology using algebra [@read1984], thus avoid this simplistic view.

It gets more confusing when one start to consider Chinese kinship terminologies. First of all, the genealogy monoid doesn't capture the relative age issue. Let's ignore that by using words like "兄弟" (brother) in place of "兄" (older brother) or "弟" (younger brother)[^1]. Chinese kinship has different names for entities in the paternal and maternal line that would otherwise have the same name in American terminology.

[^1]: Actually, relative age issues are very local, and can be handled easily and left as an exercise to the reader. 

If a person is interested in finding the shortest way to describe a relative using Chinese terminologies, it can be reduced to the following problem.

Property:

$M$ is word hyperbolic.

{Theorem}
    Every monoid with a presentation that induces a finite confluent rewriting system is word hyperbolic.

{Problem}
    
    All elements are given in normal forms.

    Input: $S=\{s_1,\ldots,s_k\}\subseteq M$. A positive weight function $w:S\to \N^+$. An element $x\in M$.

    Output: Sequence $a_1,\ldots,a_m$, such that $x = \prod_{i=1}^m s_{a_i}$ and $\sum_{i=1}^m w(s_{a_i})$ is minimized.

It is not even obvious if the product even exists. However, if finding one single product is decidable, then the problem is in NP. 

It's still interesting to consider a simpler problem, where $S$ and $w$ are fixed and the input is only $x$.

Claims

1. The Cayley graph is strongly connected if we delete ego.
2. Given the above claim, $xy=z$ always have a solution if $z\neq 1$.
3. All simple cycles has length $\leq 3$.
4. Checking if an element expressible by $S$ is called the submonoid membership problem.

What is known: If this monoid somehow have some hyperbolic property, then this problem can be solved in polynomial time. 

# Reference