---
title: A common **3SUM-hard** reduction
---

I'm writing this to address a problem I often hear from friends. They usually get it from some technical interview.

{Problem}

    Let $A$ be an array of $n$ numbers, decide if there exist index $i,j,k$, such that $A[i]+A[j]=A[k]$.

The problem is **3SUM-hard**. We reduce the problem **3SUM** to the above problem in linear time.

{Problem}(**3SUM**)

    Does there exist $a,b,c\in S$, such that $a+b+c=0$?

Consider a instance of **3SUM**. Let $m=3 \max(S \cup -S)+1$. Store $\{m\}+S$ and $\{2m\}-S$ as elements in the array $A$. 

Claim: There exist i,j,k such that $A[i]+A[j]=A[k]$ iff there exist $a,b,c\in S$ such that $a+b+c=0$.

If there exist $i,j,k$ such that $A[i]+A[j]=A[k]$, this implies $A[i]=a+m,A[j]=b+m,A[k]=2m+(-c)$ for some $a,b,c\in S$. The other direction is similar.