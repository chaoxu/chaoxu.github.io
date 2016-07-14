---
title: Sushi sharing problem
---

The problem was first brought up by [Sam McCauley](http://www.cs.sunysb.edu/~smccauley/) when I was still in Stony Brook.

Two germophobic friends are sharing sushi. There are two kind of sushi, $0$ and $1$. The sushi are lied out on a $n\times m$ matrix. There are $a$ sushi of type $0$ and $b$ sushi of type $1$, both are even numbers and $a+b = nm$. One can pick up a sushi with a chopstick by picking it up horizontally, or vertically. It will touch the adjacent sushis horizontally or vertically, respectively.

Each person want to eat exactly $a/2$ sushi of type $0$ and $b/2$ sushi of type $1$, and no one want to eat a sushi touched by someone else. Is this always possible? Yes it is!

First, it's easy to see each person can pick up a entire row or column, and collapse the remaining matrix. All the remaining sushi in the matrix are clean. We will assume each person pick up a entire row/column. 

We prove this by induction. Let $f(i)$ be the number of $1$'s in the $i$th column, and $f(S)=\{f(i)|i\in S\}$. As long as both friends pick up the same number of each sushi before collapse the matrix, we can go to a smaller case. 

Before the case by case analysis, we note of two conditions:

Even sum condition

:   $\sum_{i\in [1..m]} f(i)=b$, it must be even.

Even side condition

:   Because $nm$ is even, either $n$ or $m$ is even.

- $n>m$, rotate the matrix.

- $n+1<m$, by pigeonhole principle, $f(i)=f(j)$ for some $i$ and $j$. One person pick $i$th column and the other pick the $j$th column.

- $n+1\geq m$ and $m\geq \frac{1 + \sqrt{1+16n}}{2}$. If there is no $2$ columns with the same number of $0$'s and $1$'s, then let $f([1..m])=S$ to be a set of $m$ elements. Now, consider we pick 4 distinct elements $a,b,c,d$ from $S$ and if $a+b=c+d$, then we can let one person pick columns $f^{-1}(\{a,b\})$ and the other pick columns $f^{-1}(\{c,d\})$. Note if $a+b=c+d$, and we know that $a,b$ and $c,d$ are distinct, and $\{a,b\}\neq \{c,d\}$, then $a,b,c,d$ must be all distinct. $a+b$ can take $2n-1$ different values, between $1$ and $2n-1$. There are ${m \choose 2}$ pairwise sums. This shows there must be at least $2$ pairs that both have the same difference by pigeonhole principle when ${m\choose 2}\geq 2n$, which is true when $m\geq \frac{1 + \sqrt{5+16n}}{2}$.  To translate this,
    - If $n+1=m$, then this is always true when $n\geq 3$.
    - If $n=m$, then this is always true when $n\geq 5$.

- $n\leq 2$ and $n+1=m$. Since both $0+1$ and $0+1+2$ is odd, we must have two columns have the same value.

- $n=m=2$. If there is no $2$ columns with the same number of $0$'s and $1$'s, then $f([1..2])=\{0,2\}$ in order to satisfy the even condition. But each row would have the same number of different kind of sushi. Let each friend take a row.

- $n=m=3$. This is not possible with even side condition.

- $n=m=4$. This is the last case. If no $2$ columns have same number of $0$'s and $1$'s, then $f([1..4])=\{a_1,a_2,a_3,a_4\}$ where $a_1<a_2<a_3<a_4$ and has to equal to one of the following due to the even sum condition. In all cases, $a_1+a_4=a_2+a_3$.

    - $0,1,2,3$
    - $0,1,3,4$
    - $1,2,3,4$

