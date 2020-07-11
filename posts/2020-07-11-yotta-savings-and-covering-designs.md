---
title: Yotta Savings and covering designs
tags: banking, probability
---

[Yotta Savings](https://www.withyotta.com/) is a FDIC insured savings account that gives you "interest" in another way. It has a 0.2% APR. It is much better than the APR of many banks, but much worse than actual high APR savings accounts, e.g. Marcus, Ally. However, for each \$25, Yotta gives you a ticket weekly to their drawing, which you would have some chance of winning a big amount of money. This is very close to [Premium Bond](https://en.wikipedia.org/wiki/Premium_Bond) in the UK.
What if one wants to actually figure out the actual APR one can hope for. People at [Hacker News](https://news.ycombinator.com/item?id=23780062) already did a lot of analysis, and in expectation you are looking at more than 3% APR. If you are going to sign up, please use my referral code `CHAO1` and we both get 100 tickets. 

What about worst case guaranteed return? For example, if you believe Yotta is out to get you and draw numbers to minimize what you are going to earn (paranoid? yep). The entire expectation calculation means nothing to you. What is the guaranteed return if you save a large sum of money in there. Note each person can have at most 10000 tickets, so a maximum investment of \$250,000.

Recall $[n]=\set{1,\ldots,n}$. Each ticket allows you to pick a size $6$ subset $X$ of $[70]$, and a single element $y$ from $[25]$. At the end of the week, Yotta also would have drawn a $6$ element subset $A$ of $[70]$, and a single element $b$ from $[25]$. Let a combination $(X,y)$ be called a ticket, and $T$ is the space of all tickets. A draw is also a ticket.

To actually find the optimum, you can write a huge optimization problem. Assume we have a valuation function $f:T\times T\to \R$, where $f(u,v)$ is the winning for given ticket $u$ and draw $v$. For $n$ ticket, the optimum guaranteed return is $\max_{F\in T^n} \min_{v\in T} \sum_{u\in F} f(u,v)$. This problem is too large to solve exactly. Let's try some existing tools to get a quick lower bound.

There are many ways to win, but we only need to concentrate on the following, the rest are not helpful with meeting the bottom line. Let $u=(X,y), v=(A,b)$.

- If $b=y$ and $|X\cap Y|=0$, then $f(u,v)=0.1$.
- If $b=y$ and $|X\cap Y|=1$, then $f(u,v)=0.2$.
- If $b=y$ and $|X\cap Y|=2$, then $f(u,v)=0.8$.
- If $b=y$ and $|X\cap Y|=3$, then $f(u,v)=10$.
- If $b\neq y$ and $|X\cap Y|=3$, then $f(u,v)=0.4$.

It is easy to see if you have 25 tickets, you can win at least \$0.1 by picking each possible $y$.
How about $X$? Let $a_0=.1, a_1=.2, a_2=.8$. If we can find a small family of $6$ element subsets, such that every set of size $i$ are covered, then we can earn at least $a_i$.

::: Definition

  A $(v,k,t)$*-covering design* is family of $k$-element subsets of $[v]$, such that every $t$-element subset of $[v]$ is covered. The $k$-element subsets are called *blocks*.

:::

So we are looking for a $(70,6,k)$ design of small size. [Dan Gordon](https://www.dmgordon.org) has curated a good [collection of covering designs](https://www.dmgordon.org/cover/). The smallest known covering designs for each $k$ are $12, 172, 3258$. The last one is too large to be useful, but gives an indication on the size we are looking at. In fact, we can ask for stronger property. 

::: Definition

  A $n$-block $(v,k,t)$-design is called *nice*, if for each $t'\leq t$ subset, it is covered by at least $\left\lfloor n{v\choose k}/{v\choose t'} \right\rfloor$ blocks.

:::

[The $(70,6,2)$-covering design by Jan de Heer and Steve Muir](https://ljcr.dmgordon.org/show_cover.php?v=70&k=6&t=2) is a nice design.

At $12\times 25=300$ tickets, we can guarantee a win of $.2+11\times .1=1.3$ by creating 25 copies of the $(70,6,1)$-covering design. Similarly, at $172\times 25=4300$ tickets, one can guarantee at least a win of $.8$ by creating 25 copies of the $(70,6,2)$-covering design. Due to it is also a nice design, each element appears at least 14 times. So we are looking at $.8+13\times .2 + 158\times .1=19.2$. So \$19.2 earnings per week for \$107,500 investment, which is a 0.93\% APR. 

This is only a lower bound to the optimization problem, so maybe even more APR can be obtained through better design. Together with the 0.2\% interest you earn a guaranteed 1.13\% APR for having a lot of money. 

We can do more if certain design exists. If we can find a $(70,6,3)$-covering design of $25 \times n$ blocks that can be partitioned into 25 nice $(70,6,2)$ designs, then we can do more. In theory, a nice $(70,6,2)$-covering design has a lower bound of 164 blocks. So taking $n=164$, we are looking at:

 1. 1 ticket of .8 winning,
 2. 1 ticket of .4 winning,
 3. 13 tickets of .2 winning,
 4. 150 tickets of .1 winning.

A total of \$18.8 per week over investment of \$102,500. A 0.96\% APR.
My guess is there is a potential of finding a spot of 1\% APR by solving the optimization problem exactly. 

Input the entire design into the Yotta system every week is *a crazy amount* of work, and clearly not worth the time. It is better to be not paranoid and let the algorithm give you random numbers. Or hope one day Yotta allows one to import a list of numbers.
