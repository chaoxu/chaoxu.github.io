---
title: Yotta Savings and covering designs
tags: banking
---

[Yotta Savings](https://www.withyotta.com/) is a FDIC insured savings account that give you "interest" in another way. It has a 0.2% APY. It is much better than the APY of many banks, but much worse than actual high APY savings accounts, e.g. Marcus, Ally. However, for each \$25, Yotta gives you a weekly entry to their drawing, which you would have some chance of winning big amount of money. This is very close to [Premium Bond](https://en.wikipedia.org/wiki/Premium_Bond) in the UK.

What if one want to actually figure out what is the actual APY one can hope for. People at [Hacker News](https://news.ycombinator.com/item?id=23780062) already did a lot of analysis, but in expectation you are looking at more than 3% APY.

What about worst case guaranteed return? So even if I'm the most unlucky person in the world, I can still get pretty nice returns.

Note we can have at most 10000 tickets. So maximum investment is \$250,000.

Recall $[n]=\set{1,\ldots,n}$.
Each ticket allows you to pick a size $6$ subset $X$ of $[70]$, and a single element $y$ from $[25]$. 
At end of the week, Yotta also would have a $6$ element subset $A$ of $[70]$, and a single element $b$ from $[25]$. Let a combination $(X,y)$ be called a ticket, and $T$ is the space of all tickets. A draw is also a ticket.

To actually find the optimum, you can write a huge optimization problem. Assume we have a valuation function $f:T\times T\to \R$, where $f(u,v)$ returns the value given ticket $u$ and the draw $v$. For $m$ ticket, the optimum guaranteed return is $\max_{F\in T^m} \min_{v\in T} \sum_{u\in F} f(u,v)$. This problem is too large to solve exactly. So let's look at if we can use some existing tools to get a quick approximation. 

There are many ways to win, but we only need to concentrate on the following:

Let $u=(X,y), v=(A,b)$.

 1. If $b=y$ and $|X\cap Y|=0$, then $f(u,v)=.1$.
 2. If $b=y$ and $|X\cap Y|=1$, then $f(u,v)=.2$.
 3. If $b=y$ and $|X\cap Y|=2$, then $f(u,v)=.8$.
 4. If $b=y$ and $|X\cap Y|=3$, then $f(u,v)=10$.
 5. If $b\neq y$ and $|X\cap Y|=3$, then $f(u,v)=.4$.

It is easy to see if you have 25 tickets, you can win at least \$0.1 by picking each possible $y$.

How about $X$? Let $a_0=.1, a_1=.2, a_2=.8$. If we can find a small family of $6$ element subsets, such that every set of size $i$ are covered, then we can earn at least $a_i$.

A $(v,k,t)$-covering design are family of $k$-element subsets of $[v]$, such that every $t$-element subset of $[v]$ is covered. The $k$-element subsets are called *blocks*. So we are looking for a $(70,6,k)$ design of small size. 
[Dan Gordon](https://www.dmgordon.org) has curated a good [collection of covering designs](https://www.dmgordon.org/cover/). 
The smallest known covering designs for each $k$ are $12, 172, 3258$.

At $12\times 25=300$ tickets, we can guarantee a win of $.2+11\times .1=1.3$ by create 25 copies of the $(70,6,1)$ covering design. 
Similarly, at $172\times 25=4300$ tickets, can guarantee at least a win of $.8+171\times .1=17.9$ by create 25 copies of the $(70,6,2)$ covering design. However, some of them might also win $.2$ instead of $.1$. So if our design also have the nice property that each element is covered roughly the amount of times, say 14, then we are looking at $.8+13\times .2 + 157\times .1=19.1$. This might not be true, but likely because designs are usually very symmetric. 

If we can find a $(70,6,3)$ covering design of $m$ elements that can be partitioned into 25 $(70,6,2)$ designs each with the above nice property, then we are looking at something like $.8+13\times 0.2 + (m/25-1)\times .1+0.4$. I don't know if *such design exists* though. Let's be hopeful assume it exists for $m=4300$. Then we are looking at \$20.9 earning per week for \$107,500 investment. Basically just a bit over 1\% APY. Together with the APY you earn a guaranteed 1.2\% APY for having a lot of money.

Input the entire design into the Yotta system every week is *a crazy amount* of work, and clearly not worth the time. It is better to just let the algorithm give you random numbers. Or maybe ask Yotta to have a excel import, but they don't allow ANY automation. 
