---
title: Impossibility theorem of marriage tax
tags: Tax
---

It is well known that there could be [marriage penalty or marriage bonus](https://en.wikipedia.org/wiki/Marriage_penalty). For example, in 2019, if two people each make more than $306,175, then they have to pay more tax after getting married. In the worst case, they have to pay $8,165 more. Not that bad. However, if one person make all the money, and the other has no income, then together they will always pay a smaller amount of tax. 

I always thought this is because the tax code is designed to make sure in a family, there is a sole breadwinner. But recently I realized it is just mathematically impossible to have anything other than a linear tax and preserve some other nice properties.

Indeed, this was shown by Lovell [@Lovell82].

Let $\R_+$ be the positive reals.
Consider functions $S:\R_+ \to \R_+$ and $J:\R_+ \to\R_+$. The first is for tax paid for a single person and tax paid for a married couple file jointly. The input for married file jointly is a single number, which is the combination of the taxable income of the couple. This is called horizontal equity in marriage.

Marriage neutral is precisely when $S(x)+S(y) = J(x+y)$.
We define a few notions, it is not completely the same as the ones in Lovell's paper [@Lovell82], but it essentially demonstrate the same idea. 

A tax function $T$ should have the following properties.

1. Reasonable Tax: $0\leq T(x)\leq x$.
2. Principal of Progressiveness: there is some $c>0$ such that $\frac{T(x)}{x} > \frac{T(y)}{y}$ for all $x>y>c$.

The second one tries to tax the rich more, as in larger proportion of their money.
The reasonable tax requirement makes sure $S(x) = ax$ for $a\in[0,1]$. It is easy to see we cannot hope to have principal of progressiveness.

::: Remark
Married filing separately is always no better than them being single and file their own taxes. 
::: 

I personally think there should not be a marriage penalty at any income level to encourage marriage. Of course, people might disagree and think the rich should have a marriage penalty, since it is a small amount compare to their total income so they won't care anyways. 

Anyway, consider the world where there can only be marriage bonus. That is we have the property $S(x)+S(y)\geq J(x+y)$.
An *easy tax function* is a function that has reasonable tax property, and is a piecewise-linear convex that has at least $1$ breakpoint larger than $0$. This is strictly stronger than principal of progressiveness. This is satisfied by the current personal income tax function used by the IRS.

Let $S$ be a easy tax function, then we can obtain an easy tax function $J$ that always gives a marriage bonus. Indeed, let $J = \inf_{a+b=x,a,b\geq 0} S(a)+S(b)$. $J$ is extreme in a way that any function greater than it at any point will cause marriage penalty. $J$ is the infimal convolution of $S$ and itself, which would also be piecewise-linear convex. 
If $S$ is the personal income tax function for 2019, then $J$ matches the 2019 IRS married file jointly function up to \$612,350! It's just for some reason the IRS decide to cut this $J$ off at \$612,350, and then impose a higher rate just to penalize families with two very high income earners. 