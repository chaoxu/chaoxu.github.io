---
title: Traditional vs Roth IRA under fixed amount of investment
tags: Tax
---

I've read a lot of articles on Traditional vs Roth IRA, however one can do a serious mathematical analysis of which one is better, given that you want to invest a total of $x$ income into the account for a particular year. This means, money over than the contribution limit will be put in a normal investment account. 

There is a widely held belief that if your effective tax rate is higher today than when you cash out, it is always better to do pre-tax contribution (Traditional IRA). However, we can show this is not always the case. You need effective tax rate to be somewhat lower unless you are in the lowest of tax bracket. Assume for simplicity, you live in a place without state taxes. 

We make the assumption you have enough money to maximize the contribution, but you have even more money left to put in a normal account. 

Here are the 3 options.

- Type 1: (Traditional IRA, 401(k)) No tax when contributing, taxed as ordinary income afterwards.
- Type 2: (Roth IRA, Roth 401(k) Taxed when contributing, and no taxes afterwards.
- Type 3: Normal account, taxed when contributing, and tax with either ordinary income or capital gain taxes depending on type. 

Consider the simplest model. You have a stock that you buy and hold. It generates no dividends. At the time when you sell it, it worths $k$ times more. All your money will be going into that stock.

Assume you allocate $x$ income into $x_1,x_2,x_3$, which is the amount of income allocated into Type $1,2,3$ accounts, respectively.

How much after tax income is generated when you sell the stock?

Let $\alpha$ be the (approximate) effective tax rate today, $\alpha'$ is an (approximate) effective tax rate when you sell. $\beta'$ is the capital gain tax rate.

The after-tax income contributed by each account is
 - Type 1: $kx_1(1-\alpha')$
 - Type 2: $kx_2(1-\alpha)$
 - Type 3: $(k-1)x_3(1-\alpha)(1-\beta') + x_3(1-\alpha) = x_3(1-\alpha)((k-1)(1-\beta')+1)$

Now, we also have the following constraint. $x_1+x_2(1-\alpha)=B$, $x_1+x_2+x_3=x$, etc. Since $B$ and $x$ are fixed. 
We have $x_2 = \frac{B-x_1}{1-\alpha}$, and $x_3 = x-x_1-\frac{B-x_1}{1-\alpha}$

Define $f(x_1)=kx_1(1-\alpha') + k(B-x_1) + (x-x_1-\frac{B-x_1}{1-\alpha})(1-\alpha)((k-1)(1-\beta')+1)$, which is a linear function. We take the derivative of $f$ and obtain the slope if $k(\alpha-\alpha')- (k-1)\alpha\beta'$. If the slope is positive, it means when should maximize type $1$ account, and if the slope is negative, we should maximize type $2$ account.

In order for the inequality to work for all $k>1$, we need $\alpha-\alpha'\geq \alpha\beta'$.
In other words, we need $\alpha'\leq \alpha(1-\beta')$ in order to safely say it is better to maximize type 1.

We didn't even consider what happens if there are dividend involved. It would shift the scale even more toward type 2. 

I think the moral of the story is you have to be careful and actually model everything correctly. Also, since there is no way to know the tax rate in the future, some people hedge by putting money in both type 1 and type 2 accounts. 

Good thing is that it still suggest pre-tax is better than after-tax if your tax rate is much higher than your future tax rate. Which might not be true if you are very conscious about saving money (for example, save 80% of your income), even you are in the highest tax bracket. 