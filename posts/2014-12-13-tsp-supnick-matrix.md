---
title: TSP, Max TSP and Supnick
---

{Problem}
    Given $f$ and $x_1,\ldots,x_n$. Find a permutation $\pi$ that maximizes (minimizes)
    \[
    \sum_{i=1}^n f(x_{\pi(i)},x_{\pi(i+1)}).
    \]

All our index calculations are mod $n$.

Assume $f$ can be evaluated in $O(1)$ time. The TSP problem reduces to this one.

{Definition}(Supnick)
    For all $x\leq x'$, $y\leq y'$, $f:\R^2\to \R$ is called Supnick if it has the following properties:

    1. Monge: $f(x,y)+f(x',y')\leq f(x',y)+f(x,y')$.
    2. Symmetric: $f(x,y)=f(y,x)$.

The name Supnick comes from [Supnick matrix](http://en.wikipedia.org/wiki/Supnick_matrix), which are symmetric Monge matrices. The following theorem implies an $O(n\log n)$ algorithm to solve the TSP problem if the distance matrix is Supnick [@supnick].

{Theorem}(Supnick’s)
    Let $x_1\leq x_2 \leq \ldots \leq x_n$, $f$ is Supnick, then
    \[
        \sum_{i=1}^n f(x_{\pi(i)},x_{\pi(i+1)})
    \]

    1. is minimized when $\pi = (1~3~5~7~\ldots~8~6~4~2)$.

    2. is maximized when $\pi = (n ~ 2 ~ (n-2) ~ 4 ~ (n-4) ~\ldots~5~ (n-3) ~3~ (n-1)~1)$.
 
Supnick matrices appears at many places. For example, [maximize the area of a radar chart](http://www.chaoxuprime.com/posts/2012-08-08-maximize-the-area-of-a-radar-chart.html) and [find a permutation maximize sum of adjacent distance](http://cstheory.stackexchange.com/questions/27808/finding-a-permutation-x-p-1x-p-2-x-p-n-of-x-1-x-2-x-n-whi).

It is a special case of a even larger class of problem that has the above permutation as solution: Quadratic assignment problem where the matrices are monotone antimonge and benevolent symmetric toeplitz matrix [@burkardQA]. 
 
