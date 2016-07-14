---
title: Maximize the area of a radar chart
tags: math
---

People use [radar charts](http://en.wikipedia.org/wiki/Radar_chart) to present their ability in different skills. 

Like all self presentations, it need to make the person "look good". A chart with larger area looks better than the ones with less area. The remaining of the article take about how to find a radar chart with maximum area.

![Two Radar Charts](/files/radarchart.png)

In the image above, the left side has less area than the right. 

Given the sequence of data $(a_1,c_1),\ldots,(a_n,c_n)$, where $a_i$ is the value, a positive number, $c_i$ is a coordinate(in the image, the coordinates would be "combinatorics", "algorithms", etc.). How can we permute them, such that the data plotted on the radio chart is maximized? 

The area of of any single section of the radio chart is $ab \frac{\cos \theta}{2}$, where $\theta = \frac{2\pi}{n}$, and $a,b$ are the value for two adjacent coordinates. The area of the entire shape is the sum.

Once we can maximize $\sum_{i=1}^n a_{\pi(i)} a_{\pi(i+1)}$, we are done.

{Lemma}
    If $x\geq a\geq b \geq c \geq d\geq 0$, then $ax+bx - ab \geq cx+dx-cd$.

{Proof}
    Let's consider what is required to make sure the property works.
    \begin{align*}
    (a+b)x-ab &\geq  (c+d)x-cd\\
    x&\geq \frac{ab-cd}{a-c+b-d}\\
    x&\geq \frac{ab-bc+bc-cd}{a-c+b-d}\\
    x&\geq \frac{b(a-c)+c(b-d)}{a-c+b-d}\\
    x&\geq \frac{b(a-c+b-d)}{a-c+b-d}\\
    x&\geq b
    \end{align*}
    The lemma is true, as $x\geq b$ is part of the hypothesis for $x$.

{Theorem}
    Let $a_1,\ldots,a_n$ be a sequence of positive numbers, such that $a_1 \leq \ldots \leq a_n$. 
    
    Let $\pi$ be the following:

    1. $\pi(n+1) = 1$.
    2. $\pi(1),\ldots,\pi(n) = 1, 3, \ldots, n-1, n, n-2, \ldots, 4, 2$ if $n$ is even
    3. $\pi(1),\ldots,\pi(n) = 1, 3, \ldots, n-2, n, n-1, \ldots, 4, 2$ if $n$ is odd.
    
    $\pi$ maximizes
    \[
    \sum_{i=1}^n a_{\pi(i)}a_{\pi(i+1)}
    \]

{Proof}
    Proof by induction.
    
    *Base case:* Base case for $n=1,2$ are trivial.
    
    *Inductive Step:* Consider it is true for $n-1$, we want to show it is true for $n$.
    
    Consider we already obtained a sequence from $a_1$ to $a_{n-1}$, and we want to insert $a_n$ somewhere. 
    Clearly $a_n$ need to be placed in a position that maximizes its contribution. If we can insert it between $a_{n-1}$ and $a_{n-2}$, then
    we get a contribution of $a_{n-1}a_n+a_{n-2}a_n-a_{n-1}a_{n-2}$. By the lemma, it will give us the maximum contribution.
    
    If we can get the maximum contribution (over all possible configurations) to a maximum configuration for $a_1$ to $a_{n-1}$, then we are done. The inductive hypothesis show the maximum configuration have adjacent $a_{n-1}$ and $a_{n-2}$, therefore this shows the maximum configuration is where we insert $a_n$ in between $a_{n-1}$ and $a_{n-2}$ in the previous maximum configuration.

This implies a simple $O(n \log n)$ algorithm. Find the permutation that sort the input sequence by value, then composes it with permutation $\pi$.

Actually this problem is a special case of the TSP problem on a product matrix. A matrix is called a product matrix if $M_{i,j} = a_ib_j$ for vectors $(a_1,\ldots,a_n)$ and $(b_1,\ldots,b_n)$. It is solvable in polynomial time if $M$ is a symmetric matrix [@burkard1998].



