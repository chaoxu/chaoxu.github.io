---
title: Pack a histogram
tags: classical, algorithm
---
 
Consider we have a histogram of $n$ values and $n$ distinct colors. The $i$th bar is a rectangle of size $1\times v_i$, where $v_i$ is the value and has color $i$. The total area of all the bars are $n$. We are interested in make a few cuts to pack it into $n$ squares each one of area $1$, such that each square is consist of at most $2$ different colors.

Formally, we are seeking an algorithm for the following problem:

{Problem}

	Input: $(v_1,1),\ldots,(v_n,n)$.

	Output: $S=\{s_1,\ldots,s_n\}$, such that $s_i = \{(x_{2i-1},c_{2i-1}),(x_{2i},c_{2i})\}$, $x_{2i}+x_{2i-1}=1$ and for all $1 \leq i\leq n$, 
	\[
	\sum_{s\in S} \sum_{(x,i)\in s} x = v_i.
	\]

Here is an algorithm that make sure this can be done in linear time. The idea is to separate the numbers into two bins. $(v_i,i)$ is in the large bin if $v_i > 1$, and it is in the small bin otherwise. 
The idea is always pick two elements, one in large bin one in small bin, say $(x,i)$ and $(y,j)$, where $x\leq 1< y$. Now we create a set $\{(x,i),(1-x,j)\}$, and put $(y-(1-x),j)$ back into the bins. 

If we can always prove that there is an element in the small bin during the execution of the algorithm, then we find an linear time algorithm for this problem. This is clear by simple induction.

This problem come up in order to implement the alias method. I recommend a good read on this topic [Darts, Dice, and Coins: Sampling from a Discrete Distribution](http://www.keithschwarz.com/darts-dice-coins/) by [Keith Schwarz](http://www.keithschwarz.com).