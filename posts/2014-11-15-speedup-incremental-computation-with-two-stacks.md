---
title: Speed up incremental computation with two stacks
tags: algorithmic toolkit
---

# Introduction

{Problem}
	Consider a knapsack of capacity $C$ and a empty sequence of objects. One can update the sequence by add or delete objects from either end of the sequence. Construct a data structure, such that we can output the maximum possible value of the knapsack after every update if we pack the knapsack using the objects in the sequence.

This is a generalization of the [online knapsack problem](http://codeforces.com/blog/entry/14366), which is an generalization of a hard [offline knapsack problem](https://www.hackerrank.com/contests/cs-quora/challenges/quora-feed-optimizer) used by Quora. 

A naive algorithm just recompute the knapsack each time using a dynamic programming algorithm. 
This would take $O(mnC)$ time, where $m$ is the total number of updates, $n$ is the maximum number of objects in the sequence. 
The rest of the article describe the idea of decomposable function, which lead to an solution that solves this problem in $O(mC)$ time.

# Decomposable function

{Definition}
	A function $f:\cup_{i=1}^\infty X^i \to Y$ is called $(\triangleright,\bowtie,\triangleleft)$-decomposable, if for all $1 \leq k \leq n$,
	\[
	f(x_1,\ldots,x_n)=(x_1 \triangleright \ldots \triangleright x_k \triangleright id_{\triangleright}) \bowtie (id_{\triangleleft} \triangleleft x_{k+1}\triangleleft \ldots \triangleleft x_n)
	\]
	where 

	  1. $\triangleright:X\times Y_{\triangleright}\to Y_{\triangleright}$ is right associative.
	  2. $\triangleleft:Y_{\triangleleft} \times X\to Y_{\triangleleft}$ is left associative. 
	  3. $\bowtie:Y_{\triangleright}\times Y_{\triangleleft}\to Y$.

{Problem}
	Let $f$ be a $(\triangleright,\bowtie,\triangleleft)$-decomposable function. Let $S$ be a finite sequence of elements, such that $S$ is in the domain of $f$. Dynamically output $f(S)$ after every deque operation on $S$, such that we call $\triangleright,\bowtie$ and $\triangleleft$ amortized constant number of times per operation. 

We will use $\bigtriangleright_{i=1}^n x_i$ to mean $x_1 \triangleright \ldots \triangleright x_n \triangleright id_{\triangleright}$, and $\bigtriangleleft_{i=1}^n x_i$ to mean $id_{\triangleright} \triangleleft x_1 \triangleleft \ldots \triangleleft x_n$. 

$f$ is called decomposable if there exist $(\triangleright,\bowtie,\triangleleft)$ such that it's decomposable.

The intuition is the function $f$ can be decomposed into solving two pieces of problems. Each piece of the problem has a incremental nature. 

$D_C(x_1,\ldots,x_n)$, the maximum value possible given objects $x_1,\ldots,x_n$ and a knapsack of capacity $C$ is a decomposable function. If $x_1\triangleright \ldots \triangleright x_n$ produces the last row of the common dynamic programming algorithm for the knapsack, then we can let $\bowtie$ to be an operation that combine the last rows to output a value.

# Implement the data structure

Intuitively, $\triangleright$ and $\triangleleft$ produces two stacks. $\bowtie$ combines the information on the stack to produce the solution to $f$.

## $\triangleleft$, a stack

{Problem}
	We have a stack of elements $S$, dynamically maintain $foldl \triangleleft$ in $O(1)$ $\triangleleft$ per operation.

Say $S=x_1,\ldots,x_n$. We store $\bigtriangleleft_{i=1}^k x_i$ for all $1\leq k\leq n$, and whenever there is an insertion, we compute $\bigtriangleleft_{i=1}^{n+1} x_i = \bigtriangleleft_{i=1}^n x_i \triangleleft x_{n+1}$ in one monoid operation. Deletion can be handled by discarding a value. In the worst case, $\triangleleft$ gets called only once.

But this requires us to store $O(n)$ values after the fold. We can decrease the extra space to store only $O(\sqrt{n})$ prefix sums.

Let $S(k) = \bigtriangleleft_{i=1}^k x_i$. Originally we store $S(k)$ for all $1 \leq k\leq n$. We show how we store only when $k$ is a perfect square and around $\sqrt{n}$ other elements. If $k^2\leq n<(k+1)^2$, we make sure we store $S(i)$ for all $i$ between $k^2$ and $n$, and do not store any non perfect square $i$ smaller than $(k-1)^2$(this means we actively clean them up as $n$ grows large). Assume we are deleting, we can delete around $\sqrt{n}$ elements before we hit a perfect square, in that case we would need to recompute the sums from the previous perfect square. It's not hard to see some amortized analysis, the extra space can be made into $O(\sqrt{n})$.

Actually, there is a entire range of space/time trade-offs possible. $O(1/\epsilon)$ amortized time per operation and $O(n^{\epsilon})$ extra space [@swamy1983].

## $\bowtie$, combine two stacks to simulate a deque

First, let's consider we are allowed to add on both side of the sequence, but deletion is only at the beginning of the sequence. 

The idea is to build this through two stacks. This is a very common problem, and we can see the [solutions here](http://www.cs.cmu.edu/afs/cs/academic/class/15750-s01/www/notes/lect0123). One thing to remember is that the left stack uses $\triangleright$, the right stack uses $\triangleleft$.

We can try to simulate the deque with 3 stacks(where $n$ deque operation maps to $9n$ stack operations) [@Petersen2001], but that is just an interesting exercise. We just need to use our two stack set up as in the queue and occasionally rebuild everything. When our front stack become empty and we remove an element in front of our sequence, we just rebuild the structure with two stacks of the same size. There are ways to do the rebuilding without use extra space. The case on the other side is handled symmetrically. Therefore we still maintain $O(1)$ amortized monoid time per operation and $O(\sqrt{n})$ extra space. Finally, we combine the result through $\bowtie$.

{Remark}
	There exist worst case constant time simulation of deque using a few stacks [@Petersen2001]. Thus it is conceivable to make everything from amortized to worst case, with obvious increase in space.

# Examples

I have the code sample for the entire [framework](https://gist.github.com/chaoxu/8c63f1c7e464f26053e6) along with the examples. There are only $6$ functions.

`emptyDecomposableDequeue` initialize the data structure with the corresponding functions. `pushFront`, `pushBack`, `popFront` and `popBack` manipulates the sequences. `measure` returns the result after apply our decomposable function to the sequence.


## Knapsack

Our common dynamic programming formulation is to sequentially compute tables $D_S$, such that $D_S[C]$ contains the maximum value picking objects from $S$ with knapsack capacity $C$.

Usually, we order the objects as $x_1,\ldots,x_n$, and $S_i=\{x_1,\ldots,x_i\}$. We compute table $D_{S_{i+1}}$ from $D_{S_i}$ in $O(C)$ time. This should be the operation for $\triangleright$ and $\triangleleft$. To get the particular entry $D_{S\cup T}[C]$, $O(C)$ time suffice if we are given table for $D_S$ and $D_T$. This would be the $\bowtie$ operation.  

This implies if the sequence size is at most $n$, then for any sequence of $m$ operations, we can dynamically compute all $f$ in $O(mC)$ time using only $O(\sqrt{n}C)$ space. Notice once we convert the general algorithm to work in this special case, it is essentially the [solution by Lei Huang](http://codeforces.com/blog/entry/14366#comment-193779). In fact, this general data structure is inspired by his solution. 

## Maximum subarray sum

The common dynamic programming problem of finding the maximum sum in an array(or, here we consider as a sequence) with both negative and positive numbers. One can compute it in linear time with Kadane's algorithm. This is a decomposable function, too. 

Let $\bigtriangleright_{i=j}^k x_i=(a,b,c,d)$, where $a$ is the maximum sum using the beginning, $b$ is the sum of all elements from the beginning, $c$ is the maximum using the current element and $d$ is the maximum seen so far. $\triangleleft$ is defined similarly. One can see that $(a,b,c,d')\bowtie (a',b',c',d') = \max(a+a',d,d')$. 

## Dynamic sum of a sequence of elements

{Problem}
	Let $x_1,\ldots,x_n$ be a finite sequence of elements from a monoid $(M,+)$. Dynamically maintain the sum of all element in the sequence if we can add and delete element in both end of the sequence.

A simpler version of this problem is asked in [CS theory](http://cstheory.stackexchange.com/questions/18655/maintaining-the-product-of-a-queue-of-semigroup-elements/), where we are allowed to add in one end and delete in another.

Let $f$ be the sum of elements in the sequence, then $f$ is $(+,+,+)$-decomposable. There is a more general statement: 

{Theorem}
	$f$ is a homomorphism, then it is decomposable.

Of course, there is already a data structure support this--a finger tree [@Hinze2006]. Because of the monoid structure, we can even allow concatenations. 

