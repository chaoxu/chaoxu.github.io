---
title: Finger tree allowing apply functions to each element
tags: data structure, lazy propagation
---
 
Let $(M,+)$ be a monoid. We are interested in maintaining a sequence $a = a_1,\ldots,a_n\in M$ under all updates currently supported by finger tree. However, we are also interested in adding another update, which we call the function update.

Let $f = f_1,\ldots,f_n\in M\to M$. 
   
The functions satisfies the following property.

$f_1(x_1)+\ldots+f_n(x_n) = f_1(y_1)+\ldots+f_n(y_n)$ if $\sum_{i=1}^n x_i = \sum_{i=1}^n y_i$.

The sequence $f$ is given implicitly, where it has two methods:

 - `evaluate(X)`: It returns $f_1(x_1) + \ldots +f_n(x_n)$ for any sequence $x_1,\ldots,x_n$ such that $\sum_{i=1}^n x_i = X$. 
 - `split(j)`: returns a representation for $f_1,\ldots,f_j$ and $f_{j+1},\ldots,f_n$.

We are interested in implementing `FunctionUpdate(a,f)`, the output would be a representation of the sequence $f_1(a_1),\ldots,f_n(a_n)$.

Many problems actually require update to a entire interval of the sequence, which makes this extremely valuable. For example, consider the following simple problem.

Maintain a sequence of integers $a_1,\ldots,a_n$, such that we can do the operation $inc(i,j)$, which increment all numbers from $i$th to $j$th index by $1$. That is, the new sequence is $a_1,\ldots,a_{i-1},a_i+1,\ldots,a_j+1,a_{j+1},\ldots,a_n$. Also, it has a function $value(i)$ which returns $a_i$. This problem can be solved by finger tree with function update operation. 

I want an actual implementation of such data structure so I can implement the min-cost flow algorithm for series-parallel graphs [@Booth1993416]. 