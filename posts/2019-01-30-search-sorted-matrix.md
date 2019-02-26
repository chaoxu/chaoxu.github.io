---
title: Search in a sorted matrix with an oracle
tags: algorithm, data structure
---

Consider a infinite matrix $M$. Another way to think about it is a function $f:\N\to \N\to X$.
A matrix $M$ is sorted if $M_{i,j}\leq M_{i,j+1}$ and $M_{i,j}\leq M_{i+1,j}$.

{Problem}
    Given a sorted matrix $M$, and an oracle that takes $\lambda$ returns if a value is $\lambda <\lambda^*$ or $\lambda \geq \lambda^*$. Find the largest value no larger than $\lambda^*$.

Assuming there are at most $k$ elements no larger than $\lambda^*$, and we know the smallest $n$ and $m$ such that $M_{i,j}>\lambda^*$ if $i>n$ or $j>m$. Also, let $t$ be the smallest number such that $M_{t,t}>\lambda^*$. One can see that $t\leq \min(n,m)$ and $k=O(\max(n,m)^2)$. 

Let's first consider the case when $n$ and $m$ is known and $n\leq m$. It is [Leetcode 240. Search a 2D Matrix II](https://leetcode.com/problems/search-a-2d-matrix-ii/). However, our problem is more general, because comparison with $\lambda^*$ can only be done through the oracle. 
Craig Gidney wrote about an [optimal algorithm](http://twistedoakstudios.com/blog/Post5365_searching-a-sorted-matrix-faster) with $O(n\log \frac{m}{n})$ running time, matrix access algorithm. 
However, the oracle access is too large. There are times where the oracle access is slow. For example, when using it as a subroutine for finding a [bottleneck $k$-link path](https://chaoxuprime.com/posts/2019-01-31-bottleneck-k-link-path.html).
There is an algorithm with optimal running time and $O(\log(nm))$ oracle access. 

Let's consider a special case, where $n=m=2^i$ for some $i$. This case was shown in [@FredericksonZ17].
For each submatrix, the two vertices on the opposite diagonal indicates the largest and smallest element in the submatrix. Hence each matrix can be represented by two numbers, indicate the maximum and minimum. These numbers are called the representative of the matrix. The idea is we keep two numbers $\lambda_1$ and $\lambda_2$, such that we know $\lambda^*\in [\lambda_1,\lambda_2]$. 
The algorithm keep partition the matrix into small matrices, updating $\lambda_1$ and $\lambda_2$, and discard matrices outside the range. 
We apply the following algorithm. Let $R$ consists of the multiset of representatives of the matrix, and $R'$ be the representatives that lies inside $[\lambda_1,\lambda_2]$. We find $\lambda$, the median of $R'$. Test if $\lambda<\lambda^*$. If so, then $\lambda_1$ updates to $\lambda$, otherwise $\lambda_2$ updates to $\lambda$. This step is done twice. 
Now, we split the matrices with more than one element into $4$ equally sized matrices, and repeat the algorithm.
Recall at all times, the matrices does not contain any element in $[\lambda_1,\lambda_2]$ are discarded. 

There is at most $O(\log n)$ iterations before range shrinks to a single element, hence at most $O(\log n)$ oracle calls. The difficulty is to show that the overall time is only $O(n)$. Intuitively, in each iteration we quadruple the number of matrices, but we half it by two calls to the oracle. Therefore in $\log n$ steps we obtain roughly $2^{\log n}=O(n)$ matrices. However, at this point, the matrices are all singletons, and no more matrix can be created. We will only decrease the number of matrices by each oracle call. Careful reader can trace the whole argument in Lemma 2.1 of [@FredericksonZ17].

For the more general case, one can find the proof in [@FredericksonJ84]. Note the proof is for selection, but one can easily modify it to work for search. 

Now, $n$ and $m$ is not known, but we can quickly using exponential search to find it. Indeed, we just have to apply exponential search in the first row and first column using the oracle. This gives us an extra $O(\log n + \log m)=O(\log nm)$ oracle calls. 

Let $k$ to be the number of elements no larger than $\lambda^*$. We can get running time relative to $k$. Use exponential search until we find the first $i$ such that $M_{2^i,2^i}>\lambda^*$. So we can upper bound $t$. Then one can solve the problem with $2$ matrices. One $t\times k$ matrix and a $k\times t$ matrix. The total running time is therefore $O(\log k+t\log k/t)=O(t\log k)$. In fact, we get $O(\log k)$ oracle calls and $O(t\log k)$ running time. Here we can take $t$ to be $\sqrt{k}$, and obtain $O(\sqrt{k}\log k)$ time.

Note if we relax on number of oracle calls. I know how to get a $O(\sqrt{k})$ running time.

{Theorem}
    Given $\lambda^*$ and a $n\times m$ sorted matrix such that the $i$th row has $k_i$ elements no larger than $x$. Let $k=\sum_{i} k_i$. We can find $\lambda^*$ in $O(\sum_{i} \log (k_{i+1}-k_i+1) ) = O(n \log \frac{k/n^2})$ time.

The idea is simple, we do exponential search on each row to find the largest element no larger than $\lambda^*$, but we reuse information from the previous row. This gives us the running time $O(\sum_{i} \log (k_{i+1}-k_i+1) )$. The main difficulty is to show why is is $O(n \log \frac{k}{n^2})$. 
Once we show that, we can use the theorem to obtain $O(\sqrt{k})$ running time.

# Remark

There is an alternative algorithm which can be found in [@JacobR08]. The alternative algorithm is quite close to a post about [selection in a sorted matrix](https://chaoxuprime.com/posts/2014-04-02-selection-in-a-sorted-matrix.html).

The careful reader might observe the known search algorithms follow the exact same structure as algorithms for selection. Indeed, we *are* doing selection but we do not know the rank of the element. Intuitively, many selection algorithm, the rank is *only used* to remove the correct set of candidates. Hence this suggest one can modify the algorithm to use the oracle call in place of the rank. 
