---
title: More algorithms on perfectly balanced photo gallery
---

Recently there was [an article](http://www.crispymtn.com/stories/the-algorithm-for-a-perfectly-balanced-photo-gallery) by [Johannes Treitz](https://twitter.com/jtreitz) [submitted to Hacker News](https://news.ycombinator.com/item?id=6198400) about how to display a set of pictures as nicely as possible. The article doesn't have a formal description of the problem, so here is my take on what it mean by perfectly balanced.

{Problem}
    
    Given a sequence of $n$ rectangles where the $i$th rectangle has width $W_i$ and height $H_i$. Also given are width $W$ and target height $H$.
    Partition the sequence to $k$ consecutive subsequences, such that we scale the rectangles keeping the aspect ratio, and each subsequence of rectangles fills the entire width $W$, and is close to the target height $H$.
    
The problem is not so well defined. To make sure the heights are close, do we minimize the sum of all differences? Minimize the maximum difference? Or maybe, we just want to minimize the difference between consecutive rows, such that the first row has height close to $h$.

Nevertheless, in real applications, the exact definition doesn't matter that much. Treitz reduce the problem to the linear partition problem.

{Problem}
    
    Let $a_1,\ldots,a_n$ be a sequence of positive reals. We want to partition it into $k$ consecutive subsequences, such that the maximum sum over each subsequence is minimized. 
    Formally, find $k+1$ positions $b_1=1,b_2,\ldots,b_{k},b_{k+1}=n$, such that $\max_{i=1}^{k} \sum_{j=b_i}^{b_{i+1}} a_j$ is minimized.

$a_i = W_i/H_i$ is the aspect ratio. This article will explore the techniques to solve the problem in $O(kn)$ time. It is only a high level overview, and leaves the details unfilled. 

# Minimize total difference

Let's first consider a simpler problem for demonstration.

{Problem}
    
    Let $a_1,\ldots,a_n$ be a sequence of positive reals. We want to partition it into $k$ consecutive subsequences, such that the total difference between the sum of each consecutive sequence and the average sum of the whole sequence is minimized. 
    Formally, let $\mu = \frac{1}{k}\sum_{i=1}^n a_i$. Find $k+1$ positions $b_1=1,b_2,\ldots,b_k,b_{k+1}=n$, such that $\sum_{i=1}^{k} |\sum_{j=b_i}^{b_{i+1}} a_j - \mu|$ is minimized.

We will solve this problem with a reduction to a problem on a DAG, and then apply dynamic programming. Since we can always turn a problem that ask us to find the *cost* to a problem that ask us to find a *construction* that achieve the cost(with the same time/space bound). We will only concern with the cost version of the problem as it's much clearer. (One can implement some arrows in Haskell to make DP for construction as easy as DP for cost, sounds like a nice project. )

Build a directed graph $D(V,A)$, where $V=\{v_1,\ldots,v_n\}$, $(v_i,v_j)\in A$ if and only if $i\leq j$. (We can make it $i < j$ instead, depend on if we consider a empty sequence a valid sequence.)

We have a weight function $w(i,j)$ that assign weights to arc $(v_i,v_j)$. Define $w(i,j) = |\sum_{k=i}^{j-1} a_i - \mu|$. 

If we find a $k$-edge path of minimum weight from $v_1$ to $v_n$, then this implies a solution to [Problem 3].

# Solve the minimum weight $k$-edge path problem
Define $C(d,i)$ to be the $d$-edge path from $v_1$ to $v_i$ with minimum weight. We want to find $C(k,n)$.

\[
C(d,i) = \min_{1\leq j\leq i} {C(d-1,j) + w(j,i)}
\]

If we set $w(j,i)=\infty$ if $j>i$, then we have a better representation.

\[
C(d,i) = \min_{1\leq j\leq n} {C(d-1,j) + w(j,i)}
\]

There are $kn$ entries in the DP table for $C$, and $C(d,i)$ requires $O(n)$ time to compute. This means the algorithm will take $O(kn^2)$ time. 

# Improve the time complexity
There is a standard technique on totally monotone matrices that can reduce the complexity of the problem to $O(kn)$. 

{Definition}

    A weight function $w$ is Monge if for every $1<i+1<j\leq n$, we have 
    \[
    w(i,j) + w(i+1,j+1)\leq w(i,j+1) + w(i+1,j)
    \]
    . 
{Theorem}

    $w$ is Monge.
    
{Proof}
    
    Let $\sum_{k=i+1}^{j-1} a_i - \mu = m$
    \begin{align*}
    w(i,j)+w(i+1,j+1) &=  |\sum_{k=i}^{j-1} a_i - \mu|
                        + |\sum_{k=i+1}^{j} a_i - \mu|\\
                      &= |a_i + m| + |a_j + m|\\
                      &\leq |a_i+a_j+m| + |m|\\
                      &= w(i,j+1)+w(i+1,j)
    \end{align*}
    To prove the $\leq$, see that $a_i,a_j$ are positive, one can consider either $m$ is negative or positive, and notice either way the inequality holds true. 

{Remark}

    We can replace $|\cdot|$ with $|\cdot|^p$ for $p\geq 1$. When $p=2$, we minimizes the variance(and standard deviation).
    
{Definition}
    
    A matrix is totally monotone if for every $i<i'$ and $j<j'$, $a_{i,j} > a_{i',j} \implies a_{i,j'} > a_{i',j'}$. 

![totally monotone](/files/totallymonotone.jpg)
<br /><sup>Image Credit: [Vanessa Li](http://vanessa.li).</sup>

{Remark}
    
    There are isomorphic definition of Monge and totally monotone, depend on if the person want to find row or column minima. 

Define a matrix $M^d$, such that $M_{j,i}^d = C(d-1,j) + w(j,i)$, the original recurrence become 
\[
C(d,i) = \min_{1\leq j\leq n} {M^d_{j,i}}
\]
In other words, $C(d,i)$ is the $i$th column's minima of $M^d$. 

{Theorem}

    If $w$ is Monge, then $M^d$ is a $n\times n$ totally monotone matrix.

Using the [SMAWK algorithm](http://en.wikipedia.org/wiki/SMAWK_algorithm), all column minimas can be found in $O(n)$. Finding $C(d,i)$ takes only $O(1)$ time on average!

Here is the very simple code to show how this can be done easily if we have a [Haskell implementation of the SMAWK algorithm](http://dailyhaskellexercise.tumblr.com/post/57781874558/column-minima-in-a-totally-monotone-matrix). The indexing is a bit different from the description in the article.

    import Data.Array
    import SMAWK

    minCostkEdgePath k n w = d!(k-1,n)
      where
        d = array ((0,0),(k-1,n)) [ ((i,j), f i j) | i<-[0..k-1],j<-[0..n]]
        f 0 i = w 0 i
        f k i = m k (p!(k,i)) i
        p = array ((1,0),(k,n)) [((z,i),t) |z<-[1..k],(i,t)<-zip [0..n] (columnMinima (m z) (n+1) (n+1))]
        m k j i = (+) (d!(k-1,j)) (w j i)

    minCostMuPartition k xs = minCostkEdgePath k n w
        where 
            w i j 
             | i <= j    = abs $! s!j - s!i - avg
             | otherwise = 2*m
            s   = listArray (0, n-1) $ scanl (+) 0 xs
            n   = length xs
            m   = sum xs
            avg = m/fromIntegral k

# Solve the linear partition problem

Now, returning to the original problem. Again, we can reduce the problem to a problem on a directed graph. This time, $w(i,j) = \sum_{k=i}^{j-1} a_i$. The weight of a path is the maximum weight of the edges in the path. A $k$-edge path with minimum weight implies the solution to the original problem.

\[
C(d,i) = \min_{1\leq j\leq n} M^d_{j,i}
\]

where $M^d_{j,i} = \max (C(d-1,j),w(j,i))$.

Just like [Problem 3], this describes a $O(kn^2)$ time algorithm. Compare $w,C$ and $M^d$ with the previous problem to see there isn't much difference. 

# Improve the time complexity, again

Can we define an alternative to the Monge property? Yes, we can extend this to *algebraic Monge property*. Just replace $+$ with some associative operation $\oplus$, $\leq$ with a total order that is ordered with respect to $\oplus$, i.e. $a \leq a\oplus b$. 

So it seems, using the algorithm above with little modification, we can solve the problem in $O(kn)$ time because we can just replace $+$ by $\min$.

However, algebraic Monge property in general doesn't imply totally monotone matrix. 

Consider the simple matrix, and our operation is $\max$.

\begin{bmatrix}
1& 2\\
0& 2
\end{bmatrix}

$\max(1,2)=\max(0,2)$, but the matrix is not totally monotone. $1>0$ but $2\not > 2$.

If instead the operation is strictly compatible, i.e. $a \oplus b< a \oplus c$ if $b < c$, then we can always produce a totally monotone matrix. This is not the case with $\max$. 

Some readers who are familiar with [$L^p$ space](http://en.wikipedia.org/wiki/Lp_space) might point out for any sequence of reals $a_1,\ldots,a_n$ and $\epsilon>0$, there exist a $p\geq 1$, such that $(\sum_{i=1}^n |a_i|^p)^{1/p} - \max_{i=1}^n |a_i|< \epsilon$. So if we don't need to be exact, pick a large enough $p$ as an exponential is good enough. This opens up a can of numerical analysis, and that's undesirable...

Good news, a more restrictive but good enough variant of the Monge property hold.

{Definition}
    
    $w$ has the *strict bottleneck Monge property* if either of the following is true for $1<i+1<j$:
        
    1. $\max(w(i,j), w(i+1,j+1)) < \max(w(i+1,j),w(i,j+1))$.
    2. $\max(w(i,j), w(i+1,j+1)) = \max(w(i+1,j),w(i,j+1))$ and $\min(w(i,j), w(i+1,j+1)) \leq \min(w(i+1,j),w(i,j+1))$.

Our $w$ in consideration has strict bottleneck Monge property. Because all the numbers are positive, $w(i,j+1)>w(i+1,j+1)>w(i+1,j)$, $w(i,j+1)>w(i,j)>w(i+1,j)$. A simple case check on the relation between $w(i,j)$ and $w(i+1,j+1)$ will give the desired proof. Why this property? You can check by a case by case proof that this property implies total monotonicity.

{Theorem}

    If $w$ has the strict bottleneck Monge property, then $M^d$ is a $n\times n$ totally monotone matrix.
    
There seems to be a direct proof by analyze 12 different cases, but I got too bored after the second case. One can read [@Bein2005455] for the proof. It doesn't contain the exact theorem, but a result that implies this one. The proof is quite involved. Their idea is to construct a new matrix with a new operation over sorted sequences of numbers instead of just numbers. Prove this matrix is strictly compatible and has the algebraic Monge property, and show this can always be done if the original matrix has the strict bottleneck Monge property. 
 
The final punchline.
    
{Theorem}

    Change the last `(+)` in `minCostkEdgePath` to `max`, and change how `w` is computed in `minCostMuPartition` solves [Problem 2].

# Rethink the original problem

We have developed a $O(kn)$ algorithm for [Problem 2]. As we can see from the practical application, this is a $O(n^2)$ algorithm because the $k$ is of the order $n$.

For a few hundred photos, we can afford to run it in real time. This will reach a limit once there are thousand of photos.

Can we do better? Yes, by considering a different kind of reduction(thank god we didn't formally define what *close* means). Instead of compute $k$ and try to fit $k$ rows, why not just make sure each row's width is almost the width we want and scale accordingly? We want each rows to approximately have width $W$, we don't really care how many rows there are.

{Problem}
    
    Let $a_1,\ldots,a_n$ be a sequence of positive reals, and a number $\mu = W/H$. We want to partition it into consecutive subsequences, such that the maximum difference between the sum of each consecutive sequence and the $\mu$ is minimized. 
    Formally, find a $k$ and a sequence of $k+1$ numbers $b_1=1,b_2,\ldots,b_{k},b_{k+1}=n$, such that $\max_{i=1}^{k} |\sum_{j=b_i}^{b_{i+1}} a_j - \mu|$ is minimized.

This is the exact same problem described on section 5 of [@Bein2005455]. It can be solve in $O(n)$ time. 


