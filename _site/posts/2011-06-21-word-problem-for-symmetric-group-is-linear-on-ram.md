---
title: Word problem for symmetric group is linear on RAM
tags: BSU REU, computational complexity, group theory
---

# Linear time algorithm for symmetric group

{Problem}(The word problem for symmetric groups)

    **Input:***
    $w$ is a word of length $l$ from the presentation $S_n = \langle x_1,x_2,\ldots,x_n \mid x_i^2 = 1, x_{i+1}x_ix_{i+1} = x_ix_{i+1}x_i, x_ix_j = x_jx_i \rangle$ where $|i-j|\neq 1$.
     
    **Output:**
    Return `true` if $w$ is the identity, else return `false`.

The representation was crucial for coming up with a linear time algorithm respect to $n$ and $l$. This is not a word problem on one group, but on a set of group.

{Theorem}
    The following is a $O(n+l)$ algorithm for the word problem for symmetric groups on RAM.
 
    1. Produce an array `a` of size $n$, Such that `a[i] = i`. (Array start with index 1)
    2. Reading the word letter by letter. If one encounters $x_i$, `swap(a[i],a[i+1])`.
    3. Test if the `a[i] == i` for all $i$. If true, return `true`, else return `false`.

{Proof}
    The algorithm takes $O(n+l)$ time is obvious. The correctness needs to be justified.

    $x_i$ can be represented as the transposition $(i~i+1)$. Define $(n~n+1) = (n~1)$.

    Represent a element of the group as a permutation $\pi$ in the 2 line notation. wlog, assume $\pi(j) = i$ and $\pi(k) = i+1$.

    \[
    \begin{pmatrix} 1 &  \cdots &j & \cdots & k& \cdots & n \\ \pi(1) &  \cdots & i & \cdots & i+1 & \cdots & \pi(n)\end{pmatrix}(i~i+1) = 
    \begin{pmatrix} 1 & \cdots &j & \cdots & k& \cdots & n \\ \pi(1) & \cdots & i+1 & \cdots & i & \cdots & \pi(n)\end{pmatrix}
    \]

    If we call $j$ the index of $i$ if $\pi(j) = i$. Then each transposition is a swap of indices.

    The value of `a[i]` in the algorithm stores the index of $i$. Array `a` represent the identity iff `a[i] = i` for all $i$.

    This proves the the correctness of the algorithm.

The algorithm can be modified so it runs in $O(l n!)$ time for a Turing machine. For each fixed $n$, a Turing machine $M$ can construct another Turing machine $M_n$, such that it store the state of the array as the state of the Turing machine.

This proves every symmetric group is automatic. For any fixed $S_n$, the Turing machine $M_n$ can solve the problem in $O(l)$ time without writing anything to the tape and can only move to the right, which is equivalent to a finite state automata.

{Remark}
    Automatic is a property of a group, not a set of groups. That's why $n$ is ignored in the $O(ln!)$, because it's fixed for each $S_n$. I was confused for a while before I read a concrete definition. 

 
# Algorithms on reduce the word to normal form
The normal form of a word $w\in S_n$ is $w = u_1u_2\ldots u_n$, such that $u_i\in U_i$, and $U_i = \{1, x_n, x_nx_{n-1}, \ldots, x_n\ldots x_1\}$.

One can construct a purely symbolic algorithm that apply only the group relations. We measure the time by the amount of group relations used.

Siegel proposed an $O(l^2)$ algorithm to solve this problem. It is shown in [these slides](http://chaoxuprime.com/files/works/2011summer/week1pres.pdf).

If there exist an algorithm $A(w)$ that write the word $w$ of length $l$ in normal form in $O(f(l,n))$ time., then one can always make it into an algorithm taking $O(l f(n^2,n))$ time.

Observe that $w = w'yz$ where $y$ and $z$ are words in normal form, and the length of $|z|$ is maximized. $A(w) =A(w'A(yz))$. Here $y$ can be worst case, one single letter, it doesn't change the complexity. Let's introduce two algorithms. A' and A''.

$A'(w)$ first find the $z$ in the description, then returns the value of $A''(w',A(x_iz))$, where $w = w'x_iz$.

Recursively calculate $A''(w,z)$ with the following definition.
$A''(1,z) = z$.
$A''(w,z) = A''(w', A(x_iz))$, where $w = w'x_i$.

{Theorem}
    $A(w) = A'(w)$ and runs in $O(l f(n^2,n))$ time.

{Proof}
    $A''(w,z)$ can ran at most $l$ times, each time it makes a call to $A(w)$, contribute the factor $O(f(n^2,n))$.
 
In particular, Siegel's algorithm can be modified to run in $O(l n^4)$ time.