---
title: Word problem for braid group using a representation
tags: braid group, BSU REU, group theory, Haskell
---

{Problem}(The word problem for braid groups)
    **Input:**
    $w$ is a word of length $l$ from the presentation $\langle \sigma_1,\sigma_2,\ldots,\sigma_{n-1} \mid  \sigma_{i+1}\sigma_i\sigma_{i+1} = \sigma_i\sigma_{i+1}\sigma_i, \sigma_i\sigma_j = \sigma_j\sigma_i \rangle$ where $|i-j|\neq 1$.
       
    **Output:**
    Return `true` if $w$ is the identity, else return `false`.

The word problem for braid group was solved a long time ago with Artin combing. It requires one to express pure braid group $P_n$ as a semidirect product of free groups $F_1,\ldots,F_{n-1}$. It is slow and quite difficult, at least I wasn't able to come up with an explicit algorithm for it.

Another way was to create a faithful homomorphism $^*: B_n \to \mathop{\mathrm{Aut}}(F_n)$. This is what I implemented in Haskell.

If $B_n \to \mathop{\mathrm{Aut}}(F_n)$ is faithful and it sends $w$ to $w^*$, then $w$ is $I$ iff $w^*(a) = a$ for every generator $a$ of $F_n$.

In the [a survey by Jonathan Boiser](http://campillos.ucmerced.edu/~jboiser/boiserMSThesis.pdf), there is one explicate map. Defined as
\[
\sigma_i^*(t_j) =
\begin{cases}
 t_j & \text{if } j\neq i, i+1 \\
 t_{i+1} & \text{if } j=i \\
 t_{i+1}t_it^{-1}_{i+1} & \text{if } j=i+1\\
\end{cases}
\]

Where $t_i$ are generators of $F_n$, and $\sigma_i$ are the generators of the braid group. The inverse can easily be found.

Using a recursive definition. Let any word of the form $(\sigma_i w)^*$ be $\sigma_i^* \circ w^*$. The algorithm is obvious. Test if $w^*(t_i) = t_i$ for every generator in $F_n$ is applying a list of $\sigma_i$ to elements in $F_n$.
 
I wrote the program in Haskell, and [posted it on github](https://gist.github.com/1041985).
<script src="https://gist.github.com/1041985.js?file=word_problem_braid_group.hs"></script>

The analysis: Given a word with length $l$ in $B_n$, how long does it take to solve the problem with this algorithm?
Each application of $\sigma_i^*(u)$ to some word $u$ takes $O(|u|)$ time. One then free reduce. $\sigma_i$ is applied $l$ times.

An application of $\sigma_i^*$ can potentially triple the length of the word. If one shows that it can only increase the length of the word by a constant term(given one started from a generator), then we have a $O(l^2 n)$ algorithm.

Braid groups are automatic. If this natural algorithm solves the problem in $O(l^2 n)$ time, it is not a surprise. However, it is likely not true. The running time is more likely to be $O(c^l n)$ for some constant $c$. Although I can prove neither $\sigma_i^*$ and increase the length by a constant factor or by a constant. The best known algorithm in 2000 was provided by [a paper of Hessam Hamidi-Tehrani](http://www.sciencedirect.com/science/article/pii/S0166864199000632). It runs in $O(l^2 n  + l n \log n)$ time, and was proved with advanced techniques.

**Update 06/24/2011**: Siegel provided a example in $B_3$ where the algorithm run in exponential time.
$((\sigma_2\sigma_1^{-1})^n)^*(t_1)$. 

If we define $a_n,b_n,c_n$ to be the amount of $t_1,t_2,t_3$ (include it's inverses) at step $n$, ignoring the possibility of cancellation. We have the following recurrence relation.

\begin{align*}
a_{n+1} &= 2a_n + b_n\\
b_{n+1} &= c_n\\
c_{n+1} &= a_n+2c_n\\
\end{align*}

One can show $a_n+b_n+c_n = 2F_{2n+1}-1$. It is also true that cancellations are not possible. 