---
title: Sum of sparse array in linear time
---

Let $\vec{n}=(n_1,\ldots,n_d)$. Define $\vec{x}\mod \vec{n} = (x_1 \mod n_1,\ldots,x_d\mod n_d)$. We want to represent an $d$-dimensional array. Assume $A$ and $B$ are both $d$-dimensional arrays of size $\vec{n}$ and one have $m$ non-zero elements one have $m'$ non-zero elements. The following operations should be supported. 

1. $circshift(A,\vec{r})$ creates a new sparse array $C$, such that $C((\vec{i}+\vec{r}) \mod \vec{n})=A(\vec{i})$. This is the [circshift operation](http://www.mathworks.com/help/matlab/ref/circshift.html) in matlab. Running time should be $O(m)$.
2. Similarly, we can define $shift(A,\vec{t})$, such that $C(\vec{i}+\vec{t})=A(\vec{i})$, if $\vec{i}+\vec{t}$ is outside the range, we have $0$ instead. 
3. $A+B$ returns a sparse array $C$ such that $C(\vec{i})=A(\vec{i})+B(\vec{i})$ in $O(m+m')$ time. 
4. $elements(A)$ output all elements with associated coordinates in $A$ in $O(m)$ time.
5. $initialize(x,\vec{n})$ create a new sparse array with size $\vec{n}$ and one element $x$ initialized at $\vec{0}$ position. 

We create a sparse list with $d-1$ dimension slices, and the lower dimension slices can be handled inductively. All we need to do is to implement sparse list. The list should store the position and value pairs ordered by position. Sum can be done with a merge operation. circshift go though all elements one by one and update the position, and then rotate the list so the smallest position is in the beginning.

This data structure can be useful output sensitive dynamic programming problems. For example, multidimensional subset sum have a simple recursive solution.

1. Input $S=\{s_1,\ldots,s_n\}$
2. $X\gets \{0\}$.
3. For $i$ from $1$ to $n$, $X\gets X \cup \{x+s_i|x\in X\}$.
4. Return $X$

The running time is $O(|S||X|)$, where $X$ is the final output, since $\{x+s_i|x\in X\}$ is a shift, and union is just a sum. Using circshift, it can produce a output sensitive algorithm for subset sums in finite abelian groups.