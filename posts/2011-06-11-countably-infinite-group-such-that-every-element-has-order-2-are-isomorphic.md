---
title: Countably infinite groups such that every element has order 2 are isomorphic
tags: group theory, puzzle
---
I once saw the following puzzle: 

{Problem}
    Given a list of $2n-1$ non-negative integers. Every number except one appeared twice. The memory that contain the integers are read only. Can you use $O(1)$ additional space to find the integer that only appeared once?
 
The solution was the xor function.
 
If $a_j$ is the number that didn't appear twice,
\[
\bigoplus_{i=1}^{2n-1} a_i = a_j
\]

The reason was because xor have the following property.
$a \oplus b = b \oplus a$, $a \oplus a = 0$ and $0 \oplus a = a$ for all $a,b\geq 0$.
One can see $(\mathbb{N}, \oplus)$ is a abelian group. 

Is this the unique function to solve this problem?

In some way, yes. 
Here is a theorem.

{Theorem}
    The countably infinite group $G$ such that $g^2 = 1$ for all $g\in G$ is $(\mathbb{N}, \oplus)$ up to isomorphism.

{Proof}
    
    [See the answer by Pete Clark](http://math.stackexchange.com/a/17057/96).