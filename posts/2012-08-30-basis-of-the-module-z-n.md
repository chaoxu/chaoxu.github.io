---
title: Basis of the module $\Z^n$
tags: math
---

A student who is taking linear algebra asked me the following problem.

If we consider the field $\R$ restricted to $\Z$, and create a "vector space" on $\Z$. How do we know if $v,u\in \Z^2$ "spans" $\Z^2$?

Formally, what can we say about $v$ and $u$ if for every $w\in \Z^2$, there exist $n,m\in \Z$, such that $nv + mu = w$.

We can generalize it and put it in terms of modules, as $\Z$ is only a ring but not a field.

{Theorem}
    $v_1,\ldots,v_n$ is a basis for the module $\Z^n$ iff the matrix $M$ formed by the vectors is a unimodular matrix.

{Proof}
    $\Rightarrow$
    If $\det(M)=0$, then $v_1,\ldots,v_n$ are not linearly independent.
    If $|\det(M)|\geq 2$, then the parallelepiped formed by $v_1,\ldots,v_n$ has volume $\geq 2$. If there is any integer point not on the corners of the parallelepiped, then that point can't be written as linear combination of $v_1,\ldots,v_n$. Notice that it must contain some lattice points not on the corners of the parallelepiped. One can see why by consider a large box that contain volume of $m$ such parallelepiped, but contain at least $2m$ lattice points. 
    
    This shows if $M$ is not unimodular, then $v_1,\ldots,v_n$ can't be a basis. 
    
    Alternative proof: $M$ is not unimodular then $M^{-1}$ contain a non-integer entry. This shows there exist a $b$, such that the solution $x$ to $Mx=b$ contain a non-integer entry. (proposed by Thao Do)
    
    $\Leftarrow$
    $|\det(M)|=1$ implies it has a inverse over $\Z$, thus $Mx = b$ for any $b\in \Z^n$ always has a solution. 