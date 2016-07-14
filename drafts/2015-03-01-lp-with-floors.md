---
title: Linear programming with floor functions
---

If $x,y\in \R^n$, define $\lfloor xy \rfloor$ to be $\sum_{i=1}^n \lfloor x_iy_i \rfloor$. If $A\in \R^{m\times n}$, then $\lfloor Ax \rfloor = \begin{pmatrix} \lfloor A_1 x \rfloor \\ \vdots \\ \lfloor A_m x \rfloor \end{pmatrix}$.

Let's consider the following problem, given $A\in \R^{m\times n},A'\in \R^{m'\times n}$, $b\in \R^m$, $b'\in \R^{m'}$, $c,x\in \R^n$.

\begin{align*}
\text{min }   & cx\\
\text{s.t }   & Ax\leq b\\
              & \lfloor A'x\rfloor \leq b'\\
\end{align*}

We are interested in solving this kind of LP.
Notice there was a special case considered

# Reference