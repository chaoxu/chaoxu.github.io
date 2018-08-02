---
title: Totally Unimodular Matrices
tags: matrix
---

{Definition}(Totally Unimodular Matrices)

    A matrix $A$ is *totally unimodular* if for all square submatrix $A'$ of $A$, $\det(A') \in \{-1,0,1\}$. 

We will use $TU$ to be the set of all totally unimodular matrices.

By this definition, all the entries in a totally unimodular matrix must be $-1,0,1$ by consider square submatrix of size $1$.

{Proposition}
    
    If $A \in TU$, $A_j$ be the $j$th row of $A$, then 
    
    1. $A^T \in TU$,
    2. $A'$ is a submatrix of $A$, then $A'\in TU$,
    3. \[
         \begin{bmatrix}
           0\\
           A_1\\
           A_2\\
           A_3\\
           \vdots\\
           A_{m}
           \end{bmatrix} \in TU
       \].
    4. If $A$ is a square matrix, then $A^{-1} \in TU$,
    5. If $A'$ is formed by row swapping of $A$, then $A'\in TU$.
    6. Multiply by ${-1,0,1}$ to a row.
    
    
{Proof}

    1. $det(M) = det(M^T)$.
    2. square submatrix of $A'$ is also square submatrix of $A$.
    3. The submatrices that doesn't contain the first row has corresponding submatrix in $A$, ones that does contain the first row has determinant 0.   
    4. see [inverse of a totally unimodular matrix](http://mathoverflow.net/questions/128113/inverse-of-a-totally-unimodular-matrix).
    5. row switching. Consider we switched row $i$ and $j$. Consider any submatrix, if it doesn't contain row $i$ or $j$, then it still has determinant $-1,0,1$. If it contain both row $i$ and $j$, then the determinant is just the negation when the rows are switched back. If it only contain one of row $i$ and $j$, wlog let it be $i$, then it has the rows in order $a_1,\ldots,a_k,i,a_{k+1},\ldots,a_l$, then there is a submatrix in $A$ using the rows in sequence $a_1,\ldots,a_j,i,a_{j+1},\ldots,a_l$, and the absolute value of their determinants are equal. 
    6. multiply by a constant to preserve the $-1,0,1$ properties, then use the argument similar as above, note the determinant of the submatrix can change only by sign.

     
The above properties can be useful to prove many simple statements, for example, operations like duplicate a row, column are closed in $TU$.

$TU$ is not closed under matrix multiplication, consider $\begin{bmatrix} 1 & 1 \\ 0 & 1\end{bmatrix}^2 = \begin{bmatrix} 1 & 2 \\ 0 & 1\end{bmatrix}$. 
