---
title: Linear time algorithm for the word problem on $B_3$
tags: braid group, BSU REU, computational complexity
---

The word problem is solved if one can put $w$ in Garside normal form. The process is trivial in $B_3$ and can be done in linear time on a Turing machine. Remember $\Delta = \sigma_1\sigma_2\sigma_1$.

1. Rewrite all generator with negative exponents into ones with positive exponents and a $\Delta^{-1}$.
2. Move every $\Delta^{-1}$ to the beginning of the word. First locate the last $\Delta^{-1}$, and move it forward using the relations $\Delta^{2n+1}\sigma_i = \sigma_{3-i} \Delta^{2n+1}$ and $\Delta^{2n}\sigma_i = \sigma_i\Delta^{2n}$. Pick up other $\Delta^{-1}$'s in between.
3. Move every $\Delta$ factor in the word to the beginning of the word. Read from the end of the word, move it forward using $\Delta$ relations, pick up other $\Delta$ on the way. Check if moving $\Delta$ generates another $\Delta$. $B_3$ don't have $\sigma_i\sigma_j = \sigma_j\sigma_i$ relation, therefore if a new $\Delta$ is created from moving $\Delta$'s around, it is a local event. To be precise, during this process, $x\Delta^m y = x\Delta^{m+1} y'$ if and only if the first 3 letter in the current presentation of $y$ is $\Delta$. 


 A demonstration on a positive word.
\begin{align*}
\sigma_2\sigma_1\sigma_1\sigma_2\sigma_1\sigma_1\sigma_1\sigma_1 &= \sigma_2\sigma_1\sigma_1\sigma_2\sigma_1(\sigma_1\sigma_1\sigma_1)\\
&= \sigma_2\sigma_1\sigma_1\sigma_2(\sigma_1\sigma_1\sigma_1)\sigma_1\\
&= \sigma_2\sigma_1\sigma_1(\sigma_2\sigma_1\sigma_1)\sigma_1\sigma_1\\
&= \sigma_2\sigma_1(\sigma_1\sigma_2\sigma_1)\sigma_1\sigma_1\sigma_1\\
&= \sigma_2\sigma_1\Delta\sigma_1\sigma_1\sigma_1\\
&= \sigma_2\sigma_1\Delta(\sigma_1\sigma_1\sigma_1)\\
&= \sigma_2\Delta(\sigma_2\sigma_1\sigma_1)\sigma_1\\
&= \Delta(\sigma_1\sigma_2\sigma_1)\sigma_1\sigma_1\\
&= \Delta^2\sigma_1\sigma_1\\
&= \Delta^2(\sigma_1\sigma_1\cdot 1)\\
&= \Delta^2\sigma_1^2\\
\end{align*}

I wrote a [Haskell implementation of the algorithm](https://gist.github.com/1066286).
 <script src="https://gist.github.com/1066286.js?file=word_problem_b3.hs"></script>