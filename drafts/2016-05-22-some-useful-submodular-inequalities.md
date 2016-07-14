---
title: Useful inequalities for submodular functions
tags: submodular
---

If $f$ is non-negative symmetric submodular, then we can always assume it is normal. Since we can define $g=f(S)-f(\emptyset)$, which is also non-negative symmetric submodular. This is true because $f(\emptyset)$ take on the smallest value, because $2f(S) = f(S)+f(V\setminus S)\geq 2f(\emptyset)$.


If $f$ is non-negative symmetric, then

$f(S\cup T)+f(T)\geq f(S)\geq f(S\cup T)-f(T)$.

First, $f(S)+f(T)\geq f(S\cup T)+f(S\cap T)$, so $f(S) \geq f(S\cup T)+f(S\cap T) - f(T)\geq f(S\cup T)- f(T)$.

$f(\overline{S\cup T})+f(T)=f(\overline{S}\setminus T)+f(T) \geq f(\overline{S})=f(S)$.