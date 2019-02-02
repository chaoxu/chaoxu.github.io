---
title: Regular expression for a interval of non-negative integers
tags: Haskell, regular expression
---

Let $m,n \in \N$ and $m\leq n$. I wrote [a program in Haskell that generate a regular expression that matches all decimal representation of some number in between $a$ and $b$ inclusive](https://gist.github.com/chaoxu/5210853). The regular expression would have length $O(\log n \log \log n)$. I also included a simple version, which shows how the algorithm is done.

The code was made for the decimal system, but of course it can be generalized to any base.  

Example:

`matchIntRange 123 4321`

`1(2[3-9]|[3-9]\d)|[2-9]\d\d|[123]\d{3}|4([012]\d\d|3([01]\d|2[01]))`

The main idea is to consider a set of tries. Each one contain strings with the same length. The regular expression has a structure closely related to this trie. It be nice if there is a polytime algorithm to generate the shortest regular expression for this problem if all we can use is (), |. 
 
It is important to also handle cases where we will have repeated elements

`matchIntRange 12121212 12121212`

`(12){4}`

This is done by recursively try to compress a free monoid element with the [FreeMonoidCompress](/posts/2013-03-26-represent-an-element-in-a-free-monoid-with-minimum-weight.html) module.

<script src="https://gist.github.com/chaoxu/5210853.js"></script>