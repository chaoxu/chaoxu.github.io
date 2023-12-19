---
title: 一个覆盖问题
---

让$A$为$\{0,\ldots,\ell\}$，最小的$B$有多大，使得$AB=\{ab\pmod n | a\in A, b\in B\}=\Z_n$?

大三本科生[史可](https://keshi.pro/)与我证明了可以找到满足条件的$B$并且$|B|=O(\frac{n}{\ell} \log n)$  。文章可以[这里提取](https://chaoxu.prof/files/papers/cyclic-cover.pdf)。

我很喜欢这个结果，耗时几个月，每次都是过段时间突然有个新想法于是做了点提高。最后的版本和之前的版本差距还蛮大的。以前的文章的最好结果是$\ell$不是太小或者太大的时候$B$的大小为$O(\frac{n}{\ell}\log n\log \log n)$。我们刚开始消灭了$\ell$不是太小的需求，之后一段时间消灭了$\ell$不是太大的需求。那个时候史可就在我们[每周seminar上做了个presentation（有slides）](https://tcsuestc.com/2022/06/10/almost-tight-ell-covering-of-z_n/)。最近一个月突然想通了如何再消掉一个$\log \log n$，并且大大简化之前的结论。有兴趣的可以看全文。
