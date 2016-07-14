---
title: Continued Fraction Arithmetic in Haskell
tags: Haskell
---

I learned the algorithm to do arithmetic on continued fractions from the [talk by Mark Jason Dominus](http://perl.plover.com/yak/cftalk/). This algorithm can be generalized to handle many other kind of functions.

I wrote an module ContinuedFraction in Haskell. It is an instance of the type class RealFrac. So you can use the four arithmetic operations like it's any other kind of real numbers. The standard numerical Haskell library is clearly not designed by a Algebraists. Instead of Field, Group, Ring and other algebraic structures, they use Real, Fractional and other random stuff that make sense to non-mathematicians.

A continued fraction is implemented as a list of Integral. Infinite continued fraction is possible! Although lazy evaluation make it easy to get only the first $k$ terms, some operation will not terminate. Currently, the only one I know of is comparing two equal infinite continued fractions. So make sure only compare a truncated version of two infinite continued fractions.

It is possible to create formal continued fractions that doesn't correspond to any real number. No one know what will the arithmetic outcomes be.
 
A continued fraction is canonical if the sign of all the numbers are the same. One can create non-canonical continued fractions that does converge to a real number. For example, `ContinuedFraction [1,1,-2]` is actually `ContinuedFraction [3]`. Conversion to canonical version is easy, add it by `ContinuedFraction [0]`. 

Order operations does not work with non-canonical representations. 

The code will stay in [this page](https://github.com/chaoxu/mgccl-haskell/blob/master/random/ContinuedFraction.hs) until one day I have the patience and motivation to make this into a package. This is why I need a internship in a company that actually use Haskell.

I hope people can catch possible bugs. I'm open to suggestions!