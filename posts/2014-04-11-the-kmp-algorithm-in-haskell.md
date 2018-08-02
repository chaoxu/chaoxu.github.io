---
title: The KMP algorithm in Haskell
---

Almost all the string algorithms I read are doing index manipulations somewhere. 
However meddling with indices are never a smart move in pedagogical settings. 
For example, see the code for the [KMP algorithm in C](http://www-igm.univ-mlv.fr/~lecroq/string/node8.html). 
It's short, to the point, and elegant in it's own way. 
Except it's hard to see the meaning behind all the operations.

As an example, this article demonstrates how to write the KMP string matching algorithm without all those indices. 

The KMP string matching algorithm solves the following problem.

{Problem}
    Given a string $pat$ of length $m$, return if it exist in $text$ of length $n$ in $O(n)$ time.

Half of the KMP algorithm implementations are actually [the MP algorithm](http://www-igm.univ-mlv.fr/~lecroq/string/node8.html).
[Twan van Laarhoven's implementation](http://twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell), the earlier version of the [KMP](http://hackage.haskell.org/package/KMP-0.1.0.2) package and even [Wikipedia's page](http://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm). 
Although both KMP and MP runs in $O(n)$ time, KMP uses at most $O(\log m)$ time to advance to match the next element in the list when MP could take $O(m)$ comparisons. More concretely, KMP could output the sequence $m_0,\ldots,m_{n-1}$ in $O(n)$ time, where $m_i=1$ iff $pat$ is a suffix of $text[0..i]$, and the time between output is $O(\log m)$.  
This added benefit comes at a cost.
In the MP algorithm, the failure table has only one $-1$. The failure table for KMP, there is no $-1$, and everything just goes to $0$ instead.
If one tries to not use any indices, and want to separates the searching and the table building, one would need a special element to treat the $0$ positions.

Comparing to the [KMP package](http://hackage.haskell.org/package/KMP-0.1.0.2), the implementation here doesn't use any array.

# The algorithm

We build the a automaton $A$ using a input string $S = a_0,\ldots,a_{m-1}$. 
The automaton consist of states $nil,s_0,\ldots,s_{m-1}$.

Because of laziness, it is built incrementally.
The total running time is $O(n)$, even if $m$ is much larger than $n$.

$\delta$ is the transition function, $\delta(s_i,a_i)=s_{i+1}$, and $\delta(s_i,x)=f_i$ for $x\neq a_i$, where $f_i$ is the failure state associated with state $s_i$. 
We call $a_i$ the value of $s_i$.

One would also want to know if an state is accepting state or not. This prompt us to use the following declaration for the automaton.  

```haskell
data Automaton a = Node {value   :: a,
                         success :: Automaton a,
                         failure :: Automaton a,
                         accept  :: Bool
                         } | 
                   Null {success :: Automaton a,
                         accept :: Bool}

isNull (Null _ _) = True
isNull _ = False
```

Assume we have built a automaton, then doing a matching is quite easy. Just simulate the automaton.
It's important to notice the `next` and `stay` are saying if we want to read the next character or stay at the same character.
The code below is a generalized version of matching. It basically does a fold while we match.
`isInfixOf'` is an example of how to use `matchFold` to test if a list is a infix in another list.

```haskell
matchFold :: Eq a => Automaton a -> [a] -> ([a]->b->b) -> ([a]->b->b) -> b -> b
matchFold _ [] _ _ identity = identity
matchFold state text nomat mat identity = match' state text
  where match' _ [] = identity
        match' a (x:xs)
          | not (isNull a) && value a /= x = stay
          | not (accept a)                 = nomat (x:xs) next
          | otherwise                      = mat   (x:xs) next
          where next = match' (success a) xs
                stay = match' (failure a) (x:xs)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' pattern text
 | null pattern = True
 | otherwise    = or $ matchFold (buildAutomaton pattern) text (const (False:)) (const (True:)) []
```

It is obvious how we can build the entire automaton except for the failure transition. 

Another way to reason about it. 
We impose an order on a set of strings $S$ by measuring the length of the string, so this order is a linear order when the set of strings have different length. Let $Prefix(s)$ to be the set of all prefix of $s$. $s^R$ to be the reverse of $s$.
\begin{align*}
Border(s) &= Prefix(s)\cap Prefix(s^R)\\
Border'(sa) &= \{x|x\in Border(s),xa\not\in Border(sa)\}\\
b(s) &= \max(Border(s)\backslash \{s\})\\
b'(sa) &= \max(Border'(sa))
\end{align*}

Assume we try to compute $b(x)$ for a string $x$, we first build the function $next$, such that $next$ iterates through $Prefix(x)$ by length, and $last$ is a function that returns the last element in the string.
We have the following relation for $sa$ a prefix of $x$, where $s$ is a string and $a$ is in the alphabet.

\[
b(sa) = \begin{cases} next(b(s)) & last(next(b(s))) = a\\
        next(b(b(s)a)) & otherwise \end{cases}
\]

\[
b'(sa) = \begin{cases} b'(next(b(s))) & last(b'(next(b(s)))) = a\\
        b'(b'(next(b(s))a)) & otherwise \end{cases}
\]
In fact, one can see the failure function is precisely $b'$. What's not clear is how can one compute this function without keep track of the entire history of $b$.

The essential function is `build ys s`. It builds the automaton by consume the remaining string, and `s` records essential information to compute the transition of the failure function.

1. $value s = value t$, then $b(t) = b(s)$, and we would know $b'(next(t)) = next(s)$.
2. $value s \neq value t$, then $b(t) = s$, and we would compute $b'(next(t))$ by searching back through failure edges.

Note we have also computed $b'(next(t))$, which allow us to compute the next node $next(t)$.

So in `build ys s`, `s` will precisely store the state $b'(t)$.

```haskell
buildAutomaton :: Eq a => [a] -> Automaton a
buildAutomaton (x:xs) = automaton
  where automaton = Node x (build xs automaton) (Null automaton False) (null xs)
        build [] s = s
        build (x:xs) s
         | x == value s = success s `nextNode` failure s
         | otherwise    = newS      `nextNode` s
         where nextNode a b = Node x (build xs a) b (null xs)
               newS         = success $ until (\s->isNull s || x == value s) failure s
```