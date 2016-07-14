---
title: Selection in a sorted matrix
---

A matrix is sorted if every row and column are non-increasing. 

A common interview problem asks for the $k$th smallest number in a sorted matrix, and usually people give an $O(k\log k)$ algorithm. Wlog we can assume all the numbers in the matrix are distinct, as we can always break the ties by factoring in the position of the number in the matrix.

There is a $O(k)$ time solution. In fact, $O(\min(k, m))$ if it's a $n\times m$ matrix and $n\leq m$.

I'm frustrated that there isn't a good description of such an algorithm. The most common reference people provide is [@Mirzaian198513]. However there are a few downsides of that paper. It only works when $n=m$ and it is still a bit complicated. So here I will give a short description to a modified version of the algorithm.

One can see the idea closely resemble what happens in fractional cascading, we basically squeeze the unknown values between known values, so we don't have to look at most of the unknown values.

First, we assume the matrix we care about is a $n\times m$ matrix $A$. Both $n$ and $m$ are power of $2$ and $n\leq m$. 

Let $A_o$ be the matrix we get by removing all even index columns from $A$, and add the last column.

In [@Mirzaian198513], they used $A_{o,o}$, which is defined as only retain positions with odd coordinates, but I found it nicer to consider things by only stripping one coordinate. In particular, it gave a much nicer proof for the following result. 


{Definition}

    $r(a,A)$ is the number of elements in matrix $A$ smaller or equal to $a$.

{Theorem}
    
    Let $A$ be a sorted $n\times m$ matrix, then $2(r(a,A_o) - n) \leq r(a,A)\leq 2r(a,A_o)$.

{Proof}
    
    For any fixed $i$, let $f(i)$ be the largest $j$, such that $A_{i,j}\geq a$. $r(a,A)=\sum_{i=1}^n f(i)$, $r(a,A_o)=\sum_{i=1}^n \lceil f(i)/2 \rceil \leq r(a,A)/2 +n$. On the other hand $\sum_{i=1}^n f(i)/2 \leq r(a,A_o)$. 

This means if we want to find an element of rank $k$ in $A$, we can first find element of rank $k/2+n$ and $k/2$ in $A_o$, and we know the solution would be in between. The remaining operation takes $O(m)$ time:

 1. Use a flood fill starting from the position of $k/2$th element in $A_o$ and find at most $2n$ positions where the element of rank $k$ could reside. Namely it select elements in $A$ that is in between the $k/2$th element and $k/2+n$th in $A_o$. We can do this because all these elements are connected in a component.
 2. While doing the flood fill, it can also find the rank of the $k/2$th element in $A_o$ in the matrix $A$ for no extra cost.
 3. A linear time selection algorithm on all the elements resulted from the flood fill.

This would give us a recursive algorithm if instead of just finding $k$th number, it finds the $k_1$th and $k_2$th number at the same time. As long as $|k_1-k_2|=O(m)$, we can find them both only with a $O(m)$ extra time. $k_1$ and $k_2$ will be the upper and lower bounds respectively. Some basic algebra shows we will $|k_1-k_2|=O(m)$ inside each recursive step if we start with $k_1=k_2=k$.

Let $T(n,m)$ be the time used when the matrix is of size $n\times m$, and $n\leq m$. Certainly $T(1,m)=1$, and the rest follow the recursive relation $T(n,m) = cm + T(n,m/2)$ for some constant $c$.

If $n>m$, we can rotate the matrix implicitly to get the same running time. 

Solving it gives us the desired running time $O(m)$. This is also a $O(k)$ time algorithm because we only need to consider the $k\times k$ submatrix in the up-left position.

A Haskell implementation here, it requires a linear time rank selection algorithm `selectRank`.

```haskell
import Data.List
import Control.Applicative
import Control.Arrow
import Control.Monad
import RankSelection

type Matrix a = (Int->Int->a, Int, Int)

-- The input is an matrix sorted in both row and column order
-- This selects the kth smallest element. (0th is the smallest)
selectMatrixRank :: Ord a => Int -> Matrix a -> a
selectMatrixRank k (f,n,m)
 | k >= n*m || k < 0 = error "rank doesn't exist"
 | otherwise         = fst $ fst $ biselect k k (f', min n (k+1), min m (k+1))
 where f' x y= (f x y, (x, y))

biselect :: Ord a => Int -> Int -> Matrix a -> (a,a)
biselect lb ub (f',n',m') = join (***) (selectRank values) (lb-ra, ub-ra)
 where mat@(f,n,m)
        | n' > m'   = (flip f', m', n')
        | otherwise = (f', n', m')
       (a, b)
        | n < 3     = (f 0 0, f (n-1) (m-1))
        | otherwise = biselect lb' ub' halfMat
       (lb', ub')   = (lb `div` 2, min ((ub `div` 2) + n) (n * hm - 1))
       (ra, values) = (rankInMatrix mat a, selectRange mat a b)
       halfMat
        | even m = (\x y->f x (if y < hm - 1 then 2 * y else 2 * y - 1), n, hm)
        | odd  m = (\x y->f x (2*y), n, hm)
       hm = m `div` 2 + 1

-- the rank of an element in the matrix
rankInMatrix :: Ord a => Matrix a -> a -> Int
rankInMatrix mat a = sum (map (\(_,y)->1+y) $ frontier mat a)-1

-- select all elements x in the matrix such that a <= x <= b 
selectRange :: Ord a => Matrix a -> a -> a -> [a]
selectRange mat@(f,_,_) a b = concatMap search (frontier mat b)
 where search (x,y) = takeWhile (>=a) $ map (f x) [y,y-1..0]

frontier :: Ord a => Matrix a -> a -> [(Int,Int)]
frontier (f,n,m) b = step 0 (m-1)
 where step i j 
        | i > n-1 || j < 0 = []
        | f i j <= b       = (i,j):step (i+1) j
        | otherwise        = step i (j-1)
```

