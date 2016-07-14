---
title: Fill a checkerboard
tags: puzzle
---
This was a homework problem for CSE 548. It's also a very common puzzle. 

{Problem}
    Given a $n\times n$ checkerboard, one can put a few checkers on it. A rule transform a checkerboard to the next. The rule states one can put a new checker if and only if at least two of it's adjacent positions (up, down, left, right) has a checker. Apply the rule until no more checker can be added. Show that if originally the checkerboard has only $n-1$ checkers, then when the process terminates, there exist cells not filled by a checker.

{Proof}
    Consider any $n-1$ pieces that are already placed on the board. Consider the following process:

    1. Pick any piece that was never considered, so we have a filled rectangle(square) of $1\times 1$. 
    2. Find another piece that can form a larger filled rectangle from the previous rectangle.(so Manhattan distance of at most $2$ from the rectangle). This means one only regard the checkers in the rectangle and the new piece, and do the transformation until nothing more can be done. It must be form a new rectangle. If the previous rectangle is a $a\times b$ rectangle, the new rectangle is among one of the following sizes: $a\times b$(the new piece is inside the rectangle), $a\times (b+1), a\times (b+2), (a+1)\times (b+1)$ or $(a+2)\times b$. Note if the new rectangle is a $c\times d$ rectangle, $c+d\leq a+b+2$. Let the new rectangle be the one we are considering.
    3. If there is no other piece fit the description in step 2. Go to step 1.

    The process will in the end form $k$ rectangles, such that the sum of the perimeter is at most $4(n-1)$, because each time step $2$ is run, we increase the perimeter of one of the rectangle by at most 4, each time step $1$ is run, we create a rectangle of perimeter 4. We can run those step at most $n-1$ times because there are only $n-1$ checkers. 

    If any two of the rectangles can form a larger one, then the perimeter of the new rectangle is at most the sum of the two smaller one. To show this, one can consider two cases: If they overlap, then it's clearly true. If they do not overlap, there are only 2 distinct positions. Either their diagonal touch, or their diagonals doesn't touch. Try both and you will see this is true. (It's hard to draw things and post on a blog).

    This shows the largest filled rectangle can be produced from $n-1$ checkers have perimeter $4(n-1)$, but to fill the $n\times n$ board, one need $4n$ as a perimeter. Therefore no configuration of $n-1$ checkers can result a filled board.
    