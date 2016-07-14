---
title: A generalization of the art gallery theorem with reflection and a cool problem
tags: computational geometry, discrete geometry
---

When I was the TA for AMS 345(Computational Geometry) last year, I have encountered problems where I don't know if there exist a answer. Therefore I used those problem as a toy example of what a "research" could be like.

First I will demonstrate a theorem that generalize art gallery theorem. It's the first interesting theorem I discovered. :) I'm sure someone else have found it before.

The definition of guard, visibility, etc. are defined in the wiki for the [art gallery problem](http://en.wikipedia.org/wiki/Art_gallery_problem).

One want to generalize the notion of guarding a polygon. Instead of walls, the edges become mirrors. The light loses intensity every time it get reflected on the mirror. Therefore after $k$ reflections, it become indistinguishable to a guard.

{Definition}($k$-reflection visible)

    Given polygon $P$. $p,q\in P$. $p$ is called *$k$-reflection visible* to $q$ if and only if there is a ray of light from $p$ to $q$, such that it reflects at most $k$ times on the boundary of the polygon. Each reflection follows the law of reflection. (angle of incidence = angle of reflection.)

{Definition}($k$-reflection guard)

    A *$k$-reflection guard* is a guard that can see all the points that are $k$-reflection visible from himself.

{Theorem}

    If $G_k(n)$ is the minimal number of $k$-reflection guard required to guard any polygon of $n$ vertices. Then $G_k(n)=\lfloor \frac{n}{3} \rfloor$.
    
{Proof}

    By the art gallery theorem, we know $G_0(n)\leq \lfloor \frac{n}{3} \rfloor$. $G_k(n)\leq G_j(n)$ if $j\leq k$. Since a guards can only become stronger when they can see more reflections.
    $G_k(n)\leq \lfloor \frac{n}{3} \rfloor$

    The lower bound can be proved with a Chvátal's comb with very thin teeth. A Chvátal's comb with 3 teeth is shown below.

    ![Chvátal's comb](/files/chvatals_comb.png)

    Since for each teeth, the result is symmetric. We only have to consider one teeth. Suppose we pick $p$ to be the teeth vertex. A ray can behave in 2 cases:
    Case 1: The ray escape the teeth after the first reflection, and bounce between the parallel lines for $k-1$ times. It's easy to see the furthest distance this ray can travel from the teeth is bounded by the angle of the teeth and the distance between the lines. One can always find a polygon, such that the distance between teeth is large enough, such that no visible region from case 1 can overlap.

    Case 2: The ray went into the teeth after the first reflection. One can construct a teeth such that rays will bounce inside the teeth for at least $k-1$ times. If $\alpha$ is the angle of the teeth, the amount of times the ray hit the teeth is at least $\frac{\pi}{2\alpha}$ times. Convince yourself this is true by reflect entire teeth along it's edge repeatedly. The ray has to hit at least all the reflections lies between a $\frac{\pi}{2}$ sector. One can make $\alpha$ small enough, so $\frac{\pi}{2\alpha}\geq k-1$. Thus all the rays in this case has to stay in the teeth, therefore it can't overlap with visible regions of other teeth.

    ![proof](/files/artgallery_proof.png)

    Each visible region is independent. There are $\lfloor \frac{n}{3} \rfloor$ visibility regions. This gives us the desired result
    $\lfloor \frac{n}{3} \rfloor \leq G_k(n)\leq  \lfloor \frac{n}{3} \rfloor$, $G_k(n) = \lfloor \frac{n}{3} \rfloor$.

Just for fun. Here is another toy problem from last year's AMS 345 homework.

{Problem}

    Let $P$ be a simple polygon with $n = 3k$ vertices, for a positive integer $k$. Starting with a vertex, color the vertices alternately around the polygon: red, blue, green, red, blue, green, etc.
    Find a counterexample to the following claim: There exist a monochromatic guard set.

Back then, the best known counterexample has 15 vertices($k=5$). Professor Mitchell asked if it was the smallest counterexample. I start to work on the following problem:

{Problem}

    Find the smallest counterexample, and prove it's the smallest.

A counterexample with $k=3$. The colored region are the area can't be seen by vertices of that color.

![counterexample](/files/cg_counterexample.png)

It is indeed the smallest possible.

{Lemma}

    Any 2 vertices on a quadrilateral can guard the quadrilateral.

{Proof}

    There are only 2 cases, draw them and convince yourself.

{Theorem}

    There exist no counterexample for $k=2$.

{Proof}

    Suppose there exist a polygon $P$ such that $k=2$ and it is a counterexample to the original conjecture. The vertices of $P$ are
    $RGBrgb$.

    Any triangulation of the polygon result 4 triangles. Since no color exist in all triangles(else that color guards $P$), there is a triangle
    composed of only 2 colors. Therefore one side of that triangle have the same colored end points. It must be a diagonal because no edge of
    $P$ have the same colored end points.

    wlog, let the diagonal be $Rr$. Then $P$ is partitioned to 2 quadrilaterals $RGBr$ and $rgbR$. Using the lemma, we see $R$ and $r$ can guard both quadrilaterals. It implies $R$ and $r$ guards $P$. A contradiction.