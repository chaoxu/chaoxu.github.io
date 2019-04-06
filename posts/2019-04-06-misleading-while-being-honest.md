---
title: Misleading while being honest
tags: data visualization
---
Given a set of data $A$, we can plot it on a [radar chart](https://en.wikipedia.org/wiki/Radar_chart). One can permute the axis to make sure the area of the radar chart is maximized. This was explored in [a previous article](https://chaoxuprime.com/posts/2012-08-08-maximize-the-area-of-a-radar-chart.html). 

More interesting problem. Given two sets of data $A$ and $B$, we are interested in finding a common radar chart, that make $A$ look as good as possible compared to $B$. We might want to optimize the area ratio, area difference, or something else. Again, we are thinking of permuting the axis of the radar chart. 

I once mentioned this problem to [R Ravi](https://www.contrib.andrew.cmu.edu/~ravi/), and he suggest I could ask the same question for all kind of different graphs. How to mislead people with graphs while being completely honest? Indeed, this looks like a fun research project. There is a [wikipedia article completely devoted to it](https://en.wikipedia.org/wiki/Misleading_graph). In my [previous post](https://chaoxuprime.com/posts/2019-03-28-l1-linear-regression.html), I've discussed how to fitting two seemingly not that related data points through simple transformation. 

I'm interested in are algorithmic problems where one want to compute the most misleading chart, I think it would be a great fun project. 