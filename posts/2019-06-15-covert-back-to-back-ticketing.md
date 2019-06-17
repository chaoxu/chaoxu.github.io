---
title: An algorithm for covert back-to-back ticketing
tags: Optimization, algorithm, airline
typora-root-url: ../
---


Airline booking ploys are ways to circumventing airlines ticket rules in order to spend less on the ticket. A fairly nice [Wikipedia article on airline booking ploys](https://en.wikipedia.org/wiki/Airline_booking_ploys). We consider a special case, the back-to-back ticketing/nested ticketing. [TripSavvy has a good article on back-to-back ticketing](https://www.tripsavvy.com/back-to-back-ticketing-468287). 

For many airlines, back-to-back ticketing is explicitly forbidden, this includes [American Airlines](https://www.aa.com/i18n/customer-service/support/conditions-of-carriage.jsp?anchorEvent=false&from=footer?#ticketvalidity), [Delta](https://www.delta.com/us/en/booking-information/fare-classes-and-tickets/ticket-rules-restrictions) and [United](https://www.united.com/ual/en/us/fly/contract-of-carriage.html).

We want to model back-to-back ticketing into an algorithmic problem. There is a sequence of trips between two locations, and it can be grouped into different round trips. Each round trip itinerary has a different cost. A round trip itinerary is no more than a matching between two trips. 

This can be seen as simple matching problem. The vertices are trips, which we can assume a trip means "traveling from A to B on date x". There is an edge between two trips, if there is a round trip itinerary containing the trips. The cost of the edge is the cost of buying a round trip for those two trips. Since it is also possible that we buy a one-way ticket, there are also self-loops. Also, we can consider a multigraph, where the edges are colored. Each color class represents an airline.

If one does not care about the airline finding out. Hence one can see this is a minimum weight perfect matching problem allowing self-loops. However, people might actually care about airline not happy about this practice. Hence we are interested in making sure in each airline, we have no overlapping itineraries. However, we allow overlapping itineraries across airlines. 

For two edges $\set{a,b}$ and $\set{c,d}$ defined over integers, it is _independent_ if $[a,b]\cap [c,d]= \emptyset$. A set of edges is _independent_ if the edges are pairwise independent.

{Problem}(Covert back-to-back ticketing problem)
    
    **Input:** A multigraph $G=(V,E)$ where $V\subseteq [n]$, where each edge can be one of $k$ colors, and there is an edge cost function $c:E\to \R^+$. 
    
    **Output:** A perfect matching $M$ (allowing self-loops), such that each color class of $M$ is independent, and the cost is minimized. 

I suspect for arbitrary $k$, the problem is NP-hard. We show how to solve the problem in polynomial time for $k=2$. 

[Yizhi Song](https://yizhis.github.io/) stated the idea that if one edge in a color is picked, it forces some edges of the other color to be picked. We can use the idea to obtain an $O(n^3)$ time algorithm for this problem.

Since we are only working with $k=2$ case, we will let $\bar{a}$ to be the color that is not $a$. Recall $[n]=\set{1,\ldots,n}$ and $[a..b] = \set{a,a+1,\ldots,b}$. We assume the vertices $V=[n]$. For a graph with $k$ color classes, we define $G_a$ to be the subgraph consists of all edges of color $a$. $c_a(x,y)$ is the cost of a color $a$ edge $xy$. We define $D(a,y,z)$ as the optimal solution when the input graph is $G_{\bar{a}}[[z-1]\setminus \set{y}] \cup G_a[[y-1]]$. We also define $C_{a}(x,y)$ to be the optimal solution when the input graph is $G_a[[x..y]]$.

The optimal solution is $D(a,n+1,n+1)$ for either color $a$.

We express the recursive relation. For $y<z$, we have the following.
$$
D(a,y,z) =\min \begin{cases}
\min_{x<y} \set{ C_{\bar{a}}(y+2,z-1) + c_{\bar{a}}(x,y+1) + D(\bar{a},x,y)}\\
C_{\bar{a}}(y+1,z-1) + D(\bar{a},y,y)
\end{cases}
$$
It might be beneficial to see the intuition behind the two cases through the following pictures.

![First case.](/files/ticketing_case1.png)

![Second case.](/files/ticketing_case2.png)

On the other hand, when $y=z$
$$
D(a,y,y) =\min_{x<y} \set{
D(a,x,y) + c_a(x,y),
D(\bar{a},x,y) + c_{\bar{a}}(x,y)}
$$
One can easily infer the base case through definition. Note that all values of $C_a$ can be computed in $O(n^2)$ time. It takes $O(n)$ time to compute one value in $D$. Therefore, the total running time is $O(n^3)$.


