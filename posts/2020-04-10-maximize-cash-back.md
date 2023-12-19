---
title: Maximize Cash Back
tags: math, life
---

Credit cards often offers rewards for purchases. 
For example, a card that gives 5% cash back for grocery stores. 
Also, credit cards often can be used to buy gift cards for specific stores. 

For example, one can use Chase Ink Cash (CIC) and go to Staples and buy gift card for Amazon for effectively 7.5% off Amazon (I value UR at 1.5 cents each).

However, there is usually a limit. CIC caps the reward spending to \$25000. Still, it would translate to \$1875 of savings. Anything afterwards only earns 1.5%.

I often do large purchases at some retailers, and I have huge number of credit cards. It make sense for me to optimize spending. I use historical data to estimate amount of spending at retailers, and come up with a nice optimization problem. 

There are $k$ stores, we want to purchase an item of value $d_i$ in store $i$, and want spend the least amount of money. 

Stores can also sell gift cards for specific stores (some gives cash back), and one can use the gift card to buy item from other stores. For simplicity, we assume gift card for a particular store can be used to buy gift cards (so it work just like cash). $g_{i,j}$ is the cash back one obtain for buying $1$ unit of gift card for store $j$ from store $i$.

We also have $n$ credit cards. Each credit card have an upper limit $u_i$. For credit card $i$, it gives cash back of $c_{i,j}$ per unit on store $j$ if total spend of the credit card is no larger than $u_i$, otherwise it give cash back $c'_{i,j}$.

This is a min-cost flow problem. Indeed, let's build the graph. Edges default to have infinite capacity and 0 cost. Node default to have 0 demand.

For credit card $i$, we create $3$ nodes, $a_i, b_i^+, b_i^-$. There is is edge $(a_i,b_i^+)$ with capacity $u_i$, there is an edge $(a_i,b_i^-)$ with infinite capacity. 
For each store, we create a node $s_i$, and there are edges $(s_i,s_j)$ with cost $-g_{i,j}$. The demand on $s_i$ is $d_i$.
For each credit card $i$ and store $j$, there is an edge $(b_i^+,s_i)$ with cost $-c_{i,j}$, and an edge $(b_i^-,s_i)$ with cost $-c'_{i,j}$. Finally add a source node connect to each $a_i$. Source node has demand $-\sum_{i} d_i$.

Some stores does not allow gift card to be used to purchase more gift cards. Indeed, this was not the case a long time ago, until people abused the price difference to generate infinite money.

This can be fixed by duplicate this store into a cash version, so $s_{c,i}$ and gift card version $s_{g,i}$. We redirect edges to the correct ones. Namely credit card goes to $s_{c,i}$ and edges from stores goes to $s_{g,i}$. Also, create edges $(s_{c,i},s_i)$ and $(s_{g,i},s_i)$.

We are only thinking about cash backs, but there could also be discounts. It is fairly rare to see discounts, especially for companies that sell other gift cards. 
The difference between cash back and discount is when you obtain your money back. Cash back you get your money back after the transaction. For discounts, you pay less.

Adding discounts *fundamentally* changes the problem. In the min-cost flow formulation, the cost keeps track of cash backs, and demand is keep track of how much money is spent. This does not work when there are discounts. 

Instead, we have to venture into generalized flow. There is an extra factor of $\gamma$ on each edge, such that the flow going in is $1$, then the flow going out is $\gamma$. 

Anyway, one thing I learned is using credit cards to buy gift cards from grocery store (Amex Gold) or staples (using Chase Ink Cash) are amazing. Netting you large discounts on many vendors you usually cannot get a huge discount for. 

Some more interesting things to add. Buying gift cards from grocery stores can obtain fuel points. Fuel points translate to discounts on gas. However, there is only so much gas one can use (unless you selling it to others). So one can view grocery stores as having a higher discount up to a certain limit. This can also be modeled in the graph. 