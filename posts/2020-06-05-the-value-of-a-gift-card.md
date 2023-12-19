---
title: The value of a gift card in beancount
tags: accounting 
---

I'm using beancount to do accounting. In beancount, you are allowed to have commodities. One can view gift card as commodity, but also as a currency.

Say, I bought an \$100 Amazon gift card for \$90, I would write `100 AMAZON {0.9 USD}`, which means 100 units of Amazon with unit cost \$0.9.
Later on, I used \$50 Amazon gift card, then I can write `50 AMAZON {0.9 USD} @ 1 USD`. Here `@ 1 USD` means the price is \$1, and `{0.9 USD}` tells the program to look for 50 unit of Amazon with unit cost \$0.9. 

Of course, often one do not care which amazon gift card was used. So beancount allows one to write `50 AMAZON {} @ 1 USD`, and beancount automatically finds 50 units of amazon gift card through some predefined rules (For example, first in first out). I sold the remaining \$50 Amazon gift card for \$47.5. So I would write `50 AMAZON {} @ 0.95 USD`.

During the event where the gift card is used, we can record the profit by subtract cost from price. So just like stocks, we can obtain realized profit and loses. 
This is great and all, except a simple operations is impossible in beancount. It is impossible to move these amazon gift card to another account without losing all the cost information. This actually happens in real life. I might buy amazon gift card, but load gift card to different accounts. In beancount, I have no way of relaying this info. 

Here is a hack. Having a proxy price.

The idea is amazon gift card should always have a value of \$x.
Whenever you buy amazon gift card for \$y per unit, you record a profit/loss of \$x - \$y.
Whenever you use your amazon gift card at value \$z per unit, you record a profit/loss of \$z-\$x.

Here you can set $x$ to be anything. But for simplifying calculation, 1 is sufficient.
Setting it to $1$ is the best option since it simplifies the computation.