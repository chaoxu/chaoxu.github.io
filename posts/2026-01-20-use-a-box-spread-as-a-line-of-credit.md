---
title: Use a box spread as a line of credit
tags: finance
---

In an idealized world, you could borrow at the Effective Federal Funds Rate (EFFR) and also earn roughly the same rate on cash deposits. For simplicity, assume the relevant short rate is fixed for a month. Let's say it turns out to be 10% *per month*. Assume the stock will rise by 20% that month.

In reality, lenders (and banks taking deposits) need to earn a spread, so borrowing and deposit rates won't match. But for the sake of argument, assume both are close to EFFR.

Suppose that over the next month, the maximum cumulative cash shortfall I might face is \$100. Then I can keep \$100 in the bank as a buffer. Maybe I earn \$20, spend \$50, then spend \$70, and then my next paycheck of \$100 arrives. My bank balance never goes negative, and perhaps my daily average balance is \$50. Roughly speaking, I earn interest on \$50 to be \$5.

Now suppose instead I start with only \$50 in the bank and invest the other \$50 elsewhere (say, in stocks). If I run short of cash, I can borrow quickly, transfer funds into my checking account, and later repay the loan. This is essentially what a line of credit (LOC) does: you draw when needed and pay interest only on the outstanding balance.

Another way to view it is a checking account that's allowed to go negative (an overdraft line), where negative balances incur interest.

If you can borrow at the same rate you can earn on cash, then holding \$50 less cash and borrowing \$50 when needed does not create free profit. In frictionless form, you're mostly exchanging:
 
 - less interest earned on cash (because you keep less cash),
 - for interest paid on borrowing (when you're short).

Those effects can be close to offsetting only when the borrowing and lending rates are the same and the timing matches. But in practice:

 - borrowing rates are usually higher than deposit yields,
 - timing matters (you don't borrow continuously; you borrow intermittently),
 - there are transaction costs, bid/ask spreads, and operational risk.

So what's left is not a clean cancellation; it's “equity exposure minus financing frictions.” But it seems, it should still amount almost everything cancelling out in the grand scheme of things.

However, in the U.S., bank interest is generally ordinary income. Meanwhile, interest you pay is not automatically deductible. This breaks the symmetry, and needs to be further studied.

A [box spread](https://en.wikipedia.org/wiki/Box_spread) is (economically) a synthetic loan created from options. When priced efficiently, the implied financing rate tends to track a risk-free(-ish) rate for that tenor, often close to EFFR. In that sense, a box spread can approximate “borrow at a near-risk-free rate for a fixed term.”

Now, a box spread is a fixed-term, fixed-principal loan. It is not a true revolving LOC, which is what we really want. You can't "draw \$x today, repay tomorrow, redraw later" flexibility.

However, we can simulate it as a LOC. Just put unused borrowed cash in a bank. 

So consider I start with \$100, and what happens in different scenarios.

1. I borrow \$100 and "return" it immediately. I gain \$10 of interest, and lose \$10 from execute the box spread. The \$100 I did not use was put in stock. I now have \$120 (with \$20 unrealized gain), and incurs a \$10 ordinary income and \$10 capital loss.
1. I borrow \$100 and used them all. I lose \$10 from execute the box spread. The \$100 I did not use was put in stock. I now have \$120 (with \$20 unrealized gains), and a \$10 capital loss.
2. I \$100 in the bank in the beginning, I get \$110 in the end, with tax-wise \$10 ordinary income.

Now this means if I do believe there is something with higher performance than EFFR, I should always borrow for my cash needs.