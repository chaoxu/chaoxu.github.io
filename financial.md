---
title: Financial Freedom Advices
---

This article are for US person who is
 - Single, living and working as a salaried employee with yearly pretax income more than \$200,000. If you make less, just ignore tax related discussion. 
 - No need to liquidate a large percent of investment in a short time range (for example, downpayment for a house in 5 years).
 - You want to do *passive investments*.
 - Not interested in beating the market, but match the market. This means if the market goes down, money goes down, and that is fine.
 - Wants to reach financial freedom earlier in life.

Here are just things I've learned throughout the years.
In order to reach financial freedom, which requires passive income to be more than your yearly spend, you can change one of the two factors. Increase income or decrease expense. Likely you get there faster by doing both.

You might still want to work even if you reach financial freedom. But at that point, you can decide what to work on. For example, I personally have [golden handcuffs](https://en.wikipedia.org/wiki/Golden_handcuffs), where I don't have freedom to just chose any job. I have to chose a job that pays comparable to my current one. 

# Increase Income

## Asset Allocation

You have to figure out your risk tolerance. Unfortunately, all one knows is having more risk means more reward, but exactly how much risk you are getting for each action is unclear. In the end, diversify avoids the most risk. I personally recommends Personal Capital [^personalcapital] for tracking the status of all your assets. 

### Liquid cash
Having a good checking account for *small* amount of cash. This allows one to handle lot of simple bill related stuff. Make sure it is sufficient to cover very short term living expenses. Get a checking account which satisfies most of your needs [^1]. You can shop around to see which one benefits you the most. Credit unions might offer more benefits over large banks. [Check doctor of credits to find ones with good rates and offers](https://www.doctorofcredit.com/bank-accounts/
). 

### Emergency Fund
Always have around $k$ months living expense in the emergency fund. The emergency fund should exists in a high yield checking/savings account [^savings] insured by FDIC or NCUA. For most people $k=3$ is sufficient. You might want larger $k$ to avoid risks. For example, you find that it is hard to find a job in $3$ months. However, at that point, you could liquidate some of the other portfolios.

### Investments

When I say portfolio, I excludes all liquid cash and emergency funds (since they should not be a huge part of your total asset).
Note that return = cash + beta + alpha. You want to invest passively, then you can only hope to capture beta.

#### Diversification over asset class

Diverse portfolio. If you truly don't care, not even thinking about setting up a simple allocations, then just use Wealthfront [^wealthfront] or [betterment](https://www.betterment.com/). They do automatic investment techniques knowing your risk.

However if you trust yourself and want to save that little fees they charge. The easiest is a 3 fund portfolio. Total US stock (or S&P 500), Total international stock and Total US bond. Any low fee fund is sufficient. For example, a 80% stock, 20% bond portfolio using vanguard ETFs:

- 60% S&P 500 (VOO)      
- 20% International Stock (VXUS)
- 20% US Bonds (BND)

Because of market fluctuation, the relative proportions change after a while. Therefore one have to do some rebalancing.
If you set the relative fractions correctly, but doesn't want to do manual rebalancing, then you can set the percentage and use M1 Finance[^m1finance] to automatically rebalancing.

If you are into manually balancing, for the above portfolio, you can use Vanguard. You can also use the mutual funds version instead of the ETF version. 

One can also rebalance using [risk parity](https://www.investopedia.com/terms/r/risk-parity.asp). I do not know any product where you pick your funds and it does automated risk parity rebalancing. This you have to do yourself.

#### Diversification over time

Invest part of the salary whenever you obtain salary. Instead of buying in a lot at the same time. This allows you to diversify overtime. 

In order to diversify over time, you should have equal allocation each year. But your current salary might be *much smaller* than how much contribution you have in 10 years. Therefore you should use leverage if possible. One will quickly see the only leverage an average salaried employee is real estate loans, therefore it make sense to spend on real estate. 

Sometimes you obtain a large amount of money at one time, like bonuses. You can still try to invest it overtime, say in range of a year. If you know the amount of bonuses you will obtain over time, you can also invest a bit more of your each salary. However it might cause cash flow issues if your bonus goes below your expectation. 

#### Company Stocks
For stock options, unless you have strong belief that this stock is much better than your other investments. It should be sold on the day you obtain it. Don't give into [the endowment effect](https://en.wikipedia.org/wiki/Endowment_effect). However, if you do have lot of trust in your company, you should not sell all the stocks. Since if you are working at that company, you probably can't buy stock due to trading windows.

#### Experimental investments
You might see a lot of investment ideas floating around. If you are interested in them, first try [Portfolio Visualizer](https://www.portfoliovisualizer.com/) to visualize and backtest. Try different parameters, and understand the theory behind the portfolio. It might let you retire a few years early. It is definitely fine to invest small incremental amounts. 

##### Leveraged ETF

There is something called leveraged ETF. A $k\times$ ETF tracks some index. If the index changes by $x%$, then the leveraged ETF changes by $kx%$.

|Leverage| S&P 500     | 20+ treasury|
| ------ | ----------- | ----------- |
| 1x     | VOO         | TLT         |
| 2x     | SSO         | UBT         |
| 3x     | UPRO        | TMF         |

Can we just multiply it's return by $3$ using leveraged ETFs?

The answer is no. It is definitely true for a single day. But not true in the long term. Note a drop of $x$ fraction, then it needs to gain a fraction of $\frac{x}{1-x}$ to obtain the original return. Hence one can see that even if VOO had return of 0, UPRO can have negative returns. In the end volatility is what dominates. To understand more, read [inverse and leveraged ETFs](https://www.bogleheads.org/wiki/Inverse_and_leveraged_ETFs).

However, there are claims that one can [mix UPRO and TMF](https://www.bogleheads.org/forum/viewtopic.php?f=10&t=272007) to obtain amazing results. One could definitely experiment with them. In fact I also experimented with it. 

#### Brokers 
Vanguard is a great broker because almost everything is free. However, their support is only satisfactory. 
Note if you just use Vanguard setup, you *cannot* use leveraged ETF, because they [explicitly rejects them](https://investor.vanguard.com/investing/leveraged-inverse-etf-etn).
I personally use M1 Finance [^m1finance] to use the leveraged ETFs.

### My setup

My current setup
 - An experimental account of around 15% of my money, implementing 60% TMF and 40% UPRO
 - The remaining investments is almost all in VOO, and I'm trying to rebalance it to have more bonds. 

My goal setup would be:

 - 70% VOO + TLT using monthly rebalancing through risk parity
 - 20% UPRO + TMF using monthly rebalancing through risk parity
 - 10% Foreign stock


### Real Estate

Home loans are one of the cheapest loans, which gives you leverage. You should prioritize that if it make sense.
Especially there are huge amount of tax advantages (if the house is expensive enough when itemized deduction make sense). It makes even more sense if you live in the house.

## Asset Location (Taxes)

You can't control market fluctuation, but you can (almost) control taxes. Put your money in places that is tax efficient, so you can grow better. To understand asset location, [read the wiki here](https://www.bogleheads.org/wiki/Tax-efficient_fund_placement).

The idea is most US stocks, the dividends are likely to be qualified dividends, so it is quite tax efficient compared to bonds, which is interest and taxed at the personal income rate. 
International stocks requires you to pay international tax. It can be deducted if it is not in a tax deferred or tax exempt account. 

The idea is putting tax inefficient items into tax efficient accounts and put tax efficient investments into taxable accounts. 

A good rule of thumb
 - investments with high growth should be in tax exempt, then tax deferred, until you run out of such things.
 - investments generating dividends put into tax efficient accounts. 

Also note that [HSA can act as a retirement account](https://www.madfientist.com/ultimate-retirement-account/) if you can tap into it. 

### Maximization of tax benefits and free money

For a simple example:
 - Salary: 160k
 - Bonus: 15% expected
 - Stock: vested value around 50k, each year
 - 401k match: 100% match up to 6% of salary
 - Your company allows after tax 401k contribution which allows rollovers (It's not the same as Roth 401k!)
 - Lives in Seattle

Do the following in sequence until you are out of money. 

 - Maximize 401k until employer match (\$19000 + \$9600=\$28600)
 - Maximize HSA (\$3500)
 - Maximize Commuter Benefits (\$3180)
 - Maximize 401k (no change)
 - Maximize after tax 401k, IRA and [roll over to Roth IRA](https://www.forbes.com/sites/ashleaebeling/2012/01/23/the-backdoor-roth-ira-advanced-version/#f2f12d355ada) (\$56000 - \$28600 + \$6000 = \$33000)
 
If you go extreme, \$62000 per year will be saved in retirement account, where \$33000 can be taken out in 5 years, the rest needs to stay till retirement age. \$3500 for medical expenses only, \$3180 for commuting only and *has to be spent* within the year. 
In total, \$19000+\$3500+\$3180=\$25680 are from your own pre-tax money. \$33000 are after-tax money.

The [income tax calculator](https://smartasset.com/taxes/income-taxes) shows you prob have \$150000 after tax. Hence you retain around \$117000 (this is assuming stock does not go up and down and you liquidate it it as soon as it vests). Plenty of money for after-tax investments. 

## Negotiate and compare

There are many things can be negotiated. For example, your salary. Your worth is how much the company will likely to pay you, always negotiate salaries. Ask for a raise.
It's not just for yourself, your family, your loved ones. It is for their good too. If they give your salary below the market rate, you might be scooped by other companies. In the end you leave, and both side loses. 
So try to negotiate for better offers for every major financial transaction.
This even includes medical ones. Where you can go around ask for second opinions to save yourself money. So always shop around. 

# Decrease spending

Avoid consumerism. Make sure to budget, and spend according to the budget. Track your spendings. Look for good deals when you have to buy something. Join loyalty programs if it is something you buy more than once.

## Credit Cards
Never use debit cards when you can use credit cards. Make sure you only use as much as you can pay back. Build good credits. Shop around for good credit cards [^2]. Auto-pay all your cards and always pay full balance. 

### My setup

My wallet consists of the following credit cards since I'm with Chase

- The credit card where I need to meet spending requirement.
- Chase Sapphire Reserve for dining and travel.
- Chase Freedom Unlimited[^chasefreedomunlimited] for everything else.

## Cash Back Portal
Most of the things you buy probably can use a cash back portal for some of the money. Some of these portals are associated with your credit card or your airline. There are many, like ebates(now rakuten), is one of the most popular ones.

### My setup

- Whenever I'm about to buy something, I check [Cashback Monitor](https://www.cashbackmonitor.com/) to see if there is any rewards. I had a good experience with Rakuten [^rakuten].
- I usually take the portal with my credit card or airline, because they are most trust worthy, unless other sites have way too good a rebate.

## Points and miles
There are many reward programs with lot of points and miles (will be refereed commonly as points from now on). Here are some things to remember
1. *Points devalues, therefore use them quickly*. Accumulating lot of points doesn't mean much once devaluation happen. Even ultimate reward points that translate to statement credit devalues due to inflation. 
2. *Value points rationally*. Exactly how much *you* would accept to pay in cash is the value of the points. Those online valuations for flights usually make sense because most of the ticket prices are going to be high. However for hotels, it does not. Are you really getting \$200 value when you spend the points on a hotel? For example, if there is a \$100 hotel just around the corner and you would pick that \$100 hotel if you didn't have the points (and say you can spend \$100 to buy the points). This implies your points worth less than \$100.

## Travel
Credit card bonuses with points are usually sufficient if you do not travel often. Try to take advantage of it. Get familiar with the few common routes. Use [AwardHacker](https://www.awardhacker.com/) to find flights. More experienced user need to look into specific programs.

Airline programs are more important than hotel programs. There are always cheap hotels. However there are only so many airlines. So if you have to focus, focus on airline program over hotel programs.

It is usually a huge time sink to learn too many airline programs. You just have to be aware of what is possible most of the time. There are many hacks possible for flying. If you are interested starting with [flyertalk](https://www.flyertalk.com/).

Plan travel ahead if possible.

### My setup

 - Have 1 main airline (Delta) and 1 alternate airline (Alaska).
 - Obtain points on other airlines when chances come (say, credit card opening bonuses). 
 - I would go for business class if I have accumulated too much points and I'm traveling internationally. Otherwise I go with economy. 
 - Burn the points at chain hotels like they are not real money. 
 
## Restaurants

Eat out less. If you do eat out, you can earn a lot of money back from restaurants. One can read an article on how to [save on restaurant](https://thepointsguy.com/2015/01/quadruple-dip-with-points-miles-discounts-at-restaurants/).


### My setup

1. Use Seated or opentable to reserve. Seated[^seated] they give out huge rewards (in terms of gift cards) in some large cities. [Opentable](https://www.opentable.com/) also give rewards.
2. I am with [Alaska dining rewards](https://mileageplan.rewardsnetwork.com) because their miles worth the most and I do located in Seattle.
3. If I have decided on a restaurant, I check Groupon[^groupon] or Restaurant.com[^restaurant] to see if I can get a *certificates*. You must be very careful to understand what exactly it entails. 

## Time

All the strategies cost time. Try to automate as much as possible. You should assign value to your time, and decide if certain operations are worth it. If you value your free time at \$50 per hour (after tax), then it make little sense to spend an hour to obtain \$40 discount.




# Risk

Note everything you do have certain amount of risk. Generally, consider every activity and its risk to you. Almost any risk of personal harm will lead to financial risk. 

Here I outline a few.

 - Risky hobbies: The kind of hobbies where personal harm could happen. Some tamer ones are rock climbing, skiing, etc. A misstep could put you out of commission for a few weeks. In the worst case, death and become permanently disabled. 
 - Marriage: On one hand, getting married could help with taxes, but for both people high income earners, likely they will hit a [marriage penalty](https://en.wikipedia.org/wiki/Marriage_penalty). 
 - Divorce: has a huge financial risk to the higher income/low spender due to common property states and alimony. If there are children involved, there are also child support.
 - Children: children is well known to be a black hole that sucks out all your money because you want the best for them.
 - House burnt down: likely, if you are young, the house is the largest asset you own.

I'm not suggesting people should stop doing the things above. People need to understand the risk before making decisions. Indeed you two might be in love when getting married. However, a divorce would affect your future marriage since you have a few year set back and still paying alimony. These money could be used for your new family and your children from the previous marriage.

## Insurance

Buy insurances that make sense. On average, insurance company is making money. So you should only get insurances for very costly items, the kind that happens will cripple your entire finance or set you back by a few years. 

## Use professionals

You can't do everything yourself, even if it is cheaper.
Use a lawyer when it make sense. For example, dealing with immigration. I have personally bitten by not getting a lawyer for doing immigration stuff. Use a tax professional when your tax situation is more complex than what Turbotax [^turbotax] can handle.





[^chasefreedomunlimited]: [Use this referral link for chase freedom unlimited](https://www.referyourchasecard.com/18/KRE9M4JY2C). Earn 3% cash back on all purchases in your first year up to $20,000 spent. After that, earn 1.5% cash back on all purchases.
[^turbotax]: [Use this referral link](http://fbuy.me/nwyLa) to get up to 20% off.
[^seated]: [Use this referral link](https://seated.app.link/j3FZwlVB1Y) and the code `CHAO19` to obtain extra \$5 dollars on your first reservation.
[^groupon]: Please [use this referral link](https://www.groupon.com/visitor_referral/h/7a8e66c7-d3fa-467d-88c0-cb2fa0aa6384). I get \$10 and you don't get anything though.
[^restaurant]: Please [use this referral](https://www.restaurant.com/referfriends/ReferredBy?refextid=f43f5fa8&prti=5157&ext=em_raf). I get \$10 gift card, you don't get anything though.
[^rakuten]: Please [use this referral](https://www.rakuten.com/r/MGCCLX?eeid=28187) and get extra \$10.

[^1]: [SoFi Money](https://www.sofi.com/share/money/2627476/) is a checking account with a very high APR for a checking account. Use my referral link and we both get \$50 after deposit of \$100. (You have to be a US person!) However, it is a Internet bank. If you really need physical services, I was using [chase total checking](https://accounts.chase.com/raf/share/2297938276).
[^2]: I personally use [US credit card guide](https://www.uscreditcardguide.com) to shop for credit cards.
[^savings]: You probably want to look at whatever is offering the highest APR. Currently [Wealthfront](https://wlth.fr/2hp96Gw) has a pretty good offer.
[^mint]: If you are like me who only use credit cards, and there are only a few non-credit card purchase. Just link [Mint](https://mint.com) to credit cards and do some tracking. Track your checking account seperately. This is because Mint often can't figure out transfers, and end up saying my budget is crazy high.
[^chasefreedom]: [Chase Freedom](https://www.referyourchasecard.com/2a/XDCUJDEKPJ) is fairly nice for its 5% categories. However, I generally never use them to their fullest.  
[^m1finance]: Use my referral for [M1 Finance](https://mbsy.co/zBs8G) and get a free \$10.
[^personalcapital]: Use my referral for [Personal Capital](https://share.personalcapital.com/x/62x35X) to earn \$20 dollars. 
[^wealthfront]: [Wealthfront](https://wlth.fr/2hp96Gw)