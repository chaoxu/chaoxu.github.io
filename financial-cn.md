---
title: 财务自由的建议
---

文章是给

 - 美国的单身年轻拿薪水的人群, 以及现在的tax bracket比退休的时候要高.
 - 不需要很多流动性资产 (比如不准备很快买房)
 - 想要被动收入
 - 不期望跑过市场. 就是市场跌了自己跌, 心态也能正常.
 - 想要早点经济自由

这些是最近几年学到的东西. 要达到经济自由就要开源节流. 达到自己的被动收入超过支出. 
就算达到了经济自由也是可以继续工作的, 但是就不需要只是为了钱工作. 比如我现在就有金手铐, 我不能跑去换一个比现在工资低很多的工作. 

# 开源

## 资产分配

找到自己的风险容忍度. 了解自己的资产分布. 可以用Personal Capital [^personalcapital]这样的app连接自己所有的资产, 可以自动的了解现在的资产分配状况. 

### 现金
弄一个好的checking account [^checking]. 上面有*小额存款*. 可以用来解决每个月的费用. 多看点银行找到比较好用的. 我会用[Doctor of Credit](https://www.doctorofcredit.com/bank-accounts/)了解现在市面上的offer. 

### 应急资金
大概储存个$k$个月的应急资金. 对应大的波动, 最常见的是丢了工作. 这个应该存在一个高利率的被FDIC或者NCUA保护的账户里[^savings]. 对大多数人来说$k=3$就好了. 但是根据各种因素, 比如觉得自己$3$个月找不到工作, 可以适当提高$k$. 但那个时候应该有时间把投资的钱撤出来一点.

### 投资

我接下来说的投资, 是去除了所有现金和应急资金的. return = cash + beta + alpha. 你想被动投资, 就只能获得beta. 推荐阅读的书籍: [The Bogleheads' Guide to Investing](https://www.amazon.com/gp/product/1118921283/ref=as_li_qf_asin_il_tl?ie=UTF8&tag=chaoxu-20&creative=9325&linkCode=as2&creativeASIN=1118921283&linkId=2ae64a2b8e5c0c4a5742210aff4a2494). 是我看的被动投资方面最好的书了.

#### 分散投资-资产类别

你的投资组合应该是分散的. 如果你啥都不想管, 可以考虑直接用Wealthfront[^wealthfront]或者[betterment](https://www.betterment.com/). 他们自动做投资分配.
但是如果你稍微相信点自己和想要省一些管理费. 一个可能是简单的3基金组合. 比如全美股票(或者标普500), 世界股票, 全美债券. 这里面选低费用的版本. 比如80%股票, 20%债券用Vanguard ETFs:

- 60% S&P 500 (VOO)      
- 20% International Stock (VXUS)
- 20% US Bonds (BND)

当然, 还有各种不同的组合, 这个可以自己去发现.

市场波动需要定期重新平衡. 你如果已经算出了想要的资产分配百分比但是懒得自己做重新平衡, 可以用M1 Finance[^m1finance], 它自动重新平衡.
不然的话, 对于上面的组合可以直接用Vanguard. 

资产分配的另一种方法是根据[风险平价组合](https://www.investopedia.com/terms/r/risk-parity.asp). 我不知道哪里有自动风险平价组合的产品, 只能利用自己来重新平衡.

#### 分散投资-时间

为了保证时间上投资也是分散的, 你应该将薪水的一部分自动的定投. 

当然, 真正的分散定投的话, 你应该在你一生的每一年投出去一样的钱. 但是你现在的收入可能是你以后的很少一部分. 这就是为什么可以考虑使用一定量的杠杆. 暂时对于年轻人来说最常见的刚刚就是房贷. 
用多了杠杆就不太容易是passive investment了. 有一种是加了杠杆ETF(不是真的杠杆), 一般只是当天买卖用的(只有当天买卖才和真的杠杆一模一样). 后面会提到了如何长期持有.

有时你一次性获得一大笔钱, 比如奖金, 你也可以invest over time, 比如一年的时间慢慢定投. 但是如果你知道未来奖金会拿到多少, 不如提前考虑到这个, 增加你每个月的投资, 平均下来. 当然这有一定的风险, 就是你没有拿到那么多奖金, 可能会摧毁你的现金流. 

#### 公司股票
公司有时会发股票给你. 你应该做的是当场卖掉你不想持有的股票.
意思就是, 如果公司给你现金, 你愿意把其中多少换成你公司的股票. 那是你应该留下来的股票.
公司给了股票就会一直持有, 但是给了现金却不会买公司股票是[禀赋效应](https://en.wikipedia.org/wiki/Endowment_effect), 要破除.

#### 实验性投资
如果你了解到一个新的投资组合, 你可以考虑用[Portfolio Visualizer](https://www.portfoliovisualizer.com/)来回测和直观化究竟怎么样. 改改里面的parameter, 理解这个投资组合的背后理论. 放少量的钱进去, 可能可以让你早几年退休, 也可能晚几年. 

##### 杠杆ETF

$k\times$杠杆ETF如果跟踪一个指标. 则指标改变了$x%$, ETF改变$kx%$.

|杠杆        | 标普 500     | 20+ 年国债   |
| --------- | ----------- | ----------- |
| $1\times$ | VOO         | TLT         |
| $2\times$ | SSO         | UBT         |
| $3\times$ | UPRO        | TMF         |

注意$3$倍杠杆ETF和你用$3$倍的钱去投资结果并不一样. 

如果只看当天的结果, 似乎是一样的(我上面提到的几个都是每天reset). 但长期是不同的. 因为跌了$x$ fraction, 则要涨$\frac{x}{1-x}$ fraction才能得到原来的结果. 所以VOO一段时间波动直到最后获得了持平, 但UPRO可以是负的. 波动才是最可怕的. 要了解更多可以看[inverse and leveraged ETFs](https://www.bogleheads.org/wiki/Inverse_and_leveraged_ETFs).

有文章表示可以长期持有杠杆ETF [UPRO和TMF的组合](https://www.bogleheads.org/forum/viewtopic.php?f=10&t=272007)来获得很有意思的结果. 前面是原讨论链接, 但是可以看看physixfan的一系列文章. [1](https://www.physixfan.com/40-upro-60-tmf/), [2](https://www.physixfan.com/risk-parity-touziceluegaijinbandongtaidiaozhenguprohetmfdebili/), [3](https://www.physixfan.com/gupiaohechangqizhaiquandefuxiangguanxingdaodishifoukeyichixu/), [4](https://www.physixfan.com/risk-parity-touzicelue-difengxianbanben/).

#### 经纪商
Vanguard是个不错的经纪商. 和Vanguard有关的产品几乎啥都是免费或者价格低廉的. 但他们的support没有很好.
Vanguard是[*不能用*杠杆ETF的](https://investor.vanguard.com/investing/leveraged-inverse-etf-etn). 你可以考虑用其他的经纪商做. 比如我现在用M1 Finance [^m1finance].

### 我的配置

现在的配置

 - 15%的钱在 60% TMF, 40% UPRO.
 - 其他的几乎都在VOO

期望配置

 - 70% VOO + TLT using monthly rebalancing through risk parity
 - 20% UPRO + TMF using monthly rebalancing through risk parity
 - 10% Foreign stock

### 房产

房贷是最便宜的贷款了. 这是一个很好的杠杆. 而且有很多税上的优惠. 很适合开始做房地产business或者投资. 而且后期可以用这种杠杆获得越来越多的房产. 推荐个[最基本的对于房产投资有所了解的文](https://www.physixfan.com/fangchantouzixuexibiji/). 但具体怎么房产投资, 我也是在学习之中, 无法给出意见. 

## 税务

你不能控制市场, 但是你可以控制税. 作为美国用户可以[读读这个文章](https://www.bogleheads.org/wiki/Tax-efficient_fund_placement)了解基本的信息. 这整个部分假设你的income比较高, 比如\$100,000以上. 

有两个主要的税率. 一个是ordinary income tax. 一个是(long term) capital gain tax. 
当我们说税率的时候, 我们说的是effective ordinary income tax rate. 就是tax/income. 而不是marginal tax rate.
Capital gain tax rate一般低于effective ordinary income tax rate.
有的投资产生的税务比较重, 有的产生的少. 那么应该把税务重的投资放在免税或者延税账户.

 - 特别大增值的投资放在免税账户.
 - 特别大收入的投资放在免税或者延税账户. 

### 几个不同的退休账户的偏序

讨论一下3种收税的方式.

 - 放入: 是放入税前(0)还是税后(1)的收入
 - 盈利: 资产的盈利不收税(0)还是收税(1). 这里资产的盈利包括股票的分红和存款/债券的利息. 以及买卖的时候产生的盈利.
 - 取出: 钱取出来的时候是不收税(0)还是收税(1). 

所以有几种不同的账户. 
任何盈利不收税但是取出收税的都叫做tax-deferred.
盈利和取出都不收税的叫做tax-free.
所有的退休账户都会有一定的取钱要求, 如果不满足, 则会有惩罚.

*假设:* 取出的时候永远满足没有罚款的取钱要求.

以下是类别的定义. 因为篇幅问题, 所以我们只考虑最常见的几种账户. 其他账户也存在, 但普通满足文章条件的年轻人是不需要考虑的.

|类   | 放入       | 盈利        | 取出         |
|--- | --------- | ----------- | ----------- |
| A  | 0         | 0           | 0          |
| B  | 0         | 0           | 1           |
| C  | 1         | 0           | 0           |
| D  | 1         | 0           | 1           |
| E  | 1         | 1           | 1           |


- A类: 只有[HSA](https://www.madfientist.com/ultimate-retirement-account/). 取出的钱必须用于医疗. 取出来用于其他的话就需要收税.
- B类: Pre-tax 401(k), 低收入人群的Traditional IRA. 注意, 高收入人群不享有Traditional IRA的放入税前收入的属性. A类最多能放\$19000. 
- C类: Roth IRA, Roth 401(k). 这种账户只能放入税后收入, 但是提取的时候不用交税. 这类型账号5年之后本金可以拿出. 
- D类: After-tax 401(k), 高收入人群的Traditional IRA. 高收入人群放钱进入Traditional IRA都是税后收入. 取出是ordinary income而不是capital gain. 所有的D类都能转换为C类, 所以不再考虑D类. 
- E类: 普通账号. 任何时候都要交税. 但是好处是你可以随意的用它的钱.

如果盈利是正的, 那么任何类别都比E好. 如果盈利是负的, 那么只有E类才能利用负盈利获得tax-credit. 

*假设*: 没有负盈利.

可以看出在这些假设上$A>C$, $A>B$, $C>E$, $B>E$.

*假设*: 现在的税率比取钱出来的税率高.

这个假设还能获得$B>C$. 因为假如$x$是现在税率, $x'$是取出的税率. 则有1的收入, 放入$B$, 得到$k$倍收入, 取出来就是$(1-x')k$. 放入$C$类的话, 进去就是$1-x$, 取出来是$(1-x)k$.

这样我们就有完美的$A>B>C>E$.

我们来做个特殊的例子. 假如我们现在税率为x, 每年资本利得税率为y. 取出年税率为x', 取出年资本利得税率为y'. 
现在用1美元income买了一个股票, 每年涨的比例为$\alpha$, 然后每年分红比例是$\beta$, 所有分红都继续买那个股票. $k$年之后全部卖出.

我们让$X_0$为初始放入资金, $X_n$为第$n$年的资金, $Y$为最后卖出去到手资金. 则对于账户$(a,b,c)$, 我们有

- $X_0 = (1-ax)1$
- $X_n = (1+\alpha)X_{n-1} + \beta(1-by)X_{n-1} = (1+\alpha+\beta-\beta by)X_{n-1} = (1+\alpha+\beta-\beta by)^n X_0$

注意pre-tax和after-tax contribution的一大区别, 最后算税的时候按照income tax还是capital gain tax来计算.

- pre-tax: $Y = (1-cx')X_k = (1-cx')(1+\alpha+\beta-\beta by)^k X_0$.
- after-tax: $Y = (1-cy')(X_k-X_0) + X_0 = (1-cy')X_k - cy'X_0$

合在一起就是$Y = (1-a)(1-cx')X_k + a((1-cy')X_k - cy'X_0)$.

假设$\alpha=0.05$, $\beta=0.05$, $x=0.4$, $y=0.2$, $x'=0.22$, $y'=0.15$, $k=20$, $X_0=1$. 那么每个账号计算的结果大概是多少呢?

- A: 6.73
- B: 5.25
- C: 4.04
- E: 2.77

纯粹依靠改变资金分布就能改变同一美元最终的效果. 简单的金钱分配制度应该是获得了收入就是放满A类然后B类以此类推.

**常见问题**

- **Q:** 计算有问题, effective tax rate是根据具体收入改变的. 收入不一样了为什么会认为effective tax rate一样?
- **A:** 没有错, 上面的数值比较是有问题的. 但那个只是为了稍微显示可能的数值结果. 排序结果依然不变.

- **Q:** B类和C类可以放得钱的上限, 一个是pre-tax的上限, 一个是after-tax的上限. 但是存在B类和C类共用一个上限的时候. 例子就是401k和Roth 401k. 两个加起来的上限是\$19000. 那么为什么还应该用那个简单的分配制度呢? 因为完全可以放满\$19000的C类而不是\$19000的B类, 最终难道不会得利更多?
- **A:** 只考虑$B$和$C$类是没有错. 但具体情况要具体考虑. 我们可以获得以下的定论: 如果未来税率低于现在的税率的一定量的话, 则$B$类就比$C$类强. 简单的模型: 世界只有$B,C,E$三个种类(因为总是应该投A, 然后一般D类都可以转换为C类). 你拥有一定的收入$w$决定全部投资掉. 所以钱必须在$B,C,E$三个里面放. $B$的pre-tax+$C$的after-tax合在一起是$1$. 现在的税率为$x$, 取出税率为$x'$, capital gain tax rate为$y'$. 假如你的投资是直接放大输入的$k$倍. 假如投在$B$里面的income是$b$, 则最终税后投资回报为
$(w-b-\frac{1-b}{1-x})(1-x)(1+(k-1) (1-y')) + (kb)(1-x') + k(1-b)$. 这个是一个和$b$有关的线性函数, 这个函数的斜率是正的, 则放满$B$是最优, 不然放满$C$是最优. 假设$k\geq 1$. 导数为$(1-k) x y' + k (x - x')$. 如果$x-x'\geq xy'$即可得知$b=1$是最优. 这个的好处是根本都不用关心$k$有多大. 

- **Q:** 因为B类取出的时候是ordinary income. 如果投资很赚的话, 会导致退休的时候每年取钱的effective tax rate都很高. 那看前面的问题的答案, 说明after-tax才是最优的? 毕竟$x-x'$可能是0.
- **A:** 是的. 不过B和C差距最多就20%. 对于年轻人来说, 长期的盈利不交税得到的利滚利才是最大的区别.

- **Q:** 税率可能会改变, 那现在计算的税率未来可能不同.
- **A:** 是的, 如果非常怕税率涨, 就用C类. 如果怕税率跌, 就用B类.

### 最大化税务优惠

假设有这样一个人

 - 住在西雅图
 - 底薪: 160k
 - 奖金: 15%
 - 股票: 每年vest价值50k
 - 401k match: 100% match up to 6% of salary
 - 公司允许after tax 401k contribution(不是Roth 401k!), 并且允许在职的时候能partially rollover.

则每年会做以下的事情 (我现在用2019年的数字)

 - 最大化401k到employer match (\$19000 + \$9600=\$28600)
 - 最大化HSA (\$3500)
 - 最大化Commuter Benefits (\$3180)
 - 最大化401k (no change)
 - 最大化after tax 401k, traditional IRA, 然后 [backdoor Roth](https://www.forbes.com/sites/ashleaebeling/2012/01/23/the-backdoor-roth-ira-advanced-version/#f2f12d355ada)和[mega backdoor roth](https://www.madfientist.com/after-tax-contributions/)转到Roth IRA (\$56000 - \$28600 + \$6000 = \$33000)
 
极限挑战后,\$62000一年会在你的养老账户里. 其中\$33000可以5年后拿出来. 剩下的需要放在里面直到退休(实际上有技巧55岁拿出). \$3500给医疗用, \$3180给commuting用(并且必须当年用掉).
其中\$19000+\$3500+\$3180=\$25680是税前. \$33000是税后.

可以用[个税计算器](https://smartasset.com/taxes/income-taxes)得知以上的人税后应该有\$150000. 则最后存了上面的钱之后, 还剩\$117000(假设股票没变价值). 应该也是完全够一个人生活的了, 还可以做其他投资.

## 各种谈判

很多东西可以谈的, 最常见的是你的薪水. 永远应该讨论你的薪水, 也在觉得自己做的好的时候去要求涨工资.
这不仅仅是为了你, 还是为了你的公司. 这样其他公司就不会轻易的给你市场价就把你挖走了. 这样你和公司都受到了损失. [薪资谈判可以看这个文章](https://www.1point3acres.com/how-to-negotiate-salary/).
很多大型的消费也是可以谈出来的. 比如医疗手术. 这种都可以货比三家的.

# 节流

不要被消费主义洗脑. 定下你预算, 遵守预算. 记录你用钱的地方. 买东西前找好的deals. 加入loyalty programs如果你买多次.

## 信用卡
可以用信用卡的地方就不要用debit card. 只用到自己能还得起的钱. 创建好的信用记录. 找到好用的信用卡, 比如用[美国信用卡指南](https://www.uscreditcardguide.com/zh/). 自动付款, 并且自动付款整个balance.

### 我的组合

我钱包里有下面3个卡因为我还在薅Chase.

- 需要拿到bonus的卡.
- CSR吃饭和旅行. 国外出行也会带上.
- CFU[^chasefreedomunlimited]给一切其他的东西.

## 返现
你买的不少东西都有返现的机会. 其中一些和你的信用卡或者航空公司有关. 应该先去[Cashback Monitor](https://www.cashbackmonitor.com/)上有没有返现. 也可以去自己的有offer的portals看看有没有offer.

### 我的组合

- 不通过offer产生想买的欲望(击败消费主义). 而是决定买了东西之后, 看看有什么返现网站可以用.  
- 我一般用自己信用卡和航空公司的返现, 因为他们最可以相信. 除此以外, 我用过Rakuten [^rakuten], 结果还蛮好.

## 积分和里程
积分和里程很多有各种不同的名字, 我以下都叫积分. 需要考虑几样东西

1. *积分会贬值, 所以要赶快用*. 囤积积分没啥用, 某天积分都贬值了10%就等于囤的越多亏的越多. 就连UR这种可以直接变成statement credit的都在贬值. 因为你不当场用的话, 同样价值的钱你是可以拿去投资的. 
2. *理性看到积分价值*. 积分换到的东西对你的价格不是官方标注的价格. 而是如果你没有积分你会为他出多少钱的价格. 网上的积分评估的想法是你一般真的是有现金的时候都会去用那些价格买东西. 所以不要全信. 
对于机票来说, 他们的评估方法没太大问题(虽然还是有点上浮, 因为他们喜欢兑换商务舱). 机票定价几乎差不多而且需要机票的时候是刚需. 对于hotel来说就不一样了, 因为一个地区常常存在更便宜的hotel. 比如你可以用积分换个200美元的hotel. 但是你如果没有积分你会去住旁边的一个100美元的hotel. 那么你积分的价值并没有达到200美元, 换的积分的价值可能只有100美元. 

## 旅行
旅行的话值得学会用好积分. 能早早的plan好出行那是极好的.
航空公司的积分系统比较重要, 因为hotel总是可能有更便宜的. 用[Awardmapper](http://www.awardmapper.com/map.html)了解hotel的兑换状况.
关注几个重要的航空公司, 知道你[家旁边的机场是什么公司的hub](https://www.uscreditcardguide.com/airline-hubs/). 用[AwardHacker](https://www.awardhacker.com/)或者[Flyermiler](http://www.flyermiler.com/)找到哪些类型的航班有可能用上积分. 更加有经验的人可能会去研究[flyertalk](https://www.flyertalk.com/)上的文章. 
但是学习很多航空积分有关的东西耗时太久. 你只需要知道哪些技术大概是可行的, 买票前再去研究. 推荐看看[航空综述系列](https://www.uscreditcardguide.com/category/airline-miles-guide-zh/airline-summary/).
自己用现金买票的话了解一下[常见技巧](https://www.uscreditcardguide.com/why-is-my-flight-ticket-so-expensive/). 

### 我的组合

 - 我的主要航空公司为Delta和Alaska.
 - 如果有很好的获得其他航空公司的积分的deal应该去获得 (一般是开卡奖励). 
 - 国际旅行的话, 一般会用积分换取商务舱. 其他时间积分换取经济舱(或者直接现金买). 
 - Hotel只要有积分就能用掉就用掉. 
 
## 餐馆

少在外面吃. 但是如果[在外面吃的话尽量返现点钱](https://thepointsguy.com/2015/01/quadruple-dip-with-points-miles-discounts-at-restaurants/).

### 我的组合

1. 先选餐馆 (这一步不要看任何deals).
2. 用Seated或者Opentable订位, 如果可能的话. Seated[^seated]给你不少的返现(可以换成gift cards), 但是只在几个小城市里有用. [Opentable](https://www.opentable.com/)定也有返现.
3. 用[Alaska dining rewards](https://mileageplan.rewardsnetwork.com), 因为他们的miles最为值钱.
4. 看Groupon[^groupon]或Restaurant.com[^restaurant]有没有可以用的*优惠劵*. 这个你要好好的看好那个优惠券的要求, 不然会被反薅. 

## 时间

你做的一切都要花时间, 所以应该尽力自动化一切. 
时间本身也应该有现金的价值. 比如你觉得自己一小时的自由时间价值是(税后)50美元, 则如果你花了一小时省下了40美元, 就浪费了时间.

# 风险

一切都是风险. 任何对身体有损伤的事情都会获得经济风险. 当然风险一般对应的是高回报.
我来描述一些高风险的一般人不认为是投资的类型的事情.

 - 危险的爱好: 有的爱好更容易受伤. 比如攀岩, 滑雪. 很容易让自己几星期不能工作. 更危险的东西还会致死. 
 - 结婚: 结婚可能会有很大的税务优惠, 但收入差不多的两个高收入人, 会存在很大的[marriage penalty](https://en.wikipedia.org/wiki/Marriage_penalty). 
 - 离婚: 离婚是非常强的彻底摧毁一个人经济的方法. 特别是两个人中一方是高收入低支出但是另一方是低收入高支出. 很多州会要求钱50/50分. 为此可能导致低价卖掉房产. 有的州还要保证离婚的一方能保证自己正常的生活质量, 导致更有钱的一方会付alimony. 有小孩然后小孩判给了对方, 还要付常常比你自己会在那个小孩身上花的钱多很多child support. 这样离婚之后自己的收入都会大大的减少.
 - 孩子: 为了自己的孩子很多家长会付超级多的钱因为想让孩子获得最好的. 
 - 灾: 比如房子被火烧了这种. 大多的房子是自己的最大财产. 一次性就消失了.

我不是说因此人就不要做危险的运动结婚小孩买房了.
我是说要充分的了解风险, 并且因此准备好如何对抗风险. 

## 保险

保险自然是最好的对抗风险方式. 期望上来说保险公司自然会赚到钱. 所以你不需要给小的物件买保险, 因为你可以抗损. 但是对于经济价值大的东西, 是有买保险的必要的.

## 专业人士

你不可能一切都自己做, 虽然自己做可能更便宜.
用律师, 一定要用律师在失败后果非常大的事情上 [^imm].
用会计, 如果你的税务问题比turbotax[^turbotax]上能搞定的要复杂.
专业人士懂得比你多, 但是他们的incentive是为自己赚钱. 
都应该货比三家. 常常第一次的consultation是免费的. 


[^chasefreedomunlimited]: [Use this referral link for chase freedom unlimited](https://www.referyourchasecard.com/18/KRE9M4JY2C). Earn 3% cash back on all purchases in your first year up to $20,000 spent. After that, earn 1.5% cash back on all purchases.
[^turbotax]: [Use this referral link for Turbotax](http://fbuy.me/nwyLa) to get up to 20% off.
[^seated]: [Use this referral link for Seated](https://seated.app.link/j3FZwlVB1Y) and the code `CHAO19` to obtain extra \$5 dollars on your first reservation.
[^groupon]: Please [use this referral link for Groupon](https://www.groupon.com/visitor_referral/h/7a8e66c7-d3fa-467d-88c0-cb2fa0aa6384). I get \$10 and you don't get anything though.
[^restaurant]: Please [use this referral link for Restaurant.com](https://www.restaurant.com/referfriends/ReferredBy?refextid=f43f5fa8&prti=5157&ext=em_raf). I get \$10 gift card, you don't get anything though.
[^rakuten]: Please [use this referral link for Rakuten](https://www.rakuten.com/r/MGCCLX?eeid=28187) and get extra \$10.
[^checking]: [SoFi Money](https://www.sofi.com/share/money/2627476/) is a checking account with a very high APR for a checking account. Use my referral link and we both get \$50 after deposit of \$100. (You have to be a US person!) However, it is a Internet bank. If you really need physical services, I was using [chase total checking](https://accounts.chase.com/raf/share/2297938276).
[^savings]: For savings, find whatever offers the highest APR. Currently [Wealthfront](https://wlth.fr/2hp96Gw) has a pretty good offer.
[^mint]: If you are like me who only use credit cards, and there are only a few non-credit card purchase. Just link [Mint](https://mint.com) to credit cards and do some tracking. Track your checking accounts separately. This is because Mint often can't figure out transfers, and end up saying my budget is crazy high.
[^chasefreedom]: [Chase Freedom](https://www.referyourchasecard.com/2a/XDCUJDEKPJ) is fairly nice for its 5% categories. However, I generally never use them to their fullest.  
[^m1finance]: Use my referral for [M1 Finance](https://mbsy.co/zBs8G) and get \$10 for free.
[^personalcapital]: Use my referral for [Personal Capital](https://share.personalcapital.com/x/62x35X) to earn \$20 dollars. 
[^wealthfront]: Use this referral link to [Wealthfront](https://wlth.fr/2hp96Gw) to obtain \$5000 more managed free.
[^imm]: 曾经因为没有用律师弄移民导致吃了特别大的亏.