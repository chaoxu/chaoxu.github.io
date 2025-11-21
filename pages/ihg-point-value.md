---
title: IHG point value
---

Suppose you find a cash rate at an IHG hotel and you also have the option to buy IHG points. How can you tell whether redeeming points is actually cheaper than paying cash?

Assume IHG points can be purchased at <span class="dm-var" data-name="buy" data-value="0.5" contenteditable="true"></span> cpp. After factoring in a <span class="dm-var" data-name="cc_buy" data-value="2.625" contenteditable="true"></span>% credit-card cashback and a <span class="dm-var" data-name="portal_buy" data-value="2.52" contenteditable="true"></span>% portal cashback, your effective cost becomes $v=$**<span class="dm-expr" data-name="v" data-expr="buy*(1-0.01*(portal_buy+cc_buy))" data-format="4"></span>cpp**.

Now suppose the room's base rate is $r=$ $<span class="dm-var" data-name="r" data-value="100.00" contenteditable="true" data-format="2"></span>, and taxes and fees add $t=$<span class="dm-var" data-name="t" data-value="15" contenteditable="true"></span>% bringing the total cost to $s=$<span class="dm-expr"
            data-name="s"
            data-expr="r*(1+t*0.01)" data-format="currency">
      </span>.

Paying cash earns you $m=$<span class="dm-var" data-name="m" data-value="20" contenteditable="true"></span> points per dollar spent on the base rate, or $mr=$<span class="dm-expr" data-name="mr" data-expr="m*r"></span> points in total, worth <span class="dm-expr" data-name="mrv" data-expr="mr*v*0.01" data-format="currency"></span> at your valuation. Your shopping portal returns <span class="dm-var" data-name="Cr" data-value="6" contenteditable="true"></span>% of the base rate (<span class="dm-expr" data-name="Crr" data-expr="Cr*r*0.01" data-format="currency"></span>), and your credit card returns <span class="dm-var" data-name="Cs" data-value="7" contenteditable="true"></span>% of total price (<span class="dm-expr" data-name="Css" data-expr="Cs*s*0.01" data-format="currency"></span>). After subtracting all of these benefits, the net cost of paying cash is <span class="dm-expr" data-name="sv" data-expr="s-Css-Crr-mrv" data-format="currency"></span>.

If a redemption costs $p$ points, then to come out ahead you need $pv\leq$<span class="dm-expr" data-name="sv" data-expr="s-Css-Crr-mrv" data-format="2"></span>, which gives a maximum sensible redemption cost of <span class="dm-expr" data-name="svv" data-expr="Math.floor(sv/v*100)" data-format="int"></span> points. Equivalently, you should only redeem points when their effective value is at least **<span class="dm-expr" data-name="sv" data-expr="(v*s)/sv" data-format="2"></span>cpp**. If you have the old IHG card which gives you 10% redemption rebate, then effective value of <span class="dm-expr" data-name="svb" data-expr="sv*0.9" data-format="2"></span>cpp is sufficient.

Finally, suppose IHG also lets you buy a bonus bundle: <span class="dm-var" data-name="ep" data-value="5000" contenteditable="true" data-format="int"></span> points for \$<span class="dm-var" data-name="ec" data-value="31" contenteditable="true"></span>. Applying the same rebate logic, you effectively spend <span class="dm-expr" data-name="mecv" data-expr="ec - Cs*0.01*(ec*(1+t*0.01)) - Cr*0.01*ec" data-format="currency"></span> for <span class="dm-expr" data-name="epp" data-expr="ep+m*ec" data-format="int"></span> points, or **<span class="dm-expr" data-name="eccpp" data-expr="mecv/(ep+m*ec)*100" data-format="4"></span>cpp**.
