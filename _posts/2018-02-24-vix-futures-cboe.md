---
layout: post
title: Build VIX rolling futures using data from CBOE
comments: true
---




Today I woud like to show how to build rolling VIX futures using data from [CBOE](http://www.cboe.com/).
 The term structure of VIX futures can be found [here](http://www.cboe.com/data/volatilityindexes/volatilityindexes.aspx). 
The historical VIX futures information can be found here, for example, [H18](http://cfe.cboe.com/Publish/ScheduledTask/MktData/datahouse/CFE_H18_VX.csv).




{% highlight r %}
#*****************************************************************
# Get VIX historical data from CBOE for specific contracts
#*****************************************************************
out      = qis.cboe.vix.futures('VX',2018,3,T)
print(to.nice(last(out[["H18_VX"]],10)))
{% endhighlight %}



|           |Open  |High  |Low   |Close |Settle |Change |Total_Volume |EFP      |Open_Interest |
|:----------|:-----|:-----|:-----|:-----|:------|:------|:------------|:--------|:-------------|
|2018-02-08 |19.70 |23.39 |18.75 |21.60 |21.65  |1.78   |270,960.00   |2,598.00 |224,701.00    |
|2018-02-09 |21.60 |23.44 |20.34 |20.40 |20.43  |-1.22  |231,284.00   |917.00   |231,520.00    |
|2018-02-12 |20.30 |21.20 |19.75 |19.85 |19.82  |-0.60  |183,266.00   |5,507.00 |235,693.00    |
|2018-02-13 |19.85 |21.10 |19.80 |19.84 |19.82  |0.00   |138,627.00   |0.00     |239,164.00    |
|2018-02-14 |19.85 |20.81 |17.65 |17.85 |17.88  |-1.95  |177,563.00   |3,788.00 |234,454.00    |
|2018-02-15 |17.90 |18.15 |17.10 |17.55 |17.52  |-0.35  |117,472.00   |0.00     |225,204.00    |
|2018-02-16 |17.50 |18.02 |16.90 |17.76 |17.77  |0.25   |102,161.00   |0.00     |214,781.00    |
|2018-02-20 |17.70 |18.95 |17.33 |18.36 |18.38  |0.60   |98,918.00    |0.00     |206,395.00    |
|2018-02-21 |18.35 |18.95 |17.10 |18.56 |18.57  |0.20   |114,710.00   |0.00     |200,203.00    |
|2018-02-22 |18.55 |19.40 |17.85 |18.10 |18.07  |-0.50  |92,083.00    |0.00     |191,543.00    |
    




{% highlight r %}
#*****************************************************************
# Plot time series
#*****************************************************************
qis.plot.xts(out$H18_VX[,'Settle'],main = paste0('Mar-2018 Future as of ',  max(index(out$H18_VX))))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-24-vix-futures-cboe/plot-2-1.png)

{% highlight r %}
#*****************************************************************
# Get snapshot of VIX futures curve (works only on business day)
#*****************************************************************
{% endhighlight %}

{% highlight r %}
out = qis.cboe.vix.futures.term.structure()
print(to.nice(out))
{% endhighlight %}

{% highlight r %}
#*****************************************************************
# Build rolling VIX futures ( simple roll on expiration)
#*****************************************************************
out = qis.cboe.vix.rolling.futures('1,3,6')

#*****************************************************************
# Plot VIX Futures
#*****************************************************************
qis.plot.xts(out, main = paste0("Rolling VIX Futures as of ", index(last(out))))
{% endhighlight %}

![plot of chunk plot-4](/public/images/2018-02-24-vix-futures-cboe/plot-4-1.png)


*(this report was produced on: 2018-02-25)*
