---
layout: post
title: Market cap weighted country portfolio from AQR data library
comments: true
---




Today I woud like to calculate market cap weighted benchmark portfolio using
[AQR](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)


{% highlight r %}
#*****************************************************************
# Load market equities ME from AQR data library
#******************************************************************
data  = env()

# monthly market returns in excess of t-bills
data$market.excess = qis.aqr.data('betting-against-beta-equity-factors', 'monthly', 'MKT')

# monthly U.S. Treasury bill rates.
data$risk.free     =  qis.aqr.data('betting-against-beta-equity-factors', 'monthly', 'RF', last.col2extract = 2)

# total market return
data$total.return        = data$market.excess + as.vector(data$risk.free)

# momentum 
data$momentum = data$market.excess

# check market return starting dates
print(qis.xts.start.dates(data$total.return))  
{% endhighlight %}



|USA        |CAN        |AUS        |AUT        |BEL        |CHE        |DEU        |DNK        |ESP        |FIN        |FRA        |GBR        |HKG        |IRL        |ITA        |JPN        |NLD        |NOR        |NZL        |SGP        |SWE        |PRT        |GRC        |ISR        |
|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|
|1926-07-31 |1982-03-31 |1985-11-30 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1988-02-29 |1988-09-30 |1994-12-31 |
    




{% highlight r %}
# total market value of equity as of the prior month in billion USD.
data$market.cap   = qis.aqr.data('betting-against-beta-equity-factors', 'monthly', 'ME(t-1)')

# check market cap starting dates
print(qis.xts.start.dates(data$market.cap)) 
{% endhighlight %}



|USA        |CAN        |AUS        |AUT        |BEL        |CHE        |DEU        |DNK        |ESP        |FIN        |FRA        |GBR        |HKG        |IRL        |ITA        |JPN        |NLD        |NOR        |NZL        |SGP        |SWE        |PRT        |GRC        |ISR        |
|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|
|1926-07-31 |1982-03-31 |1985-11-30 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1988-02-29 |1988-09-30 |1994-12-31 |
    




{% highlight r %}
# remove NA's
data$market.cap[]   = ifna(data$market.cap, 0)
data$total.return[] = ifna(data$total.return, 0)
data$momentum[]     = ifna(data$momentum, 0)


# remove data prior to 1986
for(n in ls(data)) 
  data[[n]] = data[[n]]['1986::']

dates = index(data$total.return)


#*****************************************************************
# Construct market cap weighted benchmark
#****************************************************************** 
benchmark.weight = data$market.cap / rowSums(data$market.cap)
benchmark.ret    = make.xts(rowSums(benchmark.weight * data$total.return), dates)
benchmark.index  = cumprod(1 + benchmark.ret)

# US benchmark
usa.index = cumprod(1 + data$total.return[,'USA'])

#*****************************************************************
# Plot Benchmarks
#*****************************************************************
qis.plot(benchmark.index, type ='l', ylim = range(usa.index), main="Market cap weighted country portfolios",col = col.add.alpha('red', 150))
qis.plot.lines(usa.index, type='l',col = col.add.alpha('blue', 150))
qis.plot.legend(spl('World,US'),'red,blue',as.list(merge(benchmark.index,usa.index)))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-27-aqr-world-portfolio/plot-2-1.png)

{% highlight r %}
#*****************************************************************
# Create strategy, Long top 5 countries, Short bottom 5 countries
#*****************************************************************
momentum         = mlag(data$momentum)
strategy.weight  = qis.ntop(momentum,5,T) - qis.ntop(momentum,5,F)
strategy.ret     = make.xts(rowSums(strategy.weight * data$total.return), dates)
strategy.index   = cumprod(1 + strategy.ret)

#*****************************************************************
# Plot Strategy
#*****************************************************************
qis.plot(strategy.index, type ='l', ylim = range(benchmark.index), main="Long Short Strategy",col = col.add.alpha('red', 150))
qis.plot.lines(benchmark.index, type='l',col = col.add.alpha('blue', 150))
qis.plot.legend(spl('Strategy,World'),'red,blue',as.list(merge(strategy.index,benchmark.index)))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-27-aqr-world-portfolio/plot-2-2.png)

{% highlight r %}
#*****************************************************************
# Create summary
#*****************************************************************
models = lst()
models$benchmark = lst(ret = benchmark.ret, equity = benchmark.index, weight = benchmark.weight)
models$strategy  = lst(ret = strategy.ret, equity = strategy.index, weight = strategy.weight)

qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-27-aqr-world-portfolio/plot-2-3.png)

{% highlight r %}
print(qis.plot.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|           |benchmark         |strategy          |
|:----------|:-----------------|:-----------------|
|Period     |Jan1986 - Dec2017 |Jan1986 - Dec2017 |
|Cagr       |8.97              |6.51              |
|AveRet     |8.95              |6.5               |
|Sharpe     |0.64              |0.54              |
|Volatility |15.28             |13.25             |
|MaxDD      |-54.49            |-35.49            |
|AveDD      |-6.92             |-9.65             |
|VaR        |-7.02             |-5.69             |
|CVaR       |-10.3             |-7.85             |
    




{% highlight r %}
#*****************************************************************
# Plot weights
#*****************************************************************
qis.plot.strategy.weights(models$benchmark$weight, 'benchmark', sort.asssets=F)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-27-aqr-world-portfolio/plot-2-4.png)

{% highlight r %}
qis.plot.strategy.weights(models$strategy$weight, 'strategy', sort.asssets=F)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-27-aqr-world-portfolio/plot-2-5.png)


*(this report was produced on: 2018-02-28)*
