---
layout: post
title:  Sector and sector neutral residual reversal strategies
comments: false
---




In my previous post [Residual reversal strategy](https://sysresearcher.github.io/blitz-reversal) I implemented residual reversal strategies as proposed in the
[Short-Term Residual Reversal](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1911449) paper.
 
Today I would like to create sector and sector neutral residual strategies.



{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
prices.file        = 'sp500.Rdata'
spy.file           = 'spy.Rdata'
signals.file       = 'signals.Rdata'
dates.range        = '2000::'
periodicity        = 'months'
run.regression     = F

#*****************************************************************
# Get tickers
#*****************************************************************
tickers          = qis.sp500.components()

#*****************************************************************
# Get historical prices for stocks in S&P
#*****************************************************************
data = qis.yahoo.data(tickers = tickers, file = prices.file, dates = dates.range, obj.type = 'env', debug = F)

# remove BHF
bad.tickers = spl('BHF,EVRG')

rm.index = which(ls(data) %in% bad.tickers)
env.rm(bad.tickers, data)

rm.index         = which(names(data$prices) %in% bad.tickers)
data$prices      = data$prices[,-rm.index]
data$symbolnames = data$symbolnames[!data$symbolnames %in% bad.tickers]

tickers          = data$symbolnames


#*****************************************************************
# Find Sectors for each company given http://www.sectorspdr.com/sectorspdr/
#****************************************************************** 
sector.map = qis.sector.universe('spdr')
# map our universe against sectors universe
sectors        = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers


#*****************************************************************
# Get historical prices for SPY
#*****************************************************************
spy  = qis.yahoo.data(tickers = 'SPY', file = spy.file, dates = dates.range, obj.type = 'env', debug = F)

#*****************************************************************
# Clean prices
#****************************************************************** 
prices = data$prices; prices = qis.apply.matrix(prices, function(x) ifna.prev(x))
spy    = spy$prices;  spy    = qis.apply.matrix(spy, function(x) ifna.prev(x))

#****************************************************************** 
# check missing data for financial firms
# print(sapply(prices, count))


#*****************************************************************
# Setup monthly periods
#****************************************************************** 
period.ends        = endpoints(prices, periodicity)
period.ends        = period.ends[period.ends > 0]
prices.period      = prices[period.ends,]

# compute returns
ret             = prices.period / mlag(prices.period) - 1
next.period.ret = mlag(ret, -1)


if(periodicity == 'months') {
  prices.period = qis.xts.format.dates(prices.period, "yyyymm")
} else {
  prices.period  = prices[period.ends]
}

#*****************************************************************
# Fama/French factors
#*****************************************************************
factors  = qis.fama.french.data('F-F_Research_Data_Factors', periodicity, force.download  = F)$data

#*****************************************************************
# Overlap prices and factors
#*****************************************************************
data         = env()
data$dates   = index(prices.period)
common.dates = as.Date(qis.intersect.all(index(prices.period), index(factors)))
data$prices  = prices.period
data$factors = make.xts(matrix(NA,nrow=nrow(prices.period), ncol = dim(factors)[2]),index(prices.period) )
data$factors[common.dates,][]  = factors[common.dates,] / 100.0
data$factors = qis.apply.matrix(data$factors, function(x) ifna.prev(x))
names(data$factors) = names(factors)


#*****************************************************************
# Factor Loading Regression
#*****************************************************************
if(run.regression) {
  signals                            = list()
  signals[['Rev.Last.Err']]     = NA * prices
  signals[['Rev.Last.Err.Std']] = NA * prices
  
  for(i in tickers) {
    out = qis.factor.rolling.regression(data, i, window.len = 36, silent = T, qis.factor.rolling.regression.custom.stats) 
    
    for(j in 1:len(signals))
      signals[[j]][period.ends,i] = out$fl$custom[,j]
  }
  
  signals[['Mom.1M']]                = NA * prices
  signals[['Mom.1M']][period.ends,]  = ret
  
  signals[['Rev.1M']]                = NA * prices
  signals[['Rev.1M']]                = signals[['Mom.1M']] 
  
  save(signals, file = signals.file)
} else {
  load(signals.file)
}


compare.models = lst()


#*****************************************************************
# Test reversion strategies per sector and sector neutral
#****************************************************************
  
# Models
models = lst()

# what signal are we testing
signal.type = "Rev.1M"

# get quintiles for Reversion strategy
out = qis.make.quintiles.sector(sectors, signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = F, next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-1.png)

{% highlight r %}
# strategy names original
model.names.orig = ls(out)

# Strategy's names
model.names = paste0(signal.type,".",model.names.orig)

# Test quintiles
for(i in 1:len(model.names)) {
  
  model.name = model.names[i]
  
  # create config
  config = lst(); config[[model.name]] = yaml.load_file('input.yaml')$input
  
  # Create data objects
  data = lst(prices =  prices, data.source = 'yahoo')
  
  # Prepare for backtest
  data   = qis.prep.with.config(data,config)
  
  # Create weights
  weights               = NA * data$prices
  weights[period.ends,] = out[[model.names.orig[i]]]
  weights               = qis.apply.matrix(weights, function(x) ifna.prev(x))
  weights               = ifna(weights,0)
  
  #TDD dates
  tdd.index = period.ends[1:(len(period.ends)-2)]
  tdd.dates  = index(prices)[tdd.index]
  
  # TD 1 day after TDD
  td.index = tdd.index +1
  td.dates  = index(prices)[td.index]
  
  # Update data object
  data$tdd.dates = tdd.dates
  data$tdd.index = tdd.index
  data$td.dates  = td.dates
  data$td.index  = td.index
  
  # store weights
  data$store[[model.name]]$w = weights
  
  # create index
  data = qis.ef.index(data,config)
  
  # index level
  index = data[[model.name]]$index
  
  # calculate return
  returns = index / mlag(index) -1
  returns = ifna(returns,0)
  
  # create model
  models[[model.name]]  = lst(ret = returns, equity = index, weight = make.xts(weights, data$dates))
  
  
  if(model.name == 'Rev.1M.Sector.Neutral'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
  
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Rev.1M.ConsumerDiscretionary |Rev.1M.ConsumerStaples |Rev.1M.Energy     |Rev.1M.Financials |Rev.1M.HealthCare |Rev.1M.Industrials |Rev.1M.Materials  |Rev.1M.RealEstate |Rev.1M.Sector.Neutral |Rev.1M.Technology |Rev.1M.Utilities  |
|:-----------|:----------------------------|:----------------------|:-----------------|:-----------------|:-----------------|:------------------|:-----------------|:-----------------|:---------------------|:-----------------|:-----------------|
|Period      |Jan2003 - Aug2018            |Jan2003 - Aug2018      |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018  |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018     |Jan2003 - Aug2018 |Jan2003 - Aug2018 |
|Cagr        |1.77                         |4.09                   |-7.59             |-3.67             |6.73              |3.73               |-6.84             |10.64             |1.95                  |5.48              |-5.11             |
|AveRet      |1.77                         |4.1                    |-7.59             |-3.67             |6.74              |3.73               |-6.84             |10.65             |1.95                  |5.49              |-5.12             |
|Sharpe      |0.19                         |0.36                   |-0.28             |-0.07             |0.5               |0.31               |-0.16             |0.59              |0.27                  |0.44              |-0.37             |
|Volatility  |16.38                        |13.73                  |20.6              |21.38             |15.46             |15.77              |24.88             |20.96             |8.62                  |14.38             |12.26             |
|AveTurnover |1851.33                      |1907.6                 |1818.37           |1850.66           |1851.12           |1851               |1802.65           |1898.67           |1848.31               |1882.84           |1768.84           |
|MaxDD       |-34.05                       |-42.64                 |-80.6             |-68.84            |-34.07            |-39.3              |-78.71            |-39.98            |-24.87                |-26.9             |-59.56            |
|AveDD       |-4.67                        |-2.69                  |-9.26             |-68.84            |-3.76             |-3.3               |-10.02            |-3.86             |-1.63                 |-3.59             |-40.48            |
|VaR         |-1.51                        |-1.26                  |-1.96             |-1.51             |-1.46             |-1.4               |-2.24             |-1.6              |-0.74                 |-1.41             |-1.16             |
|CVaR        |-2.26                        |-1.86                  |-3.05             |-3.07             |-2.16             |-2.24              |-3.66             |-3.03             |-1.27                 |-1.97             |-1.84             |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = "Reversion strategies")
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-2.png)

{% highlight r %}
#*****************************************************************
# Test residual reversion strategies per sector and sector neutral
#****************************************************************

# Models
models = lst()

# what signal are we testing
signal.type = "Rev.Last.Err"

# get quintiles for Reversion strategy
out = qis.make.quintiles.sector(sectors, signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = F, next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-3.png)

{% highlight r %}
# strategy names original
model.names.orig = ls(out)

# Strategy's names
model.names = paste0(signal.type,".",model.names.orig)

# Test quintiles
for(i in 1:len(model.names)) {
  
  model.name = model.names[i]
  
  # create config
  config = lst(); config[[model.name]] = yaml.load_file('input.yaml')$input
  
  # Create data objects
  data = lst(prices =  prices, data.source = 'yahoo')
  
  # Prepare for backtest
  data   = qis.prep.with.config(data,config)
  
  # Create weights
  weights               = NA * data$prices
  weights[period.ends,] = out[[model.names.orig[i]]]
  weights               = qis.apply.matrix(weights, function(x) ifna.prev(x))
  weights               = ifna(weights,0)
  
  #TDD dates
  tdd.index = period.ends[1:(len(period.ends)-2)]
  tdd.dates  = index(prices)[tdd.index]
  
  # TD 1 day after TDD
  td.index = tdd.index +1
  td.dates  = index(prices)[td.index]
  
  # Update data object
  data$tdd.dates = tdd.dates
  data$tdd.index = tdd.index
  data$td.dates  = td.dates
  data$td.index  = td.index
  
  # store weights
  data$store[[model.name]]$w = weights
  
  # create index
  data = qis.ef.index(data,config)
  
  # index level
  index = data[[model.name]]$index
  
  # calculate return
  returns = index / mlag(index) -1
  returns = ifna(returns,0)
  
  # create model
  models[[model.name]]  = lst(ret = returns, equity = index, weight = make.xts(weights, data$dates))
  
  
  if(model.name == 'Rev.Last.Err.Sector.Neutral'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
  
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Rev.Last.Err.ConsumerDiscretionary |Rev.Last.Err.ConsumerStaples |Rev.Last.Err.Energy |Rev.Last.Err.Financials |Rev.Last.Err.HealthCare |Rev.Last.Err.Industrials |Rev.Last.Err.Materials |Rev.Last.Err.RealEstate |Rev.Last.Err.Sector.Neutral |Rev.Last.Err.Technology |Rev.Last.Err.Utilities |
|:-----------|:----------------------------------|:----------------------------|:-------------------|:-----------------------|:-----------------------|:------------------------|:----------------------|:-----------------------|:---------------------------|:-----------------------|:----------------------|
|Period      |Jan2003 - Aug2018                  |Jan2003 - Aug2018            |Jan2003 - Aug2018   |Jan2003 - Aug2018       |Jan2003 - Aug2018       |Jan2003 - Aug2018        |Jan2003 - Aug2018      |Jan2003 - Aug2018       |Jan2003 - Aug2018           |Jan2003 - Aug2018       |Jan2003 - Aug2018      |
|Cagr        |1.47                               |9.98                         |-6.6                |2.33                    |11.43                   |7.62                     |1.59                   |14.05                   |5.69                        |9.44                    |-3.96                  |
|AveRet      |1.47                               |10                           |-6.6                |2.33                    |11.44                   |7.63                     |1.59                   |14.07                   |5.69                        |9.45                    |-3.97                  |
|Sharpe      |0.17                               |0.78                         |-0.24               |0.22                    |0.8                     |0.63                     |0.18                   |0.78                    |0.86                        |0.73                    |-0.29                  |
|Volatility  |14.97                              |13.41                        |19.95               |17.13                   |14.97                   |12.99                    |22.98                  |19.11                   |6.73                        |13.75                   |11.66                  |
|AveTurnover |1845.94                            |1922.77                      |1805.38             |1862.58                 |1825.9                  |1860.24                  |1823                   |1886.37                 |1849.82                     |1867.31                 |1798.72                |
|MaxDD       |-54.13                             |-19.22                       |-78.76              |-43.71                  |-30.03                  |-31.68                   |-50.7                  |-31.7                   |-13.33                      |-24.56                  |-51.18                 |
|AveDD       |-3.99                              |-2.92                        |-9.14               |-4.14                   |-2.75                   |-2.3                     |-5.04                  |-3.96                   |-1.13                       |-2.86                   |-51.18                 |
|VaR         |-1.37                              |-1.23                        |-1.9                |-1.31                   |-1.4                    |-1.24                    |-2.13                  |-1.53                   |-0.57                       |-1.32                   |-1.13                  |
|CVaR        |-1.97                              |-1.72                        |-2.95               |-2.46                   |-2.03                   |-1.78                    |-3.27                  |-2.63                   |-0.92                       |-1.87                   |-1.73                  |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = "Residual reversion strategies Version 1")
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-4.png)

{% highlight r %}
#*****************************************************************
# Test residual reversion strategies per sector and sector neutral
#****************************************************************

# Models
models = lst()

# what signal are we testing
signal.type = "Rev.Last.Err.Std"

# get quintiles for Reversion strategy
out = qis.make.quintiles.sector(sectors, signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = F, next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-5.png)

{% highlight r %}
# strategy names original
model.names.orig = ls(out)

# Strategy's names
model.names = paste0(signal.type,".",model.names.orig)

# Test quintiles
for(i in 1:len(model.names)) {
  
  model.name = model.names[i]
  
  # create config
  config = lst(); config[[model.name]] = yaml.load_file('input.yaml')$input
  
  # Create data objects
  data = lst(prices =  prices, data.source = 'yahoo')
  
  # Prepare for backtest
  data   = qis.prep.with.config(data,config)
  
  # Create weights
  weights               = NA * data$prices
  weights[period.ends,] = out[[model.names.orig[i]]]
  weights               = qis.apply.matrix(weights, function(x) ifna.prev(x))
  weights               = ifna(weights,0)
  
  #TDD dates
  tdd.index = period.ends[1:(len(period.ends)-2)]
  tdd.dates  = index(prices)[tdd.index]
  
  # TD 1 day after TDD
  td.index = tdd.index +1
  td.dates  = index(prices)[td.index]
  
  # Update data object
  data$tdd.dates = tdd.dates
  data$tdd.index = tdd.index
  data$td.dates  = td.dates
  data$td.index  = td.index
  
  # store weights
  data$store[[model.name]]$w = weights
  
  # create index
  data = qis.ef.index(data,config)
  
  # index level
  index = data[[model.name]]$index
  
  # calculate return
  returns = index / mlag(index) -1
  returns = ifna(returns,0)
  
  # create model
  models[[model.name]]  = lst(ret = returns, equity = index, weight = make.xts(weights, data$dates))
  
  
  if(model.name == 'Rev.Last.Err.Std.Sector.Neutral'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
  
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Rev.Last.Err.Std.ConsumerDiscretionary |Rev.Last.Err.Std.ConsumerStaples |Rev.Last.Err.Std.Energy |Rev.Last.Err.Std.Financials |Rev.Last.Err.Std.HealthCare |Rev.Last.Err.Std.Industrials |Rev.Last.Err.Std.Materials |Rev.Last.Err.Std.RealEstate |Rev.Last.Err.Std.Sector.Neutral |Rev.Last.Err.Std.Technology |Rev.Last.Err.Std.Utilities |
|:-----------|:--------------------------------------|:--------------------------------|:-----------------------|:---------------------------|:---------------------------|:----------------------------|:--------------------------|:---------------------------|:-------------------------------|:---------------------------|:--------------------------|
|Period      |Jan2003 - Aug2018                      |Jan2003 - Aug2018                |Jan2003 - Aug2018       |Jan2003 - Aug2018           |Jan2003 - Aug2018           |Jan2003 - Aug2018            |Jan2003 - Aug2018          |Jan2003 - Aug2018           |Jan2003 - Aug2018               |Jan2003 - Aug2018           |Jan2003 - Aug2018          |
|Cagr        |1.79                                   |7.33                             |-7.57                   |3.33                        |7.43                        |6.71                         |-0.07                      |14.74                       |4.5                             |5.36                        |-2.2                       |
|AveRet      |1.79                                   |7.34                             |-7.58                   |3.33                        |7.43                        |6.72                         |-0.07                      |14.76                       |4.51                            |5.37                        |-2.2                       |
|Sharpe      |0.2                                    |0.63                             |-0.33                   |0.28                        |0.59                        |0.61                         |0.1                        |0.86                        |0.73                            |0.48                        |-0.14                      |
|Volatility  |14.1                                   |12.46                            |18.45                   |16.83                       |13.66                       |11.8                         |19.84                      |17.9                        |6.36                            |12.38                       |11.35                      |
|AveTurnover |1891.94                                |1947.01                          |1812.85                 |1905.01                     |1890.19                     |1925.33                      |1854.56                    |1908.05                     |1889.17                         |1919.55                     |1837.26                    |
|MaxDD       |-59.58                                 |-21.79                           |-80.79                  |-37.74                      |-39.56                      |-23.95                       |-47.37                     |-37.7                       |-12.76                          |-30.46                      |-39.88                     |
|AveDD       |-3.82                                  |-2.55                            |-17.54                  |-3.55                       |-2.68                       |-2.22                        |-8.12                      |-3.14                       |-1.04                           |-3.18                       |-10.96                     |
|VaR         |-1.26                                  |-1.16                            |-1.79                   |-1.19                       |-1.27                       |-1.09                        |-1.93                      |-1.37                       |-0.55                           |-1.22                       |-1.06                      |
|CVaR        |-1.89                                  |-1.67                            |-2.75                   |-2.4                        |-1.88                       |-1.57                        |-2.85                      |-2.41                       |-0.87                           |-1.72                       |-1.67                      |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = "Residual reversion strategies Version 2")
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-6.png)

{% highlight r %}
#*****************************************************************
# Summary for reversion strategies
#*****************************************************************

# trim models
compare.models = qis.bt.trim(compare.models, dates  = "2003::")


# Strategy performance
#print(qis.plot.strategy.sidebyside(compare.models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
print(qis.plot.strategy.sidebyside(compare.models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.with.weights))
{% endhighlight %}



|               |Rev.1M.Sector.Neutral |Rev.Last.Err.Sector.Neutral |Rev.Last.Err.Std.Sector.Neutral |
|:--------------|:---------------------|:---------------------------|:-------------------------------|
|Period         |Jan2003 - Aug2018     |Jan2003 - Aug2018           |Jan2003 - Aug2018               |
|Cagr           |1.95                  |5.69                        |4.5                             |
|AveRet         |1.95                  |5.69                        |4.51                            |
|Sharpe         |0.27                  |0.86                        |0.73                            |
|Volatility     |8.62                  |6.73                        |6.36                            |
|AveTurnover    |1848.31               |1849.82                     |1889.17                         |
|MaxLongWeight  |100                   |100                         |100                             |
|MinLongWeight  |0                     |0                           |0                               |
|MaxShortWeight |0                     |0                           |0                               |
|MinShortWeight |-100                  |-100                        |-100                            |
|MaxNetWeight   |0                     |0                           |0                               |
|MinNetWeight   |0                     |0                           |0                               |
|MaxGrossWeight |200                   |200                         |200                             |
|MinGrossWeight |0                     |0                           |0                               |
|MaxDD          |-24.87                |-13.33                      |-12.76                          |
|AveDD          |-1.63                 |-1.13                       |-1.04                           |
|VaR            |-0.74                 |-0.57                       |-0.55                           |
|CVaR           |-1.27                 |-0.92                       |-0.87                           |
    




{% highlight r %}
# Plot backtests
qis.plot.strategy(compare.models, plotX = T, log = 'y', LeftMargin = 3, main = 'Reversion strategies')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-7.png)

{% highlight r %}
#*****************************************************************
# Plot Portfolio Turnovers
#****************************************************************** 
qis.barplot.with.labels(sapply(compare.models, qis.compute.turnover), 'Average Annual Portfolio Turnover', col = col.add.alpha(3))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-03-blitz-reversal-sector/plot-2-8.png)


*(this report was produced on: 2018-09-15)*
