---
layout: post
title:  Residual reversal strategy
comments: false
---




In the paper [Short-Term Residual Reversal](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1911449) was shown that conventional
short-term reversal strategies have dynamic exposure to [Fama and French](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html) factors.
They develop an alternative weighting scheme that does not exhibit these exposures which improves the performance.
They used a rolling factor attribution regression on the prior 36 months and compute residuals which are used for ranking stocks. 

I would like to check this idea in this post.



{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
prices.file        = 'sp500.Rdata'
spy.file           = 'spy.Rdata'
signals.file       = 'signals.Rdata'
models.file        = 'models.Rdata'

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
#bad.tickers = spl('BHF,EVRG')
bad.tickers = spl('BHF')

rm.index = which(ls(data) %in% bad.tickers)
env.rm(bad.tickers, data)

rm.index         = which(names(data$prices) %in% bad.tickers)
data$prices      = data$prices[,-rm.index]
data$symbolnames = data$symbolnames[!data$symbolnames %in% bad.tickers]

tickers = data$symbolnames
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
  # data from month end moved to the begining of month
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
#data         = env()
#data$dates   = qis.intersect.all(index(prices), index(factors))
#data$prices  = prices[index(prices) %in% data$dates]
#data$factors = factors[index(factors) %in% data$dates] / 100.0


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
  signals                       = list()

  signals[['Rev.Last.Err']]     = NA * prices
  signals[['Rev.Last.Err.Std']] = NA * prices
  
  for(i in tickers) {
    out = qis.factor.rolling.regression(data, i, window.len = 36, silent = T, qis.factor.rolling.regression.custom.stats) 
    
    for(j in 1:len(signals))
      signals[[j]][period.ends,i] = out$fl$custom[,j]
  }
  
  signals[['Mom.1M']]           = NA * prices
  signals[['Mom.1M']][period.ends,]  = ret
  
  signals[['Rev.1M']]                = NA * prices
  signals[['Rev.1M']]                = signals[['Mom.1M']] 
  
  save(signals, file = signals.file)
} else {
  load(signals.file)
}

# Models
compare.models = lst()

#*****************************************************************
# Test equally weighted and simple SPY strategies
#*****************************************************************
models = lst()

# Model's names
model.names = spl('SPY,Equal.Weight')

# Test models
for(i in 1:len(model.names)) {
  
  model.name = model.names[i]
  
  # create config
  config = lst(); config[[model.name]] = yaml.load_file('input.yaml')$input

  # Create data objects
  data = lst(prices = iif(model.name == 'SPY', spy, prices), data.source = 'yahoo')
  
  # Prepare for backtest
  data   = qis.prep.with.config(data,config)
  
  # Create weights
  weights  = data$prices; weights[] = NA
  
  if(model.name == 'SPY'){
    weights[period.ends[1],] = 1.0
    weights = qis.apply.matrix(weights, function(x) ifna.prev(x))
  } else if(model.name == 'Equal.Weight'){
    weights[period.ends,] = qis.ntop(prices[period.ends,], data$nassets)
  }
  
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
}

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |SPY               |Equal.Weight      |
|:-----------|:-----------------|:-----------------|
|Period      |Jan2000 - Aug2018 |Jan2000 - Aug2018 |
|Cagr        |5.91              |16.93             |
|AveRet      |5.92              |16.96             |
|Sharpe      |0.4               |0.88              |
|Volatility  |19.13             |20.11             |
|AveTurnover |0                 |0                 |
|MaxDD       |-55.19            |-52.81            |
|AveDD       |-1.91             |-1.95             |
|VaR         |-1.91             |-1.85             |
|CVaR        |-2.87             |-2.99             |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Equal weight strategy')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-1.png)

{% highlight r %}
#*****************************************************************
# Test momentum strategy
#*****************************************************************

# Models
models = lst()

# what signal are we testing
signal.type = "Mom.1M"

# get quintiles for Reversion strategy
out = qis.make.quintiles(signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = T, position.tracker  = NULL, next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-2.png)

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
  data = lst(prices = iif(model.name == 'SPY', spy, prices), data.source = 'yahoo')
  
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
  
  if(model.name == 'Mom.1M.Spread'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Mom.1M.Q1         |Mom.1M.Q2         |Mom.1M.Q3         |Mom.1M.Q4         |Mom.1M.Q5         |Mom.1M.Spread     |
|:-----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period      |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |
|Cagr        |19.61             |19.51             |16.83             |15.72             |17.37             |-3.72             |
|AveRet      |19.63             |19.54             |16.85             |15.73             |17.39             |-3.72             |
|Sharpe      |0.85              |0.99              |0.94              |0.88              |0.87              |-0.21             |
|Volatility  |24.7              |19.94             |18.43             |18.62             |20.89             |13.61             |
|AveTurnover |1837.71           |1869.55           |1846.65           |1860.68           |1855.43           |1846.57           |
|MaxDD       |-65.63            |-51.54            |-45.78            |-47.32            |-58.61            |-52.89            |
|AveDD       |-2.04             |-1.64             |-1.74             |-1.86             |-2.09             |-8.45             |
|VaR         |-2.12             |-1.73             |-1.69             |-1.69             |-1.96             |-1.19             |
|CVaR        |-3.73             |-2.99             |-2.81             |-2.82             |-3.17             |-2.09             |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Momentum strategies')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-3.png)

{% highlight r %}
#*****************************************************************
# Test reversal strategy
#*****************************************************************

# Models
models = lst()

# what signal are we testing
signal.type = "Rev.1M"

# get quintiles for Reversion strategy
out = qis.make.quintiles(signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = F, position.tracker = NULL, next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-4.png)

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
  data = lst(prices = iif(model.name == 'SPY', spy, prices), data.source = 'yahoo')
  
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
  
  if(model.name == 'Rev.1M.Spread'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Rev.1M.Q1         |Rev.1M.Q2         |Rev.1M.Q3         |Rev.1M.Q4         |Rev.1M.Q5         |Rev.1M.Spread     |
|:-----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period      |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |
|Cagr        |19.61             |19.51             |16.83             |15.72             |17.37             |2.3               |
|AveRet      |19.63             |19.54             |16.85             |15.73             |17.39             |2.3               |
|Sharpe      |0.85              |0.99              |0.94              |0.88              |0.87              |0.24              |
|Volatility  |24.7              |19.94             |18.43             |18.62             |20.89             |13.42             |
|AveTurnover |1837.71           |1869.55           |1846.65           |1860.68           |1855.43           |1846.57           |
|MaxDD       |-65.63            |-51.54            |-45.78            |-47.32            |-58.61            |-29.02            |
|AveDD       |-2.04             |-1.64             |-1.74             |-1.86             |-2.09             |-2.41             |
|VaR         |-2.12             |-1.73             |-1.69             |-1.69             |-1.96             |-1.14             |
|CVaR        |-3.73             |-2.99             |-2.81             |-2.82             |-3.17             |-1.95             |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Reversion strategies')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-5.png)

{% highlight r %}
#*****************************************************************
# Test residual reversal strategy version 1
#*****************************************************************

# Models
models = lst()

# what signal are we testing
signal.type = "Rev.Last.Err"

# get quintiles for Reversion strategy
out = qis.make.quintiles(signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = F, position.tracker = NULL, next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-6.png)

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
  data = lst(prices = iif(model.name == 'SPY', spy, prices), data.source = 'yahoo')
  
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
  
  if(model.name == 'Rev.Last.Err.Spread'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Rev.Last.Err.Q1   |Rev.Last.Err.Q2   |Rev.Last.Err.Q3   |Rev.Last.Err.Q4   |Rev.Last.Err.Q5   |Rev.Last.Err.Spread |
|:-----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-------------------|
|Period      |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018   |
|Cagr        |23.03             |19.24             |16.61             |13.18             |15.26             |7.08                |
|AveRet      |23.06             |19.26             |16.63             |13.2              |15.28             |7.09                |
|Sharpe      |1.01              |1.01              |0.92              |0.75              |0.78              |0.77                |
|Volatility  |23.33             |19.33             |18.61             |18.78             |21.19             |9.5                 |
|AveTurnover |1805.91           |1870.74           |1838.36           |1858.98           |1872.82           |1839.37             |
|MaxDD       |-56.07            |-49.59            |-49.43            |-54.47            |-60.34            |-16.33              |
|AveDD       |-2.27             |-1.68             |-1.63             |-1.8              |-2.1              |-1.65               |
|VaR         |-1.99             |-1.7              |-1.64             |-1.73             |-1.98             |-0.84               |
|CVaR        |-3.49             |-2.92             |-2.81             |-2.87             |-3.24             |-1.3                |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Reversion strategies')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-7.png)

{% highlight r %}
#*****************************************************************
# Test residual reversal strategy version 2
#*****************************************************************

# Models
models = lst()

# what signal are we testing
signal.type = "Rev.Last.Err.Std"

# get quintiles for Reversion strategy
out = qis.make.quintiles(signals[[signal.type]][period.ends,], n.quantiles = 5, start.t =  37, high.minus.low = F, position.tracker = NULL,next.period.ret, signal.type, col.add.alpha(10))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-8.png)

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
  data = lst(prices = iif(model.name == 'SPY', spy, prices), data.source = 'yahoo')
  
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
  
  if(model.name == 'Rev.Last.Err.Std.Spread'){
    compare.models[[model.name]] =   models[[model.name]] 
  } 
}

# trim strategies
models = qis.bt.trim(models, dates  = "2003::")

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Rev.Last.Err.Std.Q1 |Rev.Last.Err.Std.Q2 |Rev.Last.Err.Std.Q3 |Rev.Last.Err.Std.Q4 |Rev.Last.Err.Std.Q5 |Rev.Last.Err.Std.Spread |
|:-----------|:-------------------|:-------------------|:-------------------|:-------------------|:-------------------|:-----------------------|
|Period      |Jan2003 - Aug2018   |Jan2003 - Aug2018   |Jan2003 - Aug2018   |Jan2003 - Aug2018   |Jan2003 - Aug2018   |Jan2003 - Aug2018       |
|Cagr        |20.8                |20.78               |18.03               |14.74               |13.02               |7.14                    |
|AveRet      |20.82               |20.8                |18.05               |14.76               |13.03               |7.15                    |
|Sharpe      |0.97                |1.05                |0.94                |0.8                 |0.71                |0.8                     |
|Volatility  |21.97               |19.95               |19.71               |19.47               |20                  |9.16                    |
|AveTurnover |1873.62             |1879.8              |1874.43             |1870.82             |1920.01             |1896.81                 |
|MaxDD       |-55.61              |-50.42              |-52.17              |-53.39              |-59.6               |-20.6                   |
|AveDD       |-1.94               |-1.82               |-1.73               |-1.82               |-2                  |-1.56                   |
|VaR         |-1.92               |-1.77               |-1.75               |-1.77               |-1.86               |-0.81                   |
|CVaR        |-3.32               |-3                  |-2.98               |-2.96               |-3.06               |-1.26                   |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Reversion strategies')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-9.png)

{% highlight r %}
#*****************************************************************
# Summary for reversion strategies
#*****************************************************************

# save strategies
save(compare.models, file = models.file)

# trim models
compare.models = qis.bt.trim(compare.models, dates  = "2003::")
#compare.models = qis.bt.trim(compare.models, dates  = "2009::")


# Strategy performance
print(qis.plot.strategy.sidebyside(compare.models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|            |Mom.1M.Spread     |Rev.1M.Spread     |Rev.Last.Err.Spread |Rev.Last.Err.Std.Spread |
|:-----------|:-----------------|:-----------------|:-------------------|:-----------------------|
|Period      |Jan2003 - Aug2018 |Jan2003 - Aug2018 |Jan2003 - Aug2018   |Jan2003 - Aug2018       |
|Cagr        |-3.72             |2.3               |7.08                |7.14                    |
|AveRet      |-3.72             |2.3               |7.09                |7.15                    |
|Sharpe      |-0.21             |0.24              |0.77                |0.8                     |
|Volatility  |13.61             |13.42             |9.5                 |9.16                    |
|AveTurnover |1846.57           |1846.57           |1839.37             |1896.81                 |
|MaxDD       |-52.89            |-29.02            |-16.33              |-20.6                   |
|AveDD       |-8.45             |-2.41             |-1.65               |-1.56                   |
|VaR         |-1.19             |-1.14             |-0.84               |-0.81                   |
|CVaR        |-2.09             |-1.95             |-1.3                |-1.26                   |
    




{% highlight r %}
# Plot backtests
layout(1)
qis.plot.strategy(compare.models, plotX = T, log = 'y', LeftMargin = 3, main = 'Reversion strategies')
mtext('Index Level', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-10.png)

{% highlight r %}
#*****************************************************************
# Plot Portfolio Turnovers
#****************************************************************** 
layout(1)
qis.barplot.with.labels(sapply(compare.models, qis.compute.turnover), 'Average Annual Portfolio Turnover', col = col.add.alpha(3))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-09-02-blitz-reversal/plot-2-11.png)


*(this report was produced on: 2018-09-15)*
