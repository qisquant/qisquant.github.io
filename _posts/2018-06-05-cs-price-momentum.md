---
layout: post
title:  Credit Suisse Price momentum factor
comments: false
---




Today, I will reuse a framework developed in [Fundamental data from ADVFN](https://sysresearcher.github.io/data-advfn) to get fundamenta data.
I will use this data to construct **Price momentum** for [Dow Jones Index](https://www.marketwatch.com/investing/index/djia) as defined by Credit Suisse here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)


Underlying measures of Price momentum (PM) are :

 1. Slope of 52 Week Trend Line (20 Day Lag)
 2. Percent Above 52 Week Low (20 Day Lag)
 3. 4/52 Week Price Oscillator (20 Day Lag)
 4. 39 Week Return (20 Day Lag)
 5. 52 Week Volume Price Trend (20 Day Lag)
 


{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
load.fundamentals = F
prices.file        = 'dj30.Rdata'
fundamentals.file  = 'dj30.fundamental.Rdata'
dates.range        = '1995::'

#*****************************************************************
# Get tickers
#*****************************************************************
tickers          = qis.dj30.components()
tickers.exchange = paste(iif(nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')


#*****************************************************************
# Find Sectors for each company given http://www.sectorspdr.com/sectorspdr/
#****************************************************************** 
sector.map = qis.sector.universe('spdr')
# map our universe against sectors universe
sectors        = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers

#*****************************************************************
# Get historical prices
#*****************************************************************
data = qis.yahoo.data(tickers = tickers, file = prices.file, dates = dates.range, obj.type = 'env', debug = F)

#*****************************************************************
# Get fundamental data
#*****************************************************************
if(load.fundamentals) {
  fundamentals <- new.env()
  for(i in 1:len(tickers)) {
    if(is.null(fundamentals[[tickers[i]]])) {
      cat(tickers[i],'\n')
      fundamentals[[tickers[i]]] = qis.advfn.fund.data(tickers.exchange[i], 100)
    }
  }
  save(fundamentals, file = fundamentals.file)
} else {
  load(file = fundamentals.file)
}


#*****************************************************************
# Create factors
#*****************************************************************
D = list()  # all data will be here
for(i in tickers) {
  
  # get fundamental info related to i ticker
  fund      = fundamentals[[i]]
  
  # get historical reporting dates
  fund.date = qis.advfn.get.dates(fund)
  
  # number of reporting dates
  nperiods  = ncol(fund)
  
  
  # Earnings per Share        
  D$EPS = qis.advfn.get.label('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
  
  # Sales, exception not available for financial firms
  D$SALE = qis.advfn.get.label('total revenue', fund, fund.date, is.12m.rolling=T)
  
  # Common Shares Outstanding
  D$CSHO = qis.advfn.get.label('total common shares out', fund, fund.date)
  
  # Common Equity
  D$CEQ = qis.advfn.get.label('total equity', fund, fund.date)
  
  # Dividends
  D$DV.PS = qis.advfn.get.label('dividends paid per share', fund, fund.date, is.12m.rolling=T)
  
  # Cash Flow, exception not available for financial firms
  D$CFL = qis.advfn.get.label('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)

  
  # add computed factors to data
  data[[i]] = merge(data[[i]], as.xts(abind(D,along=2), fund.date))    
}

qis.prep(data, align='keep.all', dates = dates.range)


#*****************************************************************
# Clean prices
#****************************************************************** 
prices = data$prices
prices = qis.apply.matrix(prices, function(x) ifna.prev(x))

# create factors
factors      = list()
factor.names = list()   


#*****************************************************************
# Create Price Momentum
#****************************************************************** 
factors$PM      = list()
factor.names$PM = 'Price Momentum' 

# find week ends
week.ends      = endpoints(prices, 'weeks')
week.prices    = prices[week.ends,]
week.nperiods  = nrow(week.prices)

#Slope of 52 Week Trend Line
factors$PM$S52W.TREND = qis.apply.matrix(week.prices, function(x) {
  c(rep(NA,51), sapply(52:week.nperiods, function(i) beta.degree( ols(cbind(1,1:52), x[(i - 52 + 1):i])$coefficients[2]) ) )
  })


#4/52 Week Price Oscillator
factors$PM$PP04.52W = qis.apply.matrix(week.prices, EMA, 4) / qis.apply.matrix(week.prices, EMA, 52)

#39 Week Return
factors$PM$R39W = week.prices / mlag(week.prices, 39)

#51 Week Volume Price Trend
# compute weekly volume
temp = qis.apply(data, function(x) cumsum(ifna(Vo(x),0)))
temp = temp[week.ends,]
week.volume = temp - mlag(temp)     
temp = (week.prices - mlag(week.prices)) * week.volume
factors$PM$VPT51W = qis.apply.matrix(temp, runSum, 51)

# Convert weekly to daily
for(i in 1:len(factors$PM)) {
  temp = prices * NA
  temp[week.ends,] = factors$PM[[i]]
  factors$PM[[i]] = qis.apply.matrix(temp, function(x) ifna.prev.next(x))
}

# Percent Above 260 Day Low
factors$PM$P260LOW = prices / qis.apply.matrix(prices, runMin, 260)

# Flip sign
for(i in names(factors$PM)) factors$PM[[i]] = -factors$PM[[i]]


print(sapply(factors$PM, count))
{% endhighlight %}



|     | S52W.TREND| PP04.52W| R39W| VPT51W| P260LOW|
|:----|----------:|--------:|----:|------:|-------:|
|MMM  |       6304|     6304| 6304|   6304|    6045|
|AXP  |       6304|     6304| 6304|   6304|    6045|
|AAPL |       6304|     6304| 6304|   6304|    6045|
|BA   |       6304|     6304| 6304|   6304|    6045|
|CAT  |       6304|     6304| 6304|   6304|    6045|
|CVX  |       6304|     6304| 6304|   6304|    6045|
|CSCO |       6304|     6304| 6304|   6304|    6045|
|KO   |       6304|     6304| 6304|   6304|    6045|
|DIS  |       6304|     6304| 6304|   6304|    6045|
|DWDP |       6304|     6304| 6304|   6304|    6045|
|XOM  |       6304|     6304| 6304|   6304|    6045|
|GS   |       6304|     6304| 6304|   6304|    4899|
|HD   |       6304|     6304| 6304|   6304|    6045|
|IBM  |       6304|     6304| 6304|   6304|    6045|
|INTC |       6304|     6304| 6304|   6304|    6045|
|JNJ  |       6304|     6304| 6304|   6304|    6045|
|JPM  |       6304|     6304| 6304|   6304|    6045|
|MCD  |       6304|     6304| 6304|   6304|    6045|
|MRK  |       6304|     6304| 6304|   6304|    6045|
|MSFT |       6304|     6304| 6304|   6304|    6045|
|NKE  |       6304|     6304| 6304|   6304|    6045|
|PFE  |       6304|     6304| 6304|   6304|    6045|
|PG   |       6304|     6304| 6304|   6304|    6045|
|TRV  |       6304|     6304| 6304|   6304|    6045|
|UTX  |       6304|     6304| 6304|   6304|    6045|
|UNH  |       6304|     6304| 6304|   6304|    6045|
|VZ   |       6304|     6304| 6304|   6304|    6045|
|V    |       6304|     6304| 6304|   6304|    2544|
|WMT  |       6304|     6304| 6304|   6304|    6045|
|WBA  |       6304|     6304| 6304|   6304|    6045|
    




{% highlight r %}
#*****************************************************************
# Normalize and add Average factor
#******************************************************************
z.scored = lst()
for(j in names(factors)) {
  # convert to z scores cross sectionaly each factor
  z.scored[[j]]    = qis.normalize.normal(factors[[j]])
  z.scored[[j]]    = qis.add.average.factor(z.scored[[j]])
  # AVG will have mean of z.scored factros. the rest of factors we keep orifinal.
  factors[[j]]$AVG =  z.scored[[j]]$AVG
}

# find month ends
month.ends       = endpoints(prices, 'months')
prices.month.end = prices[month.ends,]

# compute returns
ret            = prices.month.end / mlag(prices.month.end) - 1
next.month.ret = mlag(ret, -1)


#*****************************************************************
# Correlation Analysis of components
#*****************************************************************
factor.names.components = ls(factors$PM)
factor.names.components.desc = spl('Average.PM,Pct.Above.260DayLow,4-52.Week.Price.Oscillator,39.Week.Return,51.Week.Volume.Price.Trend')


#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  
  j = factor.names.components[i]
  x = as.vector(factors$PM[[j]][month.ends,])
  y = as.vector(next.month.ret)
  
  
  remove.ind = which(is.na(x) | is.nan(x) | is.infinite(x))
  
  x = iif(len(remove.ind) > 0,x[-remove.ind],x)
  y = iif(len(remove.ind) > 0,y[-remove.ind],y)
  
  # estimated correlation
  rho = cor.test(x, y, use = 'complete.obs', method = 'pearson')$estimate

  # t-value
  tvalue = cor.test(x, y, use = 'complete.obs', method = 'pearson')$statistic
  
  # print stat
  out  = data.frame(rho = rho, tvalue = tvalue); rownames(out) = NULL
  print(out)
  
  # Plot
  plot(x, y, pch=20, main = paste0('Correlation for ',factor.names.components.desc[i]), xlab = factor.names.components[i], ylab = 'Next Month Return')
  abline(lm(y ~ x), col='blue', lwd=2)
}
{% endhighlight %}



|                    rho|              tvalue|
|----------------------:|-------------------:|
| -0.0032793352179895999| -0.2983670492941794|
    


![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-1.png)

|                  rho|              tvalue|
|--------------------:|-------------------:|
| -0.03027109393897584| -2.6997826002929943|
    


![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-2.png)

|                  rho|              tvalue|
|--------------------:|-------------------:|
| -0.01207952287028647| -1.0991176418758724|
    


![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-3.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| -0.027277611923007351| -2.4827368478386949|
    


![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-4.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.019853990110664941| 1.8067419335518533|
    


![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-5.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0074804814836746903| 0.68061954239054012|
    


![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-6.png)

{% highlight r %}
#*****************************************************************
# Plot quintile charts for all Traditional Value factors
#*****************************************************************
#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  factor = factors$PM[[factor.names.components[i]]][month.ends,]
  qis.compute.quantiles(factor, next.month.ret, paste(factor.names.components[i], "(",factor.names.components.desc[i], ")")  )
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-7.png)![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-8.png)![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-9.png)![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-10.png)![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-11.png)![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-12.png)

{% highlight r %}
#*****************************************************************
# Backtest quantiles and quantile spread
#****************************************************************** 

# Model's names
model.names = c(paste('Q.',1:5,sep=''), 'Q.5.minus.Q.1')

# Models
models = lst()

# Test quantiles
for(i in 1:len(model.names)) {
  
  model.name = model.names[i]
  
  # create config
  config = lst(); config[[model.name]] = yaml.load_file('input.yaml')$input
  
  # Create data objects
  data = lst(prices = prices, data.source = 'yahoo')
  
  # Prepare for backtest
  data   = qis.prep.with.config(data,config)
  
  # Create weights
  weights  = data$prices; weights[] = NA
  
  # get quintiles
  out = qis.compute.quantiles(factors$PM$AVG[month.ends,], next.month.ret, plot=F) 
  
  if(model.name == 'Q.5.minus.Q.1'){
    weights[month.ends,] = iif(out$quantiles == 5, out$weights, iif(out$quantiles == 1, -out$weights, 0))
  } else {
    # use only spesific quintile
    weights[month.ends,] = iif(out$quantiles == i, out$weights, 0)
  }
  
  #TDD dates
  tdd.index = month.ends[1:(len(month.ends)-2)]
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



|           |Q.1               |Q.2               |Q.3               |Q.4               |Q.5               |Q.5.minus.Q.1     |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |
|Cagr       |18.79             |17.41             |11.33             |15.2              |16.59             |-3.38             |
|AveRet     |17.7              |16.4              |10.69             |14.33             |15.64             |-3.2              |
|Sharpe     |0.9               |0.89              |0.65              |0.81              |0.77              |-0.07             |
|Volatility |20.53             |19.2              |17.98             |18.81             |22.18             |19.83             |
|MaxDD      |-52.99            |-36.61            |-50.99            |-46.12            |-55.62            |-72.07            |
|AveDD      |-2.61             |-2.41             |-2.08             |-2.43             |-3.03             |-24.92            |
|VaR        |-2.04             |-1.84             |-1.76             |-1.76             |-1.93             |-1.89             |
|CVaR       |-2.98             |-2.8              |-2.6              |-2.74             |-3.18             |-2.7              |
    




{% highlight r %}
# Plot backtests
par(mfrow=c(1, 1))
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Historical Backtest of Average PM Factor')
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-05-cs-price-momentum/plot-2-13.png)


*(this report was produced on: 2018-08-22)*
