---
layout: post
title:  Credit Suisse Price reversal factor
comments: false
---




Today, I will reuse a framework developed in [Fundamental data from ADVFN](https://sysresearcher.github.io/data-advfn) to get fundamenta data.
I will use this data to construct **Price reversal** for [Dow Jones Index](https://www.marketwatch.com/investing/index/djia) as defined by Credit Suisse here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)


Underlying measures of Price reversal (PR) are :

 1. 5 Day Industry Relative Return
 2. 5 Day Money Flow / Volume
 3. 10 Day MACD - Signal Line
 4. 14 Day RSI (Relative Strength Indicator)
 5. 20 Day Stochastic
 6. 4 Week Industry Relative Return
 


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


# find week ends
week.ends      = endpoints(prices, 'weeks')
week.prices    = prices[week.ends,]
week.nperiods  = nrow(week.prices)

#*****************************************************************
# Create Price Reversal
#****************************************************************** 
factors$PR = list()
factor.names$PR = 'Price Reversal' 

#5 Day Industry Relative Return
factors$PR$r5DR = prices/mlag(prices, 5)
factors$PR$r5DR = factors$PR$r5DR / qis.sector.mean(factors$PR$r5DR, sectors)

#5 Day Money Flow / Volume
factors$PR$MFV = qis.apply(data, function(x) {
  MFI(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))), 5) / ifna.prev(Vo(x))
})

#10 Day MACD - Signal Line
factors$PR$MACD = qis.apply.matrix(prices, function(x) {
  temp=MACD(x, 10)
  temp[, 'macd'] - temp[, 'signal']
})      

#14 Day RSI (Relative Strength Indicator)
factors$PR$RSI = qis.apply.matrix(prices, RSI, 14)

#14 Day Stochastic
factors$PR$STOCH = qis.apply(data, function(x) {
  stoch(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))),14)[,'slowD']
})

#4 Week Industry Relative Return
factors$PR$rR4W = week.prices / mlag(week.prices,4)
factors$PR$rR4W = factors$PR$rR4W / qis.sector.mean(factors$PR$rR4W, sectors)

# Convert weekly to daily
temp = prices * NA
temp[week.ends,] = factors$PR$rR4W
factors$PR$rR4W = qis.apply.matrix(temp, function(x) ifna.prev(x))


# VOMO - Volume x Momentum
volume = qis.apply(data, function(x) ifna.prev(Vo(x)))
factors$PR$VOMO = (prices / mlag(prices,10) - 1) * qis.apply.matrix(volume, runMean, 22) / qis.apply.matrix(volume, runMean, 66)

# Flip sign
for(i in names(factors$PR)) factors$PR[[i]] = -factors$PR[[i]]


print(sapply(factors$PR, count))
{% endhighlight %}



|     | r5DR|  MFV| MACD|  RSI| STOCH| rR4W| VOMO|
|:----|----:|----:|----:|----:|-----:|----:|----:|
|MMM  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|AXP  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|AAPL | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|BA   | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|CAT  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|CVX  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|CSCO | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|KO   | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|DIS  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|DWDP | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|XOM  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|GS   | 5153| 5144| 5125| 5144|  5141| 5134| 5093|
|HD   | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|IBM  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|INTC | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|JNJ  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|JPM  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|MCD  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|MRK  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|MSFT | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|NKE  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|PFE  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|PG   | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|TRV  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|UTX  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|UNH  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|VZ   | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|V    | 2798| 2789| 2770| 2789|  2786| 2780| 2738|
|WMT  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
|WBA  | 6299| 6290| 6271| 6290|  6287| 6281| 6239|
    




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
factor.names.components = ls(factors$PR)
factor.names.components.desc = spl('Average.PM,10D.MACD,5D.Money.Flow.Volume,5D.Ind.Relative.Return,4W.Ind.Relative.Return,14D.RSI,14D.Stochastic,Volume.Momentum')



#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  
  j = factor.names.components[i]
  x = as.vector(factors$PR[[j]][month.ends,])
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
  #par(mar=c(4,4,2,1))             
  plot(x, y, pch=20, main = paste0('Correlation for ',factor.names.components.desc[i]), xlab = factor.names.components[i], ylab = 'Next Month Return')
  abline(lm(y ~ x), col='blue', lwd=2)
}
{% endhighlight %}



|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.034648573572887718| 3.1543430133768613|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-1.png)

|                  rho|            tvalue|
|--------------------:|-----------------:|
| 0.036183031929381652| 3.288042562560924|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-2.png)

|                 rho|             tvalue|
|-------------------:|------------------:|
| 0.01662710077463158| 1.5129098399121677|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-3.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.017159851520398881| 1.5614935240502055|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-4.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.018783499182154641| 1.7061902418853458|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-5.png)

|                  rho|            tvalue|
|--------------------:|-----------------:|
| 0.039931525219485522| 3.635790912312395|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-6.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.024973647985716229| 2.2727618811417165|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-7.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.048629174976783193| 4.4058169998587973|
    


![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-8.png)

{% highlight r %}
#*****************************************************************
# Plot quintile charts for all Traditional Value factors
#*****************************************************************
#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  factor = factors$PR[[factor.names.components[i]]][month.ends,]
  qis.compute.quantiles(factor, next.month.ret, paste(factor.names.components[i], "(",factor.names.components.desc[i], ")")  )
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-9.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-10.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-11.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-12.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-13.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-14.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-15.png)![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-16.png)

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
  out = qis.compute.quantiles(factors$PR$AVG[month.ends,], next.month.ret, plot=F) 
  
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
|Cagr       |11.06             |13.86             |20.04             |16.48             |21.03             |8.11              |
|AveRet     |10.44             |13.07             |18.87             |15.53             |19.8              |7.66              |
|Sharpe     |0.6               |0.75              |0.98              |0.84              |0.95              |0.53              |
|Volatility |19.77             |18.82             |19.54             |19.34             |21.48             |16.66             |
|MaxDD      |-55.42            |-51.79            |-44.05            |-45.82            |-48.46            |-36.64            |
|AveDD      |-2.92             |-2.14             |-2.35             |-2.6              |-2.51             |-4.17             |
|VaR        |-1.91             |-1.78             |-1.85             |-1.79             |-2.05             |-1.53             |
|CVaR       |-2.88             |-2.71             |-2.85             |-2.78             |-3.07             |-2.35             |
    




{% highlight r %}
# Plot backtests
par(mfrow=c(1, 1))
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Historical Backtest of Average PR Factor')
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-06-cs-price-reversal/plot-2-17.png)


*(this report was produced on: 2018-08-22)*
