---
layout: post
title:  Credit Suisse Small size factor
comments: false
---




Today, I will reuse a framework developed in [Fundamental data from ADVFN](https://sysresearcher.github.io/data-advfn) to get fundamenta data.
I will use this data to construct **Small size** for [Dow Jones Index](https://www.marketwatch.com/investing/index/djia) as defined by Credit Suisse here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)


Underlying measures of Small size (SS) are :

 1. Log of Market Capitalization
 2. Log of Market Capitalization Cubed
 3. Log of Stock Price
 4. Log of Total Assets
 5. Log of Trailing 12-Month Sales


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
  
  
  # Common Shares Outstanding
  D$CSHO = qis.advfn.get.label('total common shares out', fund, fund.date)
  
  # Sales, exception not available for financial firms
  D$SALE = qis.advfn.get.label('total revenue', fund, fund.date, is.12m.rolling=T)
  
  
  D$AT = qis.advfn.get.label('total assets', fund, fund.date)
  
  
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
# Create Small Size
#******************************************************************         
factors$SS = list()
factor.names$SS = 'Small Size'     

# Market capitalization (prices x shares outstanding)
MKVAL = prices * qis.apply(data, function(x) ifna.prev(x[, 'CSHO']))


#Log of Market Capitalization
factors$SS$MC = log(MKVAL)

#Log of Market Capitalization Cubed
factors$SS$MC3 = log(MKVAL)^3

#Log of Stock Price
factors$SS$P = log(prices)

#Log of Total Assets
factors$SS$AT = log(qis.apply(data, function(x) ifna.prev(x[, 'AT'])))

#Log of Trailing-12-Month Sales
factors$SS$SALE = log(qis.apply(data, function(x) ifna.prev(x[, 'SALE'])))

# Flip sign
for(i in names(factors$SS)) factors$SS[[i]] = -factors$SS[[i]]


print(sapply(factors$SS, count))
{% endhighlight %}



|     |   MC|  MC3|    P|   AT| SALE|
|:----|----:|----:|----:|----:|----:|
|MMM  | 6240| 6240| 6304| 6240| 6240|
|AXP  | 6240| 6240| 6304| 6240|    0|
|AAPL | 6264| 6264| 6304| 6264| 6264|
|BA   | 6240| 6240| 6304| 6240| 6240|
|CAT  | 6240| 6240| 6304| 6240| 6240|
|CVX  | 4481| 4481| 6304| 4481| 4481|
|CSCO | 6275| 6275| 6304| 6275| 6275|
|KO   | 6240| 6240| 6304| 6240| 6240|
|DIS  | 6264| 6264| 6304| 6264| 6264|
|DWDP |  257|  257| 6304|  257|   78|
|XOM  | 6240| 6240| 6304| 6240| 6240|
|GS   | 5092| 5092| 5158| 5092|    0|
|HD   | 6268| 6268| 6304| 6268| 6268|
|IBM  | 6240| 6240| 6304| 6240| 6240|
|INTC | 6240| 6240| 6304| 6240| 6240|
|JNJ  | 6240| 6240| 6304| 6240| 6240|
|JPM  | 6240| 6240| 6304| 6240|    0|
|MCD  | 6240| 6240| 6304| 6240| 6240|
|MRK  | 6240| 6240| 6304| 6240| 6240|
|MSFT | 6264| 6264| 6304| 6264| 6264|
|NKE  | 6284| 6284| 6304| 6284| 6284|
|PFE  | 6240| 6240| 6304| 6240| 6240|
|PG   | 6264| 6264| 6304| 6264| 6264|
|TRV  | 6240| 6240| 6304| 6240|    0|
|UTX  | 6240| 6240| 6304| 6240| 6240|
|UNH  | 6197| 6197| 6304| 6197|    0|
|VZ   | 6240| 6240| 6304| 6240| 6240|
|V    | 2802| 2802| 2803| 2802| 2634|
|WMT  | 6262| 6262| 6304| 6262| 6262|
|WBA  |  932|  932| 6304|  932|  932|
    




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
factor.names.components = ls(factors$SS)
factor.names.components.desc = spl('Log.Total.Assets,Average.SS,Log.MarketCap,Log.MarketCap.Cubed,Log.Stock,Loge.12M.Sales')



#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  
  j = factor.names.components[i]
  x = as.vector(factors$SS[[j]][month.ends,])
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
| 0.061921134388529919| 5.4184920214091532|
    


![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-1.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.067486761303999096| 6.1542133206846215|
    


![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-2.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.088766295234022463| 7.7844537355978405|
    


![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-3.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.087172488582458629| 7.6436030673727835|
    


![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-4.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.097477072670684881| 8.9112440899148719|
    


![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-5.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.071672636931416248| 5.6894576764174003|
    


![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-6.png)

{% highlight r %}
#*****************************************************************
# Plot quintile charts for all Traditional Value factors
#*****************************************************************
#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  factor = factors$SS[[factor.names.components[i]]][month.ends,]
  qis.compute.quantiles(factor, next.month.ret, paste(factor.names.components[i], "(",factor.names.components.desc[i], ")")  )
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-7.png)![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-8.png)![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-9.png)![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-10.png)![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-11.png)![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-12.png)

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
  out = qis.compute.quantiles(factors$SS$AVG[month.ends,], next.month.ret, plot=F) 
  
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
|Cagr       |10                |12.68             |14.4              |15.39             |30.77             |18.48             |
|AveRet     |9.44              |11.96             |13.58             |14.51             |28.9              |17.41             |
|Sharpe     |0.56              |0.68              |0.74              |0.8               |1.34              |1.14              |
|Volatility |19.46             |19.21             |20.07             |19.27             |20.6              |15.12             |
|MaxDD      |-42.78            |-57.55            |-65.69            |-48.33            |-42.03            |-31.6             |
|AveDD      |-2.62             |-2.49             |-2.64             |-2.29             |-2.33             |-2.02             |
|VaR        |-1.88             |-1.81             |-1.91             |-1.81             |-1.89             |-1.4              |
|CVaR       |-2.83             |-2.79             |-2.94             |-2.77             |-2.92             |-2.11             |
    




{% highlight r %}
# Plot backtests
par(mfrow=c(1, 1))
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Historical Backtest of Average PM Factor')
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-07-cs-small-size/plot-2-13.png)


*(this report was produced on: 2018-08-22)*
