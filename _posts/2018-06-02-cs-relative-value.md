---
layout: post
title:  Credit Suisse Relative value factor
comments: false
---




Today, I will reuse a framework developed in [Fundamental data from ADVFN](https://sysresearcher.github.io/data-advfn) to get fundamenta data.
I will use this data to construct **Relative Value** for [Dow Jones Index](https://www.marketwatch.com/investing/index/djia) as defined by Credit Suisse here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)


Underlying measures of Relative Value (RV) are :

 1.  Sector Relative: Price / Trailing Sales (Current Spread vs. 5 Year Avg.)
 2.  Sector Relative: Price / Trailing Earnings (Current Spread vs. 5Yr Avg.)
 3.  Sector Relative: Price / Trailing Cash Flow (Current Spread vs. 5Yr Avg.)
 4.  Sector Relative: Price / Trailing Sales
 5.  Sector Relative: Price / Trailing Earnings
 6.  Sector Relative: Price / Trailing Cash Flow


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
# prices = qis.yahoo.data(tickers = tickers, file = prices.file, dates = '2010::', obj.type = 'xts', debug = T)


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
  
  #--------------------------------------------------------------
  # Data for Traditional and Relative Value   
  #--------------------------------------------------------------
  
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
# Traditional Value
#****************************************************************** 
factors$TV = list()
factor.names$TV = 'Traditional Value'

# Market capitalization (prices x shares outstanding)
MKVAL = prices * qis.apply(data, function(x) ifna.prev(x[, 'CSHO']))

# Price / Earnings
# Earnings per share (EPS) = (Net Income - Dividends on Preferred Stock) / Average Outstanding Shares.
EPS = qis.apply(data, function(x) ifna.prev(x[, 'EPS']))
factors$TV$EP = EPS / prices

# Price / Trailing Sales
SALE = qis.apply(data, function(x) ifna.prev(x[, 'SALE']))   
factors$TV$SP = SALE / MKVAL

# Price / Trailing Cash Flow
CFL = qis.apply(data, function(x) ifna.prev.next(x[, 'CFL']))
factors$TV$CFP = CFL / MKVAL

# Dividend Yield
DV.PS = qis.apply(data, function(x) ifna.prev.next(x[, 'DV.PS']))
factors$TV$DY = DV.PS / prices

# Price / Book Value        
CEQ = qis.apply(data, function(x) ifna.prev.next(x[, 'CEQ'])) # common equity
factors$TV$BP = CEQ / MKVAL

# Eliminate Price/Sales and Price/Cash Flow for financial firms
factors$TV$SP[, sectors == 'Financials'] = NA
factors$TV$CFP[, sectors == 'Financials'] = NA


#*****************************************************************
# Create Relative Value
#****************************************************************** 
factors$RV = list()
factor.names$RV = 'Relative Value'     

for(i in spl('EP,SP,CFP')) {
  factors$RV[[paste('r',i,sep='')]] = factors$TV[[i]] / qis.sector.mean(factors$TV[[i]], sectors)         
}

# spreads, 5 Year Avg = 60 months
for(i in spl('rEP,rSP,rCFP')) {
  # SMA will not work if ther are NA's
  tmp = ifna(factors$RV[[i]],0)
  factors$RV[[paste('s',i,sep='')]] = factors$RV[[i]] -  apply(tmp, 2, function(x) if(all(is.na(x))) x else SMA(x,60) )
}

# tmp = factors$TV$BP[ , order(names(factors$TV$BP))]
# tmp = factors$TV$EP[ , order(names(factors$TV$EP))]
# tmp = factors$TV$DY[ , order(names(factors$TV$DY))]
# tmp = factors$TV$CFP[ , order(names(factors$TV$CFP))]
# write.xts(tmp['2018-8'], "BP.csv")
# write.xts(tmp['2018-8'], "EP.csv")
# write.xts(tmp['2018-8'], "DY.csv")
# write.xts(tmp['2018-8'], "CFP.csv")
# write.xts(tmp['2018-8'], "junk.csv")

#*****************************************************************
# Create the overall Traditional Value factor 
#****************************************************************** 
# check missing data for financial firms
print(sapply(factors$RV, count))
{% endhighlight %}



|     |  rEP|  rSP| rCFP| srEP| srSP| srCFP|
|:----|----:|----:|----:|----:|----:|-----:|
|MMM  | 6240| 6240| 6240| 6240| 6240|  6240|
|AXP  | 6240|    0|    0| 6240|    0|     0|
|AAPL | 6264| 6264| 6264| 6245| 6245|  6245|
|BA   | 6240| 6240| 6240| 6240| 6240|  6240|
|CAT  | 6240| 6240| 6240| 6240| 6240|  6240|
|CVX  | 4481| 4481| 4481| 4481| 4481|  4481|
|CSCO | 6275| 6275| 6275| 6245| 6245|  6245|
|KO   | 6240| 6157| 6157| 6240| 6157|  6157|
|DIS  | 6264| 6264| 6264| 6245| 6245|  6245|
|DWDP |   78|   78|  257|   78|   78|   257|
|XOM  | 6240| 6240| 6240| 6240| 6240|  6240|
|GS   | 4892|    0|    0| 4892|    0|     0|
|HD   | 6268| 6268| 6268| 6245| 6245|  6245|
|IBM  | 6240| 6240| 6240| 6240| 6240|  6240|
|INTC | 6240| 6240| 6240| 6240| 6240|  6240|
|JNJ  | 6240| 6240| 6240| 6240| 6240|  6240|
|JPM  | 6240|    0|    0| 6240|    0|     0|
|MCD  | 6240| 6240| 6240| 6240| 6240|  6240|
|MRK  | 6240| 6240| 6240| 6240| 6240|  6240|
|MSFT | 6264| 6264| 6264| 6245| 6245|  6245|
|NKE  | 6284| 6284| 6284| 6245| 6245|  6245|
|PFE  | 6240| 6240| 6240| 6240| 6240|  6240|
|PG   | 6264| 6181| 6181| 6245| 6162|  6162|
|TRV  | 6240|    0|    0| 6240|    0|     0|
|UTX  | 6240| 6240| 6240| 6240| 6240|  6240|
|UNH  | 5974|    0| 6197| 5974|    0|  6197|
|VZ   | 6240| 6240| 6240| 6240| 6240|  6240|
|V    | 2634| 2634| 2802| 2634| 2634|  2802|
|WMT  | 6262| 6179| 6179| 6245| 6162|  6162|
|WBA  |  932|  849|  849|  932|  849|   849|
    




{% highlight r %}
#*****************************************************************
# Normalize and add Average factor
#******************************************************************
z.scored = lst()
for(j in names(factors)[2]) {
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
factor.names.components = ls(factors$RV)
factor.names.components.desc = spl('Average.RV,Sector.Price.To.Trailing.CashFlow,Sector.Price.To.Earnings,Sector.Price.To.Trailing.Sales,Spread.Sector.Price.To.Trailing.CashFlow,Spread.Sector.Price.To.Earnings,Spread.Sector.Price.To.Trailing.Sales')

#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  
  j = factor.names.components[i]
  x = as.vector(factors$RV[[j]][month.ends,])
  y = as.vector(next.month.ret)
  
  
  remove.ind = which(is.na(x) | is.nan(x) | is.infinite(x))
  
  x = x[-remove.ind]
  y = y[-remove.ind]
  
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
| 0.039489455587368348| 3.4511865431708997|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-1.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0091488331631613806| 0.74052151997406879|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-2.png)

|                rho|             tvalue|
|------------------:|------------------:|
| 0.0167991463389143| 1.4648186799143843|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-3.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.040843931591238211| 3.2335022757998804|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-4.png)

|                    rho|               tvalue|
|----------------------:|--------------------:|
| -0.0038685074293724098| -0.31292131390164912|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-5.png)

|                    rho|               tvalue|
|----------------------:|--------------------:|
| -0.0027681591810221999| -0.24121228579087561|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-6.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.036071316593685898| 2.8533167080579567|
    


![plot of chunk plot-2](/public/images/2018-06-02-cs-relative-value/plot-2-7.png)

The correlation between each of sub-factrors and Next Month Returns is small, but significantly different from zero doe some of factors.
The small correlation is not a surprise and is usual for this type of analysis



{% highlight r %}
#*****************************************************************
# Plot quintile charts for all Traditional Value factors
#*****************************************************************
#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  factor = factors$RV[[factor.names.components[i]]][month.ends,]
  qis.compute.quantiles(factor, next.month.ret, paste(factor.names.components[i], "(",factor.names.components.desc[i], ")")  )
}
{% endhighlight %}

![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-2.png)![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-6.png)![plot of chunk plot-3](/public/images/2018-06-02-cs-relative-value/plot-3-7.png)

There is tendency of quantile 5 (Q5) to outperform quantile 1 (Q1) in most cases. 
The relationship between quantiles is not perfect, but the spread between Q5-Q1 is positive exept dividend yield factor.




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
  out = qis.compute.quantiles(factors$RV$AVG[month.ends,], next.month.ret, plot=F) 
  
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


# Align models to start from the same time
#models = qis.bt.trim(models, dates = '2010::2018')

# Strategy performance
print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.default))
{% endhighlight %}



|           |Q.1               |Q.2               |Q.3               |Q.4               |Q.5               |Q.5.minus.Q.1     |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |
|Cagr       |12.1              |14.9              |15.64             |16.72             |21.17             |6.01              |
|AveRet     |11.41             |14.05             |14.74             |15.75             |19.93             |5.68              |
|Sharpe     |0.58              |0.77              |0.84              |0.85              |0.99              |0.41              |
|Volatility |23.06             |19.42             |18.5              |19.32             |20.54             |16.74             |
|MaxDD      |-63.68            |-45.72            |-43.68            |-39.83            |-48.94            |-57.52            |
|AveDD      |-3.38             |-2.49             |-2.45             |-2.49             |-2.58             |-3                |
|VaR        |-2.21             |-1.87             |-1.74             |-1.77             |-1.9              |-1.56             |
|CVaR       |-3.38             |-2.83             |-2.66             |-2.81             |-2.96             |-2.43             |
    




{% highlight r %}
# Plot backtests
par(mfrow=c(1, 1))
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Historical Backtest of Average RV Factor')
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2018-06-02-cs-relative-value/plot-4-1.png)


*(this report was produced on: 2018-08-22)*
