---
layout: post
title:  Credit Suisse Profitability factor
comments: false
---




Today, I will reuse a framework developed in [Fundamental data from ADVFN](https://sysresearcher.github.io/data-advfn) to get fundamenta data.
I will use this data to construct **Profitability** for [Dow Jones Index](https://www.marketwatch.com/investing/index/djia) as defined by Credit Suisse here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)


Underlying measures of Profitability trends (PT) are :

 1. Sector Relative (Receivables+Inventories) / Sales
 2. Sector Relative Sales / Assets
 3. Overhead / Sales
 4. Earnings / Sales
 
 Underlying measures of Accelerating Sales (AS) are :

 1. 12 Month Sales Momemtum
 2. Change in 4Q Slope of Sales
 3. 3M Acceleration in 12M Change in Sales
 4. 6M Acceleration in 12M Change in Sales


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
  
  
  #--------------------------------------------------------------
  # Data for Profit Trends    
  #--------------------------------------------------------------
  RECT = qis.advfn.get.label('receivables', fund, fund.date)
  INVT = qis.advfn.get.label('inventories', fund, fund.date)
  
  D$AT = qis.advfn.get.label('total assets', fund, fund.date)
  XSGA = qis.advfn.get.label('Selling, General & Administrative (SG&A) Expense', fund, fund.date, is.12m.rolling=T)
  
  # Consecutive Quarters of Declines in (Receivables+Inventories) / Sales
  D$RS.CON.CHG = qis.count.consecutive.changes((RECT + INVT) / D$SALE, F)
  
  # Consecutive Qtrs of Positive Change in Trailing 12M Cash Flow / Sales
  D$CS.CON.CHG = qis.count.consecutive.changes(D$CFL/D$SALE)
  
  # Overhead = sales, general and administrative costs
  # Consecutive Quarters of Declines in Trailing 12 Month Overhead / Sales
  D$OS.CON.CHG = qis.count.consecutive.changes(XSGA/D$SALE, F)
  
  # (Industry Relative) Trailing 12 Month (Receivables+Inventories) / Sales
  D$RS = (RECT + INVT) / D$SALE
  
  # (Industry Relative) Trailing 12 Month Sales / Assets
  D$SA = D$SALE / D$AT
  
  # Trailing 12 Month Overhead / Sales
  D$OS = XSGA / D$SALE
  
  # Trailing 12 Month Earnings / Sales
  D$ES = D$EPS / D$SALE      
  
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
# Create Profit Trends
#****************************************************************** 
factors$PT = list()     
factor.names$PT = 'Profit Trends'  

for(i in spl('RS.CON.CHG,CS.CON.CHG,OS.CON.CHG,RS,SA,OS,ES')) {
  factors$PT[[i]] = qis.apply(data, function(x) ifna.prev.next(x[, i]))
}

#*****************************************************************
# Profit Trends (Relative)
#******************************************************************     
# relative 
for(i in spl('RS,SA')) {
  factors$PT[[paste('r',i,sep='')]] = factors$PT[[i]] / qis.sector.mean(factors$PT[[i]], sectors)
}  


print(sapply(factors$PT, count))
{% endhighlight %}



|     | RS.CON.CHG| CS.CON.CHG| OS.CON.CHG|   RS|   SA|   OS|   ES|  rRS|  rSA|
|:----|----------:|----------:|----------:|----:|----:|----:|----:|----:|----:|
|MMM  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|AXP  |       6304|       6304|       6304|    0|    0|    0|    0|    0|    0|
|AAPL |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|BA   |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|CAT  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|CVX  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|CSCO |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|KO   |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6221|
|DIS  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|DWDP |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|XOM  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|GS   |       6304|       6304|       6304|    0|    0|    0|    0|    0|    0|
|HD   |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|IBM  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|INTC |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|JNJ  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6252|
|JPM  |       6304|       6304|       6304|    0|    0|    0|    0|    0|    0|
|MCD  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|MRK  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6252|
|MSFT |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|NKE  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|PFE  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6252|
|PG   |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6221|
|TRV  |       6304|       6304|       6304|    0|    0|    0|    0|    0|    0|
|UTX  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|UNH  |       6304|       6304|       6304|    0|    0|    0|    0|    0|    0|
|VZ   |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|V    |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6304|
|WMT  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6221|
|WBA  |       6304|       6304|       6304| 6304| 6304| 6304| 6304| 6304| 6221|
    




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
factor.names.components = ls(factors$PT)
factor.names.components.desc = spl('Average.PT,Consecutive.PosChange.12M.Cashflow.Sales,Trailing.12M.Earnings.Sales,Trailing.12M.Overhead.Sales,Consecutive.Declines.12M.Overhead.Sales,Relative.12M.Receivables.Inventories.Sales,Relative.Consecutive.Decline.Receivables.Inventories.Sales,Consecutive.Decline.Receivables.Inventories.Sales,Relative.12M.Sales.Assets,12M.Sales.Assets')


#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  
  j = factor.names.components[i]
  x = as.vector(factors$PT[[j]][month.ends,])
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



|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0096581513619131906| 0.87877354612043523|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-1.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0079160792113795696| 0.72025527909700726|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-2.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.014656672577528011| 1.2189282163553696|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-3.png)

|                  rho|                 tvalue|
|--------------------:|----------------------:|
| -0.00010733396320204| -0.0089255145086192898|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-4.png)

|                   rho|               tvalue|
|---------------------:|--------------------:|
| -0.010633541708491529| -0.96753170345508144|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-5.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0015383399851258701| 0.12792308803540173|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-6.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0097585273298228595| 0.81152341413525675|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-7.png)

|                  rho|             tvalue|
|--------------------:|------------------:|
| 0.023925853327619161| 2.1774815699151406|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-8.png)

|                 rho|           tvalue|
|-------------------:|----------------:|
| 0.01844250841642767| 1.53187727415635|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-9.png)

|                 rho|             tvalue|
|-------------------:|------------------:|
| 0.01673605500851167| 1.3914030429833686|
    


![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-10.png)

{% highlight r %}
#*****************************************************************
# Plot quintile charts for all Traditional Value factors
#*****************************************************************
#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  factor = factors$PT[[factor.names.components[i]]][month.ends,]
  qis.compute.quantiles(factor, next.month.ret, paste(factor.names.components[i], "(",factor.names.components.desc[i], ")")  )
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-11.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-12.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-13.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-14.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-15.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-16.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-17.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-18.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-19.png)![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-20.png)

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
  out = qis.compute.quantiles(factors$PT$AVG[month.ends,], next.month.ret, plot=F) 
  
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
|Cagr       |12.95             |15.63             |16.16             |15.91             |19                |3.1               |
|AveRet     |12.21             |14.73             |15.23             |15                |17.89             |2.93              |
|Sharpe     |0.6               |0.81              |0.87              |0.86              |1.03              |0.25              |
|Volatility |24.01             |19.27             |18.25             |18.25             |17.45             |17.05             |
|MaxDD      |-67.77            |-43.54            |-50.58            |-40.18            |-34.87            |-48.26            |
|AveDD      |-2.66             |-2.23             |-2.27             |-2.58             |-2.4              |-4.88             |
|VaR        |-2.23             |-1.83             |-1.67             |-1.71             |-1.67             |-1.56             |
|CVaR       |-3.52             |-2.8              |-2.61             |-2.62             |-2.52             |-2.52             |
    




{% highlight r %}
# Plot backtests
par(mfrow=c(1, 1))
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Historical Backtest of Average PT Factor')
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-04-cs-profitability/plot-2-21.png)


*(this report was produced on: 2018-08-22)*
