---
layout: post
title:  Credit Suisse Historical growth factor
comments: false
---




Today, I will reuse a framework developed in [Fundamental data from ADVFN](https://sysresearcher.github.io/data-advfn) to get fundamenta data.
I will use this data to construct **Historical Growth** for [Dow Jones Index](https://www.marketwatch.com/investing/index/djia) as defined by Credit Suisse here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)


Underlying measures of Historical Growth (HG) are :

 1. 3 Year Average Annual Sales Growth
 2. 3 Year Average Annual Earnings Growth
 3. 12M Change in Cash Flow
 4. 4Q Slope of Cash Flow


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
  # Data for Historical Growth    
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
  
  
  # Consecutive Quarters of Positive Changes in Trailing 12 Month Cash Flow
  D$CFL.CON.CHG = qis.count.consecutive.changes(D$CFL, positive = T)
  
  # Consecutive Quarters of Positive Change in Quarterly Earnings
  D$EPS.CON.CHG = qis.count.consecutive.changes(D$EPS, positive = T)
  
  # 12 Month Change in Quarterly Cash Flow
  temp = qis.advfn.get.label('net cash from operating activities', fund, fund.date, cash.flow=T)
  D$CFL.CHG = temp / mlag(temp,4)
  
  # 3 Year Average Annual Sales Growth
  # D$SALE.3YR.GR = D$SALE
  # if(len( D$SALE) > 12) {
  #   if(!all(is.na(D$SALE))) D$SALE.3YR.GR = SMA(ifna(D$SALE / mlag(D$SALE,4) - 1,NA), n=12)
  # } else {
  #   D$SALE.3YR.GR[] = 0
  # }
  # 
  # # 3 Year Average Annual Earnings Growth
  # D$EPS.3YR.GR = D$SALE
  # if(len( D$EPS) > 12) {
  #   D$EPS.3YR.GR = SMA(D$EPS / mlag(D$EPS,4) - 1, 3*4)
  # } else {
  #   D$EPS.3YR.GR[] = 0
  # }
  # 
  
  # 12 Quarter Trendline in Trailing 12 Month Earnings
  D$EPS.TREND = D$EPS * NA
  if(len( D$EPS) > 12) {
    D$EPS.TREND[12:nperiods] = sapply(12:nperiods, function(i) beta.degree( ols(cbind(1,1:12), D$EPS[(i-12+1):i])$coefficients[2] ))
  } else {
    D$EPS.TREND[] = 0
  }
  
  
  # Slope of Trend Line Through Last 4 Quarters of Trailing 12M Cash Flows        
  D$CFL.TREND = D$CFL * NA
  D$CFL.TREND[4:nperiods] = sapply(4:nperiods, function(i) beta.degree(ols(cbind(1,1:4), D$CFL[(i-4+1):i])$coefficients[2]))
  
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
# Create Historical Growth
#****************************************************************** 
factors$HG = list()
factor.names$HG = 'Historical Growth'

for(i in spl('CFL.CON.CHG,EPS.CON.CHG,CFL.CHG,EPS.TREND,CFL.TREND')) {
#for(i in spl('CFL.CON.CHG,EPS.CON.CHG,CFL.CHG,SALE.3YR.GR,EPS.3YR.GR,EPS.TREND,CFL.TREND')) {
  factors$HG[[i]] = qis.apply(data, function(x) ifna.prev.next(x[, i]))
}

print(sapply(factors$HG, count))
{% endhighlight %}



|     | CFL.CON.CHG| EPS.CON.CHG| CFL.CHG| EPS.TREND| CFL.TREND|
|:----|-----------:|-----------:|-------:|---------:|---------:|
|MMM  |        6304|        6304|    6304|      6304|      6304|
|AXP  |        6304|        6304|    6304|      6304|      6304|
|AAPL |        6304|        6304|    6304|      6304|      6304|
|BA   |        6304|        6304|    6304|      6304|      6304|
|CAT  |        6304|        6304|    6304|      6304|      6304|
|CVX  |        6304|        6304|    6304|      6304|      6304|
|CSCO |        6304|        6304|    6304|      6304|      6304|
|KO   |        6304|        6304|    6304|      6304|      6304|
|DIS  |        6304|        6304|    6304|      6304|      6304|
|DWDP |        6304|        6304|       0|      6304|         0|
|XOM  |        6304|        6304|    6304|      6304|      6304|
|GS   |        6304|        6304|    6304|      6304|      6304|
|HD   |        6304|        6304|    6304|      6304|      6304|
|IBM  |        6304|        6304|    6304|      6304|      6304|
|INTC |        6304|        6304|    6304|      6304|      6304|
|JNJ  |        6304|        6304|    6304|      6304|      6304|
|JPM  |        6304|        6304|    6304|      6304|      6304|
|MCD  |        6304|        6304|    6304|      6304|      6304|
|MRK  |        6304|        6304|    6304|      6304|      6304|
|MSFT |        6304|        6304|    6304|      6304|      6304|
|NKE  |        6304|        6304|    6304|      6304|      6304|
|PFE  |        6304|        6304|    6304|      6304|      6304|
|PG   |        6304|        6304|    6304|      6304|      6304|
|TRV  |        6304|        6304|    6304|      6304|      6304|
|UTX  |        6304|        6304|    6304|      6304|      6304|
|UNH  |        6304|        6304|    6304|      6304|      6304|
|VZ   |        6304|        6304|    6304|      6304|      6304|
|V    |        6304|        6304|    6304|      6304|      6304|
|WMT  |        6304|        6304|    6304|      6304|      6304|
|WBA  |        6304|        6304|    6304|      6304|      6304|
    




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
factor.names.components = ls(factors$HG)
factor.names.components.desc = spl('Average.RV,12M.Change.Quaterly.CashFlow,Consecutive.PosChange.Quaterly.CashFlow,Slope.Trendline.12M.CashFlows,Consecutive.PosChange.Quaterly.Earning,12M.Trendline.12M.Earnings')


#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  
  j = factor.names.components[i]
  x = as.vector(factors$HG[[j]][month.ends,])
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



|                 rho|             tvalue|
|-------------------:|------------------:|
| 0.01316339719448693| 1.1977559210581918|
    


![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-1.png)

|                     rho|                tvalue|
|-----------------------:|---------------------:|
| -0.00084941254448875998| -0.075540458239217748|
    


![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-2.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0049237309379305096| 0.44798377789624838|
    


![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-3.png)

|                   rho|              tvalue|
|---------------------:|-------------------:|
| 0.0084512772577605508| 0.75569594842908516|
    


![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-4.png)

|                   rho|             tvalue|
|---------------------:|------------------:|
| -0.011473484755800531| -1.043966688864951|
    


![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-5.png)

|                  rho|              tvalue|
|--------------------:|-------------------:|
| -0.01126299602181809| -1.0248119707509062|
    


![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-6.png)

{% highlight r %}
#*****************************************************************
# Plot quintile charts for all Traditional Value factors
#*****************************************************************
#layout(matrix(1:8,nc=2))
for(i in 1:len(factor.names.components)) {
  factor = factors$HG[[factor.names.components[i]]][month.ends,]
  qis.compute.quantiles(factor, next.month.ret, paste(factor.names.components[i], "(",factor.names.components.desc[i], ")")  )
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-7.png)![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-8.png)![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-9.png)![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-10.png)![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-11.png)![plot of chunk plot-2](/public/images/2018-06-03-cs-historical-growth/plot-2-12.png)

There is tendency of quantile 5 (Q5) to outperform quantile 1 (Q1) in most cases. 
The relationship between quantiles is not perfect, but the spread between Q5-Q1 is positive.


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
  out = qis.compute.quantiles(factors$HG$AVG[month.ends,], next.month.ret, plot=F) 
  
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
|Cagr       |13.61             |16.66             |16.38             |13.02             |20.21             |3.56              |
|AveRet     |12.83             |15.7              |15.44             |12.28             |19.03             |3.36              |
|Sharpe     |0.68              |0.82              |0.85              |0.73              |1.05              |0.29              |
|Volatility |20.79             |20.3              |19.05             |17.97             |18.1              |15.68             |
|MaxDD      |-58.44            |-54.81            |-36.78            |-49.63            |-37.69            |-55.34            |
|AveDD      |-2.88             |-2.29             |-2.63             |-2.37             |-2.21             |-2.95             |
|VaR        |-1.9              |-1.91             |-1.83             |-1.67             |-1.7              |-1.52             |
|CVaR       |-2.98             |-2.98             |-2.79             |-2.61             |-2.58             |-2.34             |
    




{% highlight r %}
# Plot backtests
par(mfrow=c(1, 1))
qis.plot.strategy(models, plotX = T, log = 'y', LeftMargin = 3, main = 'Historical Backtest of Average HG Factor')
mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2018-06-03-cs-historical-growth/plot-3-1.png)


*(this report was produced on: 2018-08-22)*
