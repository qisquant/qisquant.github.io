---
layout: post
title: Composite Factor -  Asia
comments: true
---




Today I would like to construct long-short composite factors using Asian universe of stocks.

Universe of stocks:  large and mid cap names with approximately 150 stocks.

Few links to the previous posts:

 - [Momentum Factor - Asia](https://qisresearch.github.io/momentum-ap)
 - [Volatility Factor - Asia](https://qisresearch.github.io/volatility-ap)
 - [Size Factor - Asia](https://qisresearch.github.io/size-ap)
 - [Value Factor - Asia](https://qisresearch.github.io/value-ap)
 - [Quality Factor - Asia](https://qisresearch.github.io/quality-ap)

Factors: 

 1.  Mom.12m.1m (12m rolling returns with skipping most recent month)
 2.  Vol.6m     (6m rolling volatility)
 3.  Log.Mcap   (log of market capitalization)
 4.  OCF        (Operating cash flow)
 5.  PE         (Price to earning ratio)
 6.  BP         (Book to price ratio)
 7.  DVD        (Dividend yield)
 8.  ROE        (Return on equity)
 9.  OCFB       (Operating cash flow to book price)
 
Signal:
 
 1. Each factor factor Z-scored on [GICS](https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard) levels.
 2. Outliers winsorized.
 3. Buy high composite signal, sell low composite signal.

Signal variations :

 1. All factors weighted equally.                                              (Composite.SA)
 2. All factors weighted equally with net sum of weights unbounded from above. (Composite.SAU)
 3. All themes weighted equally (Momentum(1 factor), Volatility(1 factor), Size(1 factor), Value(4 factors), Quality(2 factors))   (Composite.TA)

Strategy constrains:
 
 1. Single name weight +-3%.
 2. Overall leverage kept at 200%.
 3. Market beta neutrality at rebalance.
 4. Compute signal every month end.
 5. Execute trades on first business day of the month.




{% highlight r %}
# run backtest
run.backtest  = F

# compute strategy
if(run.backtest) {

  # Models
  models = lst()
  
  # get all config for strategies
  config.files = list.files("config/", pattern =".yaml")
  
  # Loop over each strategy
  for(i in 1:len(config.files)) {
    
    # load configuration
    config  = yaml.load_file(paste0("config/",config.files[i])); strategy.name = ls(config)
    
    # update config with appropriate directories
    config[[strategy.name]]$qis.data = paste0(getwd(),"/",config[[strategy.name]]$index, "/")
    config[[strategy.name]]$qis.out  = paste0(getwd(),"/out/") 
    
    # process data
    data = process.data(config)
    
    # create evolution, trading schedules
    data = qis.ef.schedule(data,config)
    
    # Compute daily and forward returns
    data  = qis.ef.returns(data,config)
    
    # Adjust tracking for corporate actions.
    data = qis.ef.corpact.track(data,config)
    
    # adjust for factor coverage
    data = qis.ef.adjust.for.factor.coverage(data,config)
    
    # Create signal and do clean-ups
    data = qis.ef.signal(data,config)
    
    # Run Optimization
    data = qis.ef.default.lp(config,data)
    
    # Create  index
    data = qis.ef.index(data,config)
    
    # compute actual weights
    data = qis.ef.compute.actual.weights(data,config)
    
    # Compute market exposures
    data = qis.compute.beta.exposure(data, config)
    
    # index level
    index = data[[strategy.name]]$index
    
    # create model
    models[[strategy.name]] = lst(ret    = ifna(index / mlag(index) -1,0),
                                  equity = index,
                                  weight = make.xts(data$store[[strategy.name]]$w.actual, data$dates),
                                  beta   = data$market.betas)
  }
  # save strategy
  save(models, file = "models.Rdata")
  
} else {
  
  # load strategies
  load(file = paste0("models.Rdata"))
  
  # Scale strategy levels
  for(model.name in ls(models)){
    models[[model.name]]$equity = scale.one(models[[model.name]]$equity)
  }
  #*****************************************************************
  # Strategies statistics
  #*****************************************************************
  print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.ms))

  #*****************************************************************
  # Plot Backtests
  #*****************************************************************
  qis.plot.strategy(models, plotX = T, log = '', LeftMargin = 3, main = 'Composite strategies', copyright = copyright)
  mtext('Strategy Performance', side = 2, line = 1)
  
  #*****************************************************************
  # Plot Portfolio Turnovers
  #*****************************************************************
  qis.barplot.with.labels(sapply(models, qis.compute.turnover), 'Annual Portfolio Turnover', col = col.add.alpha(3))
  
  #*****************************************************************
  # Exposure statistics
  #***************************************************************** 
  for(model.name in ls(models)){
    
    print(paste0(model.name, " strategy : "))
    
    # plot monthly returns
    print(qis.plot.monthly.table(models[[model.name]]$equity, make.plot = F, model.name))
    
    # plot market beta exposure
    qis.plot.xts(models[[model.name]]$beta, main = paste0(model.name, " market beta exposure"), copyright = copyright, ylim = c(-1.0, 1.0))
    
    # plot # of names in the portfolio
    tmp = qis.compute.names.stat(models[[model.name]]$weight, side = 'all')
    tmp  = iif(tmp == 0, NA, tmp)
    tmp  = qis.apply.matrix(tmp, function(x) ifna.prev.next(x))
    qis.plot.xts(tmp['2006-03::'], main = paste0(model.name, " # of names in the portfolio"), copyright = copyright, ylim=c(10,200))
    
    # plot running maximum and minimum weight in portfolio
    tmp1 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'long', type = 'max', single = T); names(tmp1) = 'Max.Long'
    tmp2 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'short', type = 'min', single = T);names(tmp2) = 'Min.Short'
    tmp  = merge(tmp1,tmp2)
    tmp  = iif(tmp == 0, NA, tmp)
    tmp  = qis.apply.matrix(tmp, function(x) ifna.prev.next(x))
    qis.plot.xts(tmp, main = paste0(model.name, " single name exposure"), copyright = copyright,  ylim=c(-0.05,0.05))
    
    # plot long,short,net and gross exposure
    tmp1 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'net'); names(tmp1) = 'Net exposure'
    tmp2 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'gross'); names(tmp2) = 'Gross exposure'
    tmp3 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'long'); names(tmp3) = 'Long exposure'
    tmp4 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'short'); names(tmp4) = 'Short exposure'
    tmp  = merge(tmp1,tmp2,tmp3,tmp4)
    tmp  = iif(tmp == 0, NA, tmp)
    tmp  = qis.apply.matrix(tmp, function(x) ifna.prev.next(x))
    qis.plot.xts(tmp['2006-03::'], main = paste0(model.name, " daily exposures"), copyright = copyright, ylim=c(-2,3.0))
  }
}
{% endhighlight %}



|           |Composite.SAU     |Composite.SA      |Composite.TA      |
|:----------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2006 - Nov2018 |Jan2006 - Nov2018 |Jan2006 - Nov2018 |
|Ave.Ret    |2.14              |0.49              |2.1               |
|Sharpe     |0.34              |0.11              |0.3               |
|Volatility |7.07              |7.04              |7.86              |
|MarketBeta |0                 |-0.01             |-0.01             |
|Turnover   |693.13            |697.12            |697.62            |
|MaxDD      |-15.16            |-22.18            |-16.76            |
|VaR        |-0.69             |-0.68             |-0.76             |
    


![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-2.png)

Composite.SA strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |0.4  |-1.7 |-0.8 |-1.1 |0.4  |0.5  |-2.7 |-1.6 |3.5  |-1.2 |-0.6 |-5.0  |-8.0  |
|2007 |0.1  |2.9  |-0.1 |1.3  |4.4  |2.4  |2.5  |0.2  |-4.7 |-3.2 |-2.3 |-3.3 |-0.2  |-14.3 |
|2008 |-0.4 |-1.0 |3.7  |1.9  |-3.8 |-1.3 |-0.4 |4.6  |2.9  |-6.8 |-0.7 |0.6  |-1.4  |-11.7 |
|2009 |3.0  |-7.6 |1.3  |1.3  |6.3  |4.5  |2.6  |2.3  |-2.1 |0.3  |-3.2 |0.6  |9.0   |-11.0 |
|2010 |-0.5 |3.7  |0.0  |2.0  |0.1  |0.2  |-1.2 |-1.9 |1.0  |-0.7 |0.6  |4.0  |7.4   |-5.2  |
|2011 |-1.0 |3.1  |2.5  |0.6  |2.1  |0.9  |-1.4 |-1.0 |-0.9 |-1.7 |-1.2 |0.7  |2.5   |-8.3  |
|2012 |-1.3 |-0.8 |0.7  |2.0  |2.3  |1.2  |1.8  |0.9  |-1.2 |-1.4 |0.3  |-0.6 |3.8   |-5.1  |
|2013 |1.0  |2.0  |0.3  |0.2  |-4.4 |0.0  |0.3  |-0.2 |1.4  |2.6  |-0.8 |0.3  |2.5   |-6.9  |
|2014 |0.0  |0.7  |0.9  |-1.3 |-1.7 |0.4  |1.5  |-0.9 |1.4  |-0.7 |-1.3 |0.4  |-0.7  |-4.0  |
|2015 |-1.8 |-2.4 |-1.7 |-0.6 |0.7  |0.1  |-2.3 |0.1  |-2.1 |-1.0 |1.0  |-2.8 |-12.0 |-12.8 |
|2016 |0.4  |-0.2 |-0.3 |3.0  |-1.6 |2.0  |0.0  |4.9  |0.0  |-1.3 |-1.7 |4.1  |9.5   |-4.2  |
|2017 |-0.8 |1.0  |-1.0 |-1.4 |0.2  |0.0  |1.4  |-0.1 |-0.3 |-0.7 |-2.5 |-0.9 |-5.2  |-6.0  |
|2018 |-0.5 |0.8  |-1.9 |-1.9 |-0.4 |1.3  |1.9  |-2.3 |0.5  |1.8  |-0.5 |     |-1.3  |-5.5  |
|Avg  |-0.1 |0.2  |0.2  |0.5  |0.2  |0.9  |0.5  |0.3  |-0.5 |-0.7 |-1.0 |0.2  |0.7   |-7.9  |
    


![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-6.png)

Composite.SAU strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |0.5  |-1.8 |-0.7 |-1.2 |0.5  |1.1  |-2.3 |-0.8 |1.2  |-0.3 |0.9  |-2.8 |-5.9  |
|2007 |2.0  |2.2  |-0.3 |2.5  |3.1  |2.1  |3.4  |-0.5 |-3.3 |-2.4 |-1.7 |-1.5 |5.5  |-11.2 |
|2008 |1.0  |-0.3 |3.9  |1.9  |-3.4 |-1.8 |1.2  |4.6  |2.6  |-7.7 |0.7  |-0.3 |1.7  |-12.3 |
|2009 |3.2  |-6.5 |1.1  |1.1  |6.7  |4.2  |2.4  |2.8  |-1.9 |0.2  |-3.6 |0.7  |10.1 |-9.4  |
|2010 |-1.1 |3.5  |0.0  |2.0  |0.4  |0.5  |-1.4 |-1.0 |2.5  |-0.7 |1.0  |3.7  |9.7  |-4.8  |
|2011 |-0.7 |2.5  |2.2  |1.1  |2.4  |0.7  |-1.2 |-0.6 |-0.3 |-2.2 |-0.1 |0.6  |4.6  |-6.6  |
|2012 |-1.5 |-0.5 |0.5  |2.2  |1.7  |2.0  |2.8  |1.1  |-1.1 |-1.3 |1.2  |-0.6 |6.5  |-4.5  |
|2013 |1.1  |2.0  |1.0  |1.2  |-4.8 |0.1  |-0.2 |-0.6 |1.4  |2.6  |-0.8 |0.3  |3.2  |-7.1  |
|2014 |-0.1 |0.8  |1.3  |-1.1 |-1.5 |0.6  |1.4  |-0.8 |1.2  |-0.4 |-0.8 |0.9  |1.4  |-3.0  |
|2015 |-1.2 |-2.2 |-1.6 |0.2  |0.1  |0.0  |-2.5 |0.1  |-1.5 |-0.4 |1.1  |-2.2 |-9.8 |-10.7 |
|2016 |0.4  |0.3  |-0.7 |1.0  |-0.3 |3.8  |1.0  |4.1  |-0.2 |-2.6 |-4.1 |3.0  |5.5  |-8.9  |
|2017 |-0.7 |1.6  |-0.7 |-1.4 |0.3  |0.1  |1.5  |-0.1 |-0.6 |-0.5 |-2.4 |-1.0 |-3.9 |-6.0  |
|2018 |-0.5 |0.8  |-1.9 |-1.8 |-0.4 |1.3  |1.9  |-2.5 |0.5  |1.6  |-0.6 |     |-1.5 |-5.4  |
|Avg  |0.2  |0.4  |0.2  |0.6  |0.2  |1.1  |0.9  |0.3  |-0.1 |-1.0 |-0.8 |0.4  |2.3  |-7.4  |
    


![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-10.png)

Composite.TA strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |0.1  |0.1  |-0.3 |-1.3 |-0.1 |0.1  |-3.1 |-1.4 |2.3  |0.1  |-0.1 |-3.6 |-7.2  |
|2007 |-1.4 |2.3  |0.1  |1.6  |2.1  |3.3  |2.5  |-2.8 |-2.7 |-2.2 |-1.4 |-1.9 |-0.8 |-11.9 |
|2008 |-0.4 |0.7  |3.7  |2.8  |-3.3 |2.1  |0.2  |4.7  |-1.0 |-7.5 |-0.4 |-1.1 |-0.1 |-15.1 |
|2009 |1.0  |-3.4 |0.5  |-2.0 |1.6  |2.7  |0.8  |3.5  |-2.3 |0.8  |-1.7 |0.0  |1.2  |-7.5  |
|2010 |-2.3 |2.2  |0.6  |2.6  |2.1  |0.1  |-0.8 |-2.2 |0.5  |0.2  |1.6  |4.0  |8.8  |-5.0  |
|2011 |-1.3 |1.0  |2.8  |1.6  |2.5  |0.5  |0.3  |-1.8 |-1.9 |-1.1 |1.5  |0.3  |4.4  |-6.0  |
|2012 |-1.7 |-0.4 |1.4  |2.1  |1.5  |1.0  |2.2  |0.0  |-3.2 |-0.7 |-0.2 |-1.1 |0.6  |-6.5  |
|2013 |0.7  |2.4  |1.6  |1.6  |-5.8 |0.8  |0.2  |-1.2 |1.4  |2.8  |-1.2 |0.9  |3.9  |-7.1  |
|2014 |0.4  |0.9  |0.8  |-1.1 |-2.4 |0.6  |-0.5 |1.1  |1.5  |-0.9 |0.1  |3.6  |4.0  |-5.1  |
|2015 |-2.6 |0.0  |-0.4 |-1.0 |-0.1 |1.7  |-2.0 |1.8  |1.4  |-2.1 |4.5  |1.0  |2.1  |-6.2  |
|2016 |2.2  |1.1  |-1.0 |0.6  |0.0  |2.6  |-0.3 |2.8  |-2.6 |-3.0 |-4.6 |5.0  |2.3  |-10.3 |
|2017 |-1.7 |0.6  |-0.7 |-0.7 |1.3  |0.8  |-0.7 |-0.1 |0.0  |1.4  |-0.7 |-1.3 |-1.9 |-3.4  |
|2018 |-1.2 |1.2  |-0.4 |-1.1 |-1.7 |2.5  |2.7  |-0.4 |-0.4 |7.4  |-0.7 |     |7.7  |-4.8  |
|Avg  |-0.7 |0.7  |0.7  |0.5  |-0.3 |1.4  |0.4  |0.2  |-0.8 |-0.2 |-0.2 |0.8  |2.2  |-7.4  |
    


![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-11.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-12.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-13.png)![plot of chunk plot-3](/public/images/2018-11-04-composite-ap/plot-3-14.png)


*(this report was produced on: 2018-12-23)*
