---
layout: post
title: Volatility Factor - Europe
comments: true
---




Today I would like to construct long-short volatility factors using European universe of stocks.

Universe of stocks:  large and mid cap names of 15 developed countries in Europe with approximately 500 stocks.

Factors: 

 1. Vol.3m     (3m rolling volatility)
 2. Vol.6m     (6m rolling volatility)
 3. Vol.12m    (12m rolling volatility)
 4. Vol.24m    (24m rolling volatility)
 
Signal:
 
 1. Volatility Z-scored on [GICS](https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard) levels.
 2. Outliers winsorized.
 3. Buy low volatility names, sell high volatility names

Strategy constrains:
 
 1. Single name weight +-1%.
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
    data = qis.ef.corpact.track(data)
    
    # adjust for factor coverage
    data = qis.ef.adjust.for.factor.coverage(data,config)
    
    # adjust raw factors if needed
    data = qis.ef.adjust.config(data,config)
    
    # Create signal and do clean-ups
    data = qis.ef.default.signal(data,config)
    
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
  qis.plot.strategy(models, plotX = T, log = '', LeftMargin = 3, main = 'Volatility strategies', copyright = copyright)
  mtext('Strategy Performance', side = 2, line = 1)
  
  #*****************************************************************
  # Plot Portfolio Turnovers
  #*****************************************************************
  qis.barplot.with.labels(sapply(models, qis.compute.turnover), 'Annual Portfolio Turnover', col = col.add.alpha(3))
  
  #*****************************************************************
  # Exposure statistics
  #***************************************************************** 
  for(model.name in ls(models)){
    if(!model.name == "Vol.12m" && !model.name == "Vol.24m") {
      print(paste0(model.name, " strategy : "))
      
      # plot monthly returns
      print(qis.plot.monthly.table(models[[model.name]]$equity, make.plot = F, model.name))
      
      # plot market beta exposure
      qis.plot.xts(models[[model.name]]$beta, main = paste0(model.name, " market beta exposure"), copyright = copyright, ylim = c(-1.0, 1.0))
      
      # plot # of names in the portfolio
      tmp = qis.compute.names.stat(models[[model.name]]$weight, side = 'all')
      qis.plot.xts(tmp['2006-03::'], main = paste0(model.name, " # of names in the portfolio"), copyright = copyright, ylim=c(50,300))
      
      # plot running maximum and minimum weight in portfolio
      tmp1 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'long', type = 'max', single = T); names(tmp1) = 'Max.Long'
      tmp2 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'short', type = 'min', single = T);names(tmp2) = 'Min.Short'
      tmp  = merge(tmp1,tmp2)
      qis.plot.xts(tmp[tmp>0], main = paste0(model.name, " single name exposure"), copyright = copyright,  ylim=c(-0.05,0.05))
      
      # plot long,short,net and gross exposure
      tmp1 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'net'); names(tmp1) = 'Net exposure'
      tmp2 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'gross'); names(tmp2) = 'Gross exposure'
      tmp3 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'long'); names(tmp3) = 'Long exposure'
      tmp4 = qis.compute.weights.stat(models[[model.name]]$weight, type = 'xts', side = 'short'); names(tmp4) = 'Short exposure'
      tmp  = merge(tmp1,tmp2,tmp3,tmp4)
      qis.plot.xts(tmp['2006-03::'], main = paste0(model.name, " daily exposures"), copyright = copyright, ylim=c(-2,3.0)) 
    }
  }
}
{% endhighlight %}



|           |Vol.12m           |Vol.24m           |Vol.3m            |Vol.6m            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |
|Ave.Ret    |1.59              |0.72              |3.73              |3.78              |
|Sharpe     |0.24              |0.13              |0.5               |0.5               |
|Volatility |8.03              |7.73              |8.02              |8.12              |
|MarketBeta |-0.03             |-0.03             |-0.06             |-0.04             |
|Turnover   |865.52            |795.92            |1139.37           |955.15            |
|MaxDD      |-21.36            |-23.8             |-19.38            |-19.06            |
|VaR        |-0.8              |-0.74             |-0.75             |-0.78             |
    


![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-2.png)

Vol.3m strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |-1.4 |0.1  |-1.8 |0.3  |-0.5 |-0.4 |-1.1 |-1.5 |0.2  |1.8  |-0.1 |-4.3 |-6.5  |
|2007 |-1.8 |0.9  |-0.2 |0.8  |-0.7 |1.7  |1.3  |0.8  |-2.9 |1.1  |1.4  |0.6  |2.8  |-3.8  |
|2008 |-0.2 |-0.7 |2.7  |-2.1 |2.7  |-3.1 |8.6  |-2.4 |12.6 |10.7 |2.3  |1.4  |35.7 |-4.8  |
|2009 |-1.9 |-2.7 |-2.9 |-2.6 |-0.1 |-0.3 |-2.0 |0.7  |-0.9 |2.8  |1.7  |0.6  |-7.5 |-13.2 |
|2010 |-2.3 |0.0  |-0.1 |0.6  |2.8  |2.1  |-1.4 |-1.5 |-0.7 |1.5  |-0.4 |2.0  |2.8  |-4.2  |
|2011 |-3.8 |-0.6 |-1.3 |3.2  |1.5  |-0.1 |2.5  |4.1  |4.8  |0.1  |3.1  |2.4  |16.5 |-6.9  |
|2012 |0.4  |-0.2 |0.8  |2.4  |-0.7 |-1.0 |1.1  |-1.4 |0.4  |-1.9 |-0.1 |-0.8 |-1.1 |-7.3  |
|2013 |-3.5 |2.6  |2.0  |-4.3 |1.9  |0.0  |-1.1 |-0.7 |0.4  |-4.9 |-0.6 |-0.2 |-8.4 |-9.8  |
|2014 |-3.6 |-1.1 |1.5  |0.9  |0.9  |-0.4 |2.0  |3.4  |1.0  |0.9  |-2.9 |1.4  |4.0  |-5.8  |
|2015 |-3.2 |-3.1 |0.8  |-1.0 |1.3  |0.4  |1.5  |-1.0 |2.2  |0.8  |0.2  |-1.1 |-2.2 |-6.9  |
|2016 |-1.6 |3.7  |1.2  |0.4  |1.6  |-4.2 |-0.9 |2.1  |3.5  |4.5  |-1.3 |-0.8 |8.1  |-7.2  |
|2017 |0.4  |-1.6 |0.5  |-0.3 |0.5  |0.3  |-0.9 |0.1  |1.4  |2.1  |3.6  |-0.9 |5.4  |-3.7  |
|2018 |0.0  |0.0  |0.4  |-1.1 |2.1  |2.6  |-0.6 |2.9  |-0.8 |-1.0 |     |     |4.4  |-2.7  |
|Avg  |-1.7 |-0.3 |0.4  |-0.4 |1.1  |-0.2 |0.8  |0.5  |1.5  |1.3  |0.7  |0.4  |4.3  |-6.4  |
    


![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-6.png)

Vol.6m strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |-1.8 |0.0  |-2.3 |0.4  |1.2  |-1.0 |-0.8 |-2.2 |0.4  |1.7  |-0.9 |-5.3 |-6.7  |
|2007 |-1.3 |0.9  |-0.1 |-0.2 |-2.2 |1.3  |0.7  |0.7  |-3.7 |1.5  |1.6  |0.3  |-0.6 |-6.1  |
|2008 |-0.1 |-0.9 |3.9  |-1.2 |1.9  |-1.1 |7.9  |-2.7 |13.0 |11.0 |2.1  |4.7  |44.2 |-4.5  |
|2009 |-1.6 |-1.2 |-2.3 |-0.5 |-1.3 |-0.4 |-2.2 |1.1  |-1.3 |3.7  |0.2  |0.8  |-5.1 |-10.3 |
|2010 |-3.2 |0.3  |-1.3 |-0.2 |3.4  |2.0  |-2.0 |-1.9 |-0.5 |1.1  |0.4  |2.0  |-0.1 |-5.1  |
|2011 |-4.0 |0.1  |-1.0 |3.2  |2.8  |-0.8 |4.1  |2.0  |4.2  |1.3  |2.2  |2.5  |17.6 |-6.4  |
|2012 |1.1  |0.6  |0.2  |3.1  |-0.5 |-0.4 |1.9  |-1.8 |0.9  |-0.1 |0.3  |-0.3 |5.2  |-4.6  |
|2013 |-2.6 |4.0  |3.0  |-5.0 |2.3  |-0.1 |-2.8 |-1.7 |0.3  |-5.2 |-1.3 |0.1  |-9.1 |-13.3 |
|2014 |-3.8 |-2.0 |-0.1 |1.1  |3.0  |-1.0 |0.7  |1.3  |0.9  |2.5  |-1.3 |0.8  |2.0  |-6.9  |
|2015 |-2.6 |-2.3 |0.8  |-0.2 |1.2  |-0.6 |0.3  |0.1  |1.6  |1.3  |-0.4 |-0.5 |-1.4 |-5.3  |
|2016 |0.6  |3.4  |0.3  |0.6  |1.4  |-5.0 |0.0  |1.6  |1.9  |3.8  |0.0  |-0.6 |8.0  |-7.7  |
|2017 |0.6  |-2.2 |1.8  |1.0  |0.8  |0.3  |-1.8 |0.1  |1.7  |3.0  |2.4  |-2.1 |5.6  |-4.1  |
|2018 |0.4  |-0.3 |-0.1 |-1.7 |1.3  |1.9  |-1.3 |1.9  |-1.4 |-2.3 |     |     |-1.7 |-4.1  |
|Avg  |-1.4 |-0.1 |0.4  |-0.2 |1.1  |-0.2 |0.4  |0.0  |1.2  |1.7  |0.7  |0.6  |4.6  |-6.5  |
    


![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-10-09-volatility-eu/plot-3-10.png)


*(this report was produced on: 2018-10-30)*
