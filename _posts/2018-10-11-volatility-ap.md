---
layout: post
title: Volatility Factor - Asia
comments: true
---




Today I would like to construct long-short volatility factors using Asian universe of stocks.

Universe of stocks:  large and mid cap names with approximately 150 stocks..

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
 
 1. Single name weight +-1.5%.
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
      qis.plot.xts(tmp['2006-03::'], main = paste0(model.name, " # of names in the portfolio"), copyright = copyright, ylim=c(10,200))
      
      # plot running maximum and minimum weight in portfolio
      tmp1 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'long', type = 'max', single = T); names(tmp1) = 'Max.Long'
      tmp2 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'short', type = 'min', single = T);names(tmp2) = 'Min.Short'
      tmp  = merge(tmp1,tmp2)
      tmp  = iif(tmp == 0, NA, tmp)
      tmp  = qis.apply.matrix(tmp, function(x) ifna.prev.next(x))
      #qis.plot.xts(tmp[tmp>0], main = paste0(model.name, " single name exposure"), copyright = copyright,  ylim=c(-0.05,0.05))
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
}
{% endhighlight %}



|           |Vol.12m           |Vol.24m           |Vol.3m            |Vol.6m            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |
|Ave.Ret    |-1.87             |-2.49             |-1.5              |-2.07             |
|Sharpe     |-0.23             |-0.36             |-0.19             |-0.25             |
|Volatility |7.05              |6.44              |6.83              |7.26              |
|MarketBeta |-0.03             |-0.03             |-0.04             |-0.03             |
|Turnover   |724.86            |714.69            |766.55            |715.79            |
|MaxDD      |-37.48            |-44.64            |-30.2             |-37.86            |
|VaR        |-0.65             |-0.67             |-0.65             |-0.65             |
    


![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-2.png)

Vol.3m strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |-0.1 |-0.9 |0.9  |0.9  |0.1  |-0.1 |-2.7 |-1.9 |-0.6 |-0.1 |-1.3 |-5.7 |-7.2  |
|2007 |0.0  |0.7  |-0.8 |-0.7 |0.3  |0.3  |1.2  |-3.0 |-3.7 |-1.6 |1.7  |0.8  |-4.8 |-8.9  |
|2008 |0.3  |-1.2 |2.1  |5.3  |-5.1 |2.7  |4.5  |0.2  |1.5  |-0.8 |0.6  |-1.6 |8.4  |-8.1  |
|2009 |1.9  |-3.7 |-4.8 |3.5  |5.8  |-4.9 |2.4  |-0.2 |-1.7 |-0.9 |0.6  |0.0  |-2.6 |-10.4 |
|2010 |-0.4 |0.4  |-0.1 |0.1  |2.2  |-1.0 |-1.2 |-0.3 |-0.4 |-1.0 |1.4  |1.0  |0.7  |-4.5  |
|2011 |1.5  |0.9  |2.0  |-0.1 |-1.6 |0.6  |0.4  |-3.2 |-3.0 |-1.5 |-1.7 |-1.7 |-7.2 |-12.2 |
|2012 |-1.0 |0.7  |-1.1 |1.2  |-0.6 |-1.1 |-0.3 |-0.4 |-0.3 |-2.0 |-0.4 |-0.8 |-6.0 |-6.4  |
|2013 |-0.1 |1.1  |-1.3 |-1.6 |-0.7 |0.6  |0.4  |-0.3 |0.7  |0.6  |1.9  |1.4  |2.4  |-5.8  |
|2014 |-0.4 |-1.9 |0.3  |-0.8 |-2.4 |2.1  |-0.2 |0.9  |1.4  |-1.6 |-0.7 |0.5  |-2.9 |-5.9  |
|2015 |-1.2 |0.7  |0.8  |0.8  |0.7  |0.7  |-0.2 |-2.3 |2.3  |-2.3 |-0.2 |-0.2 |-0.6 |-4.5  |
|2016 |-1.9 |-1.2 |-2.6 |1.3  |-1.4 |1.9  |-2.5 |2.4  |-0.2 |-2.1 |-1.0 |0.0  |-7.3 |-8.3  |
|2017 |-0.8 |-0.7 |-0.7 |-0.8 |2.5  |-1.1 |-0.3 |0.6  |-0.9 |0.8  |0.4  |-0.1 |-1.2 |-4.0  |
|2018 |-1.3 |2.5  |1.6  |-0.8 |0.5  |1.3  |0.6  |-1.9 |0.7  |5.4  |     |     |8.8  |-3.0  |
|Avg  |-0.3 |-0.1 |-0.4 |0.6  |0.1  |0.2  |0.4  |-0.8 |-0.4 |-0.6 |0.2  |-0.2 |-1.4 |-6.9  |
    


![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-6.png)

Vol.6m strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |0.0  |0.2  |-0.5 |1.5  |-0.4 |-2.2 |-3.0 |-1.2 |0.1  |-0.4 |-1.0 |-6.9  |-8.9  |
|2007 |-0.7 |0.2  |0.2  |-0.5 |-1.1 |1.9  |-1.6 |-3.2 |-3.4 |-0.5 |2.3  |1.4  |-5.1  |-9.9  |
|2008 |0.4  |-0.1 |2.9  |3.5  |-6.0 |3.5  |5.7  |3.1  |0.5  |-1.7 |1.7  |-3.1 |10.1  |-8.4  |
|2009 |2.2  |-4.2 |-3.2 |4.3  |5.0  |-4.4 |0.1  |-3.8 |-1.4 |1.5  |3.4  |0.0  |-1.3  |-10.1 |
|2010 |-1.8 |1.8  |-0.3 |0.3  |4.2  |-1.3 |-0.2 |-1.0 |1.3  |-1.1 |-0.4 |1.8  |3.3   |-4.4  |
|2011 |0.0  |1.3  |0.4  |-0.1 |-1.1 |0.0  |0.0  |-3.3 |-2.3 |-0.5 |-1.6 |-3.3 |-10.1 |-12.3 |
|2012 |0.3  |-0.4 |-1.7 |0.1  |0.3  |-1.3 |-0.8 |-0.8 |-0.5 |-2.1 |-0.9 |-0.7 |-8.2  |-9.2  |
|2013 |-0.6 |0.9  |-0.3 |-1.7 |-0.6 |0.4  |0.5  |0.2  |0.2  |0.8  |2.6  |0.8  |3.2   |-4.7  |
|2014 |-1.4 |-3.0 |-0.4 |-0.8 |-0.6 |1.3  |0.0  |0.3  |0.6  |-1.2 |-0.2 |-0.2 |-5.4  |-7.0  |
|2015 |-1.8 |0.6  |0.7  |-0.6 |0.2  |0.8  |-1.7 |-2.2 |1.9  |-2.9 |0.1  |-0.1 |-5.1  |-6.4  |
|2016 |-1.7 |-1.7 |-2.7 |1.1  |-1.3 |1.6  |-2.6 |3.6  |-1.0 |-1.5 |-0.6 |-1.2 |-8.0  |-9.1  |
|2017 |-1.0 |-2.2 |-1.3 |0.1  |2.6  |-1.6 |-0.3 |-0.1 |-0.4 |0.3  |0.0  |0.4  |-3.4  |-5.5  |
|2018 |1.0  |2.2  |1.2  |0.2  |-0.3 |1.6  |0.3  |-1.3 |-0.4 |7.8  |     |     |12.5  |-3.2  |
|Avg  |-0.4 |-0.4 |-0.3 |0.4  |0.2  |0.2  |-0.2 |-0.9 |-0.5 |-0.1 |0.5  |-0.4 |-1.9  |-7.6  |
    


![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-10-11-volatility-ap/plot-3-10.png)


*(this report was produced on: 2018-10-31)*
