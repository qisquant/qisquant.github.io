---
layout: post
title: Quality Factor - Japan
comments: true
---




Today I would like to construct long-short quality factors using Japanese universe of stocks.

Universe of stocks:  large and mid cap names with approximately 350 stocks.

Factors: 

 1. ROE  (Return on equity)
 2. OCFB (Operating cash flow to book price)
 
Signal:
 
 1. Quality Z-scored on [GICS](https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard) levels.
 2. Outliers winsorized.
 3. Buy high quality names, sell low quality names.

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
  for(model.name in ls(models)) models[[model.name]]$equity = scale.one(models[[model.name]]$equity)
  
  #*****************************************************************
  # Strategies statistics
  #*****************************************************************
  print(qis.plot.strategy.sidebyside(models, make.plot = F, return.table = T, perfromance.fn = qis.strategy.stat.ms))

  #*****************************************************************
  # Plot Backtests
  #*****************************************************************
  qis.plot.strategy(models, plotX = T, log = '', LeftMargin = 3, main = 'Quality strategies', copyright = copyright)
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
{% endhighlight %}



|           |Quality.Average   |Quality.OFCFB     |Quality.ROE       |
|:----------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |
|Ave.Ret    |1.49              |2.85              |-1.16             |
|Sharpe     |0.29              |0.57              |-0.16             |
|Volatility |5.75              |5.18              |6.12              |
|MarketBeta |-0.01             |0                 |0                 |
|Turnover   |709.58            |772.15            |683.21            |
|MaxDD      |-17.09            |-18.83            |-25.65            |
|VaR        |-0.6              |-0.52             |-0.64             |
    


![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-2.png)

Quality.Average strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |0.1  |-1.2 |0.5  |2.2  |-0.1 |-2.3 |-1.7 |-0.1 |2.2  |0.4  |0.9  |0.9  |-4.8  |
|2007 |-2.4 |3.3  |-2.2 |0.5  |-0.3 |-0.4 |0.6  |-0.7 |0.8  |-0.8 |1.3  |-0.5 |-1.0 |-4.9  |
|2008 |-2.8 |-2.0 |1.5  |-0.9 |1.0  |1.9  |-3.2 |-2.3 |-1.9 |3.7  |-0.5 |-1.3 |-6.7 |-12.1 |
|2009 |0.2  |1.9  |-1.2 |-4.3 |5.1  |2.0  |-0.2 |-1.6 |0.7  |0.8  |3.7  |-0.7 |6.3  |-6.4  |
|2010 |-0.4 |0.3  |-0.2 |-0.8 |1.2  |0.6  |1.1  |-1.0 |1.7  |-0.6 |1.2  |0.4  |3.3  |-2.7  |
|2011 |2.3  |1.3  |2.1  |1.2  |0.9  |1.3  |0.0  |1.8  |-0.2 |0.5  |2.1  |0.1  |14.0 |-2.4  |
|2012 |-0.6 |-1.8 |-0.1 |3.4  |2.9  |-0.9 |3.3  |0.6  |0.3  |-0.1 |-0.5 |-4.4 |1.8  |-6.4  |
|2013 |1.4  |-0.8 |0.3  |-2.1 |2.5  |-1.8 |0.4  |-0.7 |1.0  |-0.6 |-2.6 |-1.1 |-4.1 |-6.4  |
|2014 |-0.5 |0.1  |1.0  |-0.9 |0.8  |0.6  |-0.3 |0.4  |0.4  |0.0  |-0.8 |-0.1 |0.7  |-2.6  |
|2015 |-2.4 |-0.3 |0.0  |-0.6 |0.6  |0.6  |-0.1 |-1.2 |0.2  |0.2  |-0.9 |-0.9 |-4.9 |-5.8  |
|2016 |1.1  |-0.8 |0.5  |-3.2 |0.8  |2.9  |0.5  |-2.9 |0.3  |0.2  |-1.2 |-0.1 |-2.1 |-5.9  |
|2017 |0.2  |-1.3 |1.3  |1.7  |2.9  |0.9  |-0.3 |-0.1 |1.2  |0.1  |2.3  |-0.9 |8.1  |-2.5  |
|2018 |-0.5 |1.0  |3.6  |0.6  |2.2  |-0.6 |-0.7 |1.4  |-0.9 |-0.7 |     |     |5.4  |-3.2  |
|Avg  |-0.4 |0.1  |0.4  |-0.4 |1.7  |0.5  |-0.1 |-0.6 |0.3  |0.4  |0.4  |-0.7 |1.7  |-5.1  |
    


![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-6.png)

Quality.OFCFB strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |-0.8 |-0.1 |0.6  |2.1  |1.0  |-1.5 |-0.8 |0.6  |1.9  |-0.6 |0.6  |2.9   |-2.7  |
|2007 |-0.5 |3.7  |-0.7 |1.0  |-0.2 |-1.1 |-0.1 |-1.5 |-1.1 |1.6  |1.6  |0.6  |3.3   |-6.1  |
|2008 |1.6  |-0.6 |1.1  |-1.4 |-0.6 |0.8  |-1.7 |-0.5 |0.1  |0.9  |1.6  |1.8  |3.1   |-6.5  |
|2009 |-2.1 |1.0  |-2.3 |-3.8 |2.4  |1.8  |-0.9 |-2.0 |1.3  |1.0  |3.7  |-0.6 |-1.0  |-8.2  |
|2010 |1.6  |0.7  |0.2  |0.6  |-0.4 |0.7  |0.8  |-1.1 |1.4  |-1.5 |2.6  |0.9  |6.7   |-2.6  |
|2011 |2.7  |0.3  |-0.2 |3.5  |1.2  |1.0  |3.2  |0.6  |0.7  |0.4  |1.0  |1.2  |16.9  |-2.7  |
|2012 |0.6  |-1.1 |-0.3 |2.4  |2.8  |-1.0 |1.5  |0.1  |2.0  |-0.5 |1.9  |-4.5 |3.7   |-5.1  |
|2013 |2.2  |0.1  |-2.6 |-1.1 |3.6  |-1.4 |0.7  |-0.6 |0.2  |-0.7 |-1.5 |-0.9 |-2.3  |-5.5  |
|2014 |-1.0 |0.7  |1.2  |-1.0 |1.2  |0.6  |-0.4 |0.5  |0.4  |0.4  |-0.2 |1.2  |3.7   |-2.3  |
|2015 |-1.9 |-2.1 |0.4  |-1.7 |-0.7 |-0.3 |-2.7 |-2.1 |1.0  |1.9  |-1.0 |-1.3 |-10.2 |-12.4 |
|2016 |2.1  |0.1  |-0.1 |-2.2 |0.7  |0.7  |1.8  |-5.3 |0.5  |-0.5 |-1.3 |-0.5 |-4.2  |-9.0  |
|2017 |-0.3 |-0.5 |2.6  |1.1  |2.8  |-0.3 |-0.5 |2.0  |-0.3 |0.1  |1.6  |0.5  |9.2   |-2.2  |
|2018 |0.3  |1.9  |2.3  |0.5  |2.0  |0.2  |-0.7 |0.9  |0.7  |0.2  |     |     |8.4   |-2.4  |
|Avg  |0.4  |0.3  |0.1  |-0.1 |1.3  |0.2  |0.0  |-0.7 |0.6  |0.4  |0.8  |-0.1 |3.1   |-5.2  |
    


![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-10.png)

Quality.ROE strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |-0.1 |-0.8 |1.2  |2.1  |-0.4 |-1.7 |-0.1 |-0.5 |1.2  |-0.5 |1.3  |1.7   |-4.4  |
|2007 |-2.3 |2.5  |-2.7 |-0.9 |0.7  |0.8  |-0.4 |0.0  |2.1  |-0.2 |-0.3 |0.1  |-0.8  |-5.6  |
|2008 |-1.6 |-3.6 |0.0  |-1.1 |1.9  |1.5  |-2.7 |-2.8 |-3.9 |1.1  |-0.3 |-0.2 |-11.3 |-15.6 |
|2009 |0.4  |3.1  |-1.9 |-1.5 |2.8  |0.9  |-0.8 |-1.0 |1.3  |2.1  |4.6  |-3.0 |6.9   |-6.0  |
|2010 |-0.2 |0.4  |-1.3 |-1.4 |1.5  |0.0  |-0.6 |-0.9 |1.3  |-1.1 |0.9  |0.0  |-1.5  |-4.0  |
|2011 |1.6  |-0.5 |0.9  |1.5  |1.0  |-0.1 |0.7  |0.6  |-1.5 |1.3  |2.5  |0.7  |8.8   |-3.3  |
|2012 |-0.4 |-0.6 |-0.7 |3.6  |1.9  |-0.1 |4.3  |1.0  |0.2  |0.1  |-1.2 |-6.5 |1.0   |-9.5  |
|2013 |-0.6 |-0.7 |0.0  |-3.6 |3.4  |-0.9 |1.9  |-2.8 |1.9  |-0.8 |-2.8 |-0.9 |-6.0  |-7.7  |
|2014 |-1.9 |-3.2 |0.6  |0.0  |0.6  |-0.1 |-0.1 |-0.1 |0.9  |0.2  |-0.5 |-0.3 |-3.9  |-6.6  |
|2015 |-2.6 |1.1  |-0.8 |-1.3 |1.9  |-1.0 |-1.8 |-2.5 |-1.0 |2.3  |0.4  |-0.7 |-6.0  |-9.5  |
|2016 |1.4  |0.7  |0.5  |-3.6 |-0.2 |2.4  |1.0  |-3.2 |0.3  |0.6  |-1.5 |0.3  |-1.5  |-6.4  |
|2017 |0.6  |-1.3 |1.0  |1.5  |2.1  |-0.3 |0.0  |0.2  |1.5  |-0.1 |1.5  |-2.0 |4.7   |-2.7  |
|2018 |-1.1 |-1.9 |1.1  |-1.8 |1.9  |-1.7 |0.2  |0.3  |-0.8 |-1.6 |     |     |-5.3  |-5.6  |
|Avg  |-0.6 |-0.3 |-0.3 |-0.6 |1.7  |0.1  |0.0  |-0.9 |0.1  |0.4  |0.2  |-0.9 |-1.0  |-6.7  |
    


![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-11.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-12.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-13.png)![plot of chunk plot-3](/public/images/2018-10-25-quality-jp/plot-3-14.png)


*(this report was produced on: 2018-11-07)*
