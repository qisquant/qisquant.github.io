---
layout: post
title: Quality Factor - North America
comments: true
---




Today I would like to construct long-short quality factors using US and Canada universe of stocks.

Universe of stocks:  large and mid cap names of the US and Canada markets with approximately 750 stocks.

Factors: 

 1. ROE  (Return on equity)
 2. OP   (Operating leverage)
 3. GM   (Gross margin)
 4. OCFB (Operating cash flow to book price)
 
Signal:
 
 1. Quality factor Z-scored on [GICS](https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard) levels.
 2. Outliers winsorized.
 3. Buy high quality names, sell low quality names.

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



|           |Quality.Average   |Quality.GM        |Quality.OCFB      |Quality.OL        |Quality.ROE       |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |
|Ave.Ret    |3.1               |0.68              |1.99              |4.8               |1.36              |
|Sharpe     |0.52              |0.14              |0.42              |0.73              |0.26              |
|Volatility |6.22              |5.94              |4.95              |6.7               |5.75              |
|MarketBeta |0                 |-0.01             |0                 |0.02              |0                 |
|Turnover   |733.22            |603.84            |838.82            |614.32            |784.29            |
|MaxDD      |-14.66            |-16.69            |-12.66            |-22.23            |-16.88            |
|VaR        |-0.61             |-0.62             |-0.51             |-0.67             |-0.54             |
    


![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-2.png)

Quality.Average strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |-0.7 |0.1  |1.7  |0.5  |-0.5 |1.5  |-0.3 |1.0  |1.4  |-2.2 |0.3  |2.8  |-2.6  |
|2007 |1.6  |1.2  |0.9  |-1.7 |0.3  |0.3  |-1.3 |-0.3 |-0.7 |-1.2 |1.6  |-0.2 |0.4  |-6.1  |
|2008 |0.6  |-3.6 |-1.0 |-0.1 |-0.3 |2.6  |0.7  |0.8  |-2.5 |1.3  |5.1  |-1.7 |1.9  |-6.8  |
|2009 |-2.3 |3.9  |-4.2 |0.5  |1.8  |0.0  |-1.9 |-2.5 |-0.2 |2.3  |0.6  |-1.2 |-3.6 |-9.0  |
|2010 |1.4  |0.9  |-0.3 |-0.6 |-1.5 |-0.8 |1.4  |-2.4 |1.9  |-1.0 |1.1  |-0.4 |-0.4 |-4.8  |
|2011 |1.0  |-0.8 |1.1  |0.3  |0.8  |2.0  |-2.6 |1.5  |1.5  |-0.1 |-0.4 |1.2  |5.6  |-3.7  |
|2012 |2.2  |-0.4 |3.9  |1.1  |0.7  |-2.3 |0.5  |-1.6 |-2.9 |0.2  |0.7  |0.6  |2.5  |-8.1  |
|2013 |1.2  |1.9  |-0.4 |3.2  |0.0  |1.5  |1.3  |-2.1 |1.4  |0.6  |1.6  |-1.0 |9.7  |-3.5  |
|2014 |-4.6 |0.3  |2.0  |0.6  |-1.0 |-3.0 |-1.1 |2.2  |1.8  |5.5  |1.1  |1.3  |4.9  |-6.7  |
|2015 |0.2  |-0.9 |1.7  |-2.6 |0.8  |0.0  |4.4  |1.3  |1.8  |-1.1 |2.4  |-0.2 |8.0  |-5.2  |
|2016 |2.0  |-2.5 |-2.6 |-5.5 |1.6  |-1.2 |-1.7 |1.1  |-2.2 |2.2  |2.4  |1.9  |-4.8 |-14.7 |
|2017 |-3.0 |2.2  |-0.9 |0.1  |1.3  |1.6  |-0.9 |-0.3 |1.8  |0.2  |4.2  |-0.6 |5.7  |-3.0  |
|2018 |2.6  |1.3  |0.8  |2.6  |1.6  |-0.6 |2.1  |2.0  |-0.8 |-2.4 |     |     |9.5  |-5.0  |
|Avg  |0.3  |0.2  |0.1  |0.0  |0.5  |0.0  |0.2  |0.0  |0.1  |0.6  |1.5  |0.0  |3.2  |-6.1  |
    


![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-6.png)

Quality.GM strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |-0.5 |0.9  |1.5  |-1.1 |-0.3 |1.8  |-1.6 |2.8  |1.8  |-2.2 |-0.1 |2.9  |-2.8  |
|2007 |1.9  |-0.1 |1.3  |0.3  |0.0  |-0.8 |-1.6 |0.3  |-0.9 |-4.5 |-0.4 |-0.5 |-5.1 |-9.1  |
|2008 |1.2  |-3.9 |-1.2 |-1.2 |-2.9 |0.1  |0.6  |1.5  |1.0  |2.3  |1.0  |-2.6 |-4.2 |-11.2 |
|2009 |-1.8 |3.2  |0.2  |-0.7 |0.8  |0.1  |-2.6 |-1.8 |0.3  |2.9  |-1.4 |-2.1 |-2.9 |-7.7  |
|2010 |1.9  |1.3  |-0.1 |-0.4 |-2.4 |-0.3 |1.9  |-2.2 |2.2  |-0.6 |1.6  |-1.1 |1.6  |-4.9  |
|2011 |-1.0 |0.6  |2.3  |1.7  |1.1  |2.1  |-0.1 |1.8  |2.8  |-1.2 |0.7  |0.2  |11.5 |-3.1  |
|2012 |1.3  |0.5  |3.0  |0.9  |0.0  |-3.4 |-2.2 |-1.5 |-1.3 |-3.6 |-0.5 |-0.1 |-6.8 |-13.8 |
|2013 |1.2  |1.1  |-0.9 |2.2  |0.0  |0.7  |1.1  |-0.8 |1.9  |-0.4 |1.2  |-1.0 |6.6  |-3.0  |
|2014 |-4.0 |0.2  |1.4  |0.1  |-1.1 |-2.8 |0.1  |0.8  |2.0  |3.6  |1.4  |-0.3 |1.0  |-6.5  |
|2015 |-1.6 |-1.1 |0.7  |-0.7 |-1.3 |2.3  |3.4  |1.1  |1.1  |-1.3 |1.8  |0.5  |4.7  |-4.8  |
|2016 |3.9  |-3.7 |-3.0 |-4.2 |1.5  |-1.2 |0.0  |1.5  |-2.1 |0.6  |1.1  |-0.1 |-5.8 |-12.1 |
|2017 |-4.1 |0.7  |1.1  |0.5  |-1.5 |1.1  |-0.9 |-0.6 |1.7  |0.5  |4.6  |0.3  |3.2  |-4.5  |
|2018 |0.4  |0.6  |1.6  |1.6  |0.4  |1.1  |0.9  |2.3  |-0.6 |-4.0 |     |     |4.3  |-6.6  |
|Avg  |-0.1 |-0.1 |0.6  |0.1  |-0.5 |-0.1 |0.2  |0.1  |0.8  |-0.3 |0.8  |-0.6 |0.8  |-6.9  |
    


![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-10.png)

Quality.OCFB strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |-1.5 |1.7  |-0.1 |0.5  |1.8  |-0.1 |0.9  |0.9  |-1.4 |-1.0 |1.1  |2.8  |-2.6  |
|2007 |0.2  |0.5  |1.7  |0.9  |1.9  |1.6  |2.6  |0.4  |1.2  |0.4  |3.1  |2.1  |17.8 |-2.6  |
|2008 |-2.4 |-1.0 |-0.2 |-0.9 |-0.9 |1.9  |1.6  |-0.9 |-2.5 |1.0  |3.4  |0.5  |-0.6 |-6.9  |
|2009 |-2.8 |1.8  |-1.4 |-1.7 |2.1  |0.8  |-3.1 |0.7  |0.4  |1.0  |0.8  |-2.0 |-3.6 |-6.6  |
|2010 |0.3  |0.2  |-0.5 |-0.3 |-1.4 |1.0  |-0.7 |0.0  |2.0  |-2.2 |0.1  |0.1  |-1.6 |-4.0  |
|2011 |1.3  |1.0  |0.8  |-0.5 |0.1  |1.6  |-1.1 |-1.4 |-2.8 |2.2  |-1.1 |2.8  |2.7  |-6.6  |
|2012 |3.2  |-2.4 |-0.7 |0.1  |-0.6 |0.8  |3.0  |0.0  |-1.3 |1.6  |0.6  |0.7  |5.2  |-4.3  |
|2013 |0.0  |-0.3 |-0.9 |2.7  |-0.5 |-0.8 |0.2  |0.3  |-0.1 |1.2  |0.3  |-0.3 |1.6  |-3.4  |
|2014 |-1.5 |0.7  |-0.6 |0.4  |-1.7 |-0.2 |-1.6 |1.4  |1.5  |2.5  |-0.3 |0.0  |0.6  |-4.4  |
|2015 |1.7  |-0.4 |0.9  |-2.0 |1.6  |-1.5 |1.1  |0.1  |-1.1 |-0.6 |3.6  |-2.3 |1.0  |-5.1  |
|2016 |1.2  |-4.2 |-1.2 |-2.4 |-0.2 |-1.0 |-2.1 |1.0  |-0.8 |2.1  |1.1  |1.9  |-4.6 |-11.5 |
|2017 |-1.7 |2.1  |-1.8 |-0.5 |0.3  |0.1  |-0.8 |-1.2 |0.3  |-0.7 |2.6  |0.8  |-0.5 |-5.1  |
|2018 |2.7  |1.1  |0.7  |-0.7 |2.8  |-0.2 |0.9  |2.1  |-0.7 |-1.6 |     |     |7.4  |-3.2  |
|Avg  |0.2  |-0.2 |-0.1 |-0.4 |0.3  |0.4  |0.0  |0.3  |-0.2 |0.4  |1.1  |0.4  |2.2  |-5.1  |
    


![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-11.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-12.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-13.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-14.png)

Quality.OL strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |1.1  |-0.3 |1.4  |0.2  |-0.5 |0.4  |0.5  |0.3  |1.3  |-0.6 |0.3  |4.1  |-1.9  |
|2007 |2.0  |1.3  |-0.6 |-2.5 |1.0  |-0.1 |-0.9 |-0.2 |-0.6 |-2.2 |-3.1 |-2.5 |-8.3 |-11.7 |
|2008 |2.0  |-2.9 |-3.5 |-1.5 |-2.0 |-4.0 |3.5  |2.3  |1.8  |0.8  |0.4  |3.8  |0.3  |-14.2 |
|2009 |-4.0 |-2.3 |4.2  |-0.8 |1.9  |-0.2 |0.7  |3.2  |-0.7 |-0.9 |0.7  |1.8  |3.4  |-7.9  |
|2010 |1.0  |2.0  |2.5  |-2.1 |0.5  |-1.3 |1.4  |-3.5 |0.3  |-1.1 |-0.3 |-0.4 |-1.2 |-8.0  |
|2011 |0.4  |-0.7 |0.2  |0.4  |1.1  |2.3  |-4.5 |-1.4 |1.7  |-1.5 |-1.4 |2.7  |-1.1 |-7.8  |
|2012 |1.6  |1.0  |5.1  |-1.3 |1.7  |-0.6 |-0.1 |-1.3 |-1.7 |1.8  |1.4  |1.1  |8.9  |-5.9  |
|2013 |3.6  |4.0  |0.9  |3.6  |2.1  |3.0  |2.4  |-0.6 |1.2  |2.4  |4.0  |0.7  |30.7 |-3.1  |
|2014 |-3.4 |-1.3 |2.8  |0.5  |0.0  |-1.0 |-0.3 |0.8  |1.7  |6.4  |2.2  |1.7  |10.3 |-5.4  |
|2015 |0.0  |0.5  |2.2  |-3.2 |3.0  |2.0  |4.1  |0.4  |3.8  |0.3  |1.1  |0.4  |15.3 |-3.8  |
|2016 |-1.5 |-1.9 |-0.6 |-2.6 |1.3  |-2.1 |-0.8 |0.4  |-0.4 |1.6  |4.0  |0.3  |-2.5 |-11.1 |
|2017 |-0.8 |3.6  |-1.2 |0.2  |1.2  |-0.3 |0.7  |-0.6 |0.6  |0.2  |0.8  |-1.5 |2.8  |-2.1  |
|2018 |1.6  |1.2  |-0.3 |2.1  |0.1  |-0.5 |1.6  |0.5  |0.0  |-0.7 |     |     |5.7  |-2.3  |
|Avg  |0.2  |0.4  |0.9  |-0.5 |0.9  |-0.3 |0.6  |0.0  |0.6  |0.6  |0.8  |0.7  |5.3  |-6.6  |
    


![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-15.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-16.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-17.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-18.png)

Quality.ROE strategy : 
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |0.0  |0.6  |-0.4 |-0.2 |0.1  |-1.8 |2.2  |0.8  |-0.1 |-1.0 |0.5  |0.7  |-4.2  |
|2007 |0.4  |1.5  |1.2  |2.2  |1.1  |0.5  |-1.4 |1.2  |0.4  |-2.6 |3.3  |-0.9 |7.1  |-4.6  |
|2008 |-0.3 |-2.3 |-0.4 |1.3  |-0.2 |1.3  |1.4  |0.2  |1.6  |2.0  |-0.7 |0.1  |4.2  |-3.7  |
|2009 |-3.2 |5.1  |-5.7 |3.4  |1.4  |0.0  |-0.8 |-4.0 |0.7  |2.5  |1.5  |-1.5 |-1.1 |-10.1 |
|2010 |-1.3 |-1.5 |0.1  |-0.6 |-1.6 |0.3  |-0.6 |-0.9 |1.4  |0.5  |1.7  |-3.5 |-6.1 |-6.1  |
|2011 |2.1  |-0.7 |0.7  |-0.6 |-0.6 |1.5  |-1.2 |1.5  |0.8  |-0.4 |-1.2 |-0.4 |1.5  |-4.7  |
|2012 |2.3  |-1.2 |0.2  |0.6  |0.2  |-1.4 |1.9  |-0.9 |-1.3 |0.9  |-0.6 |0.3  |0.9  |-4.4  |
|2013 |-1.3 |-0.8 |-0.6 |-0.6 |-1.1 |-0.8 |1.5  |-0.5 |1.1  |-0.6 |0.2  |-0.6 |-4.0 |-5.6  |
|2014 |-2.4 |-0.2 |1.4  |0.2  |-1.5 |-0.8 |-0.5 |1.1  |2.0  |3.4  |2.9  |-0.2 |5.2  |-4.6  |
|2015 |-1.8 |-1.8 |1.3  |-2.0 |1.1  |-0.4 |1.7  |-0.7 |1.0  |1.2  |1.6  |-0.3 |0.8  |-5.1  |
|2016 |2.1  |-0.4 |-0.1 |-7.1 |1.1  |-0.9 |-1.3 |1.9  |-1.0 |2.9  |2.0  |1.6  |0.5  |-11.1 |
|2017 |-2.2 |2.3  |-0.3 |-0.3 |1.2  |0.6  |-0.6 |-0.3 |2.3  |0.9  |1.9  |-0.2 |5.4  |-2.9  |
|2018 |1.1  |0.0  |0.0  |0.0  |1.3  |0.1  |1.1  |1.6  |-0.6 |-0.8 |     |     |3.8  |-3.6  |
|Avg  |-0.4 |0.0  |-0.1 |-0.3 |0.2  |0.0  |0.0  |0.2  |0.7  |0.8  |1.0  |-0.4 |1.4  |-5.4  |
    


![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-19.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-20.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-21.png)![plot of chunk plot-3](/public/images/2018-10-23-quality-na/plot-3-22.png)


*(this report was produced on: 2018-11-07)*
