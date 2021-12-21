---
layout: post
title: Volatility Factor - Global
comments: true
---






Today I would like to construct long-short global volatility factors using

 - [Volatility Factor - North America](https://qisresearch.github.io/volatility-na)
 - [Volatility Factor - Europe](https://qisresearch.github.io/volatility-eu)
 - [Volatility Factor - Japan](https://qisresearch.github.io/volatility-jp)
 - [Volatility Factor - Asia](https://qisresearch.github.io/volatility-ap)

Factors: 

 1. Vol.3m     (3m rolling volatility)
 2. Vol.6m     (6m rolling volatility)
 3. Vol.12m    (12m rolling volatility)
 4. Vol.24m    (24m rolling volatility)

Strategy constrains:

 1. Weight each regional volatility according to market capitalization
 2. Overall leverage kept at 100%.
 4. Rebalance every month end.
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
    
    # Create  index
    data = qis.ef.index(data,config)
    
    # compute actual weights
    data = qis.ef.compute.actual.weights(data,config)
    
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
  # for(model.name in ls(models)){
  #   models[[model.name]]$equity = scale.one(models[[model.name]]$equity)
  # }
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
  # Extra stat
  #***************************************************************** 
  for(model.name in ls(models)){
    
    # plot monthly returns
    print(paste0(model.name, " strategy :"))
    print(qis.plot.monthly.table(models[[model.name]]$equity, make.plot = F, model.name))

    # plot weights
    qis.plot.strategy.weights(models[[model.name]]$weight, name='Volatility strategies')
  }
}
{% endhighlight %}



|           |Vol.12m           |Vol.24m           |Vol.3m            |Vol.6m            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Feb2006 - Oct2018 |Feb2006 - Oct2018 |Feb2006 - Oct2018 |Feb2006 - Oct2018 |
|Ave.Ret    |1.14              |1.11              |1.64              |1.58              |
|Sharpe     |0.25              |0.25              |0.35              |0.33              |
|Volatility |5                 |4.95              |5.05              |5.11              |
|Turnover   |88.35             |85.61             |87.23             |87.81             |
|MaxDD      |-20.85            |-20.67            |-19.3             |-18.1             |
|VaR        |-0.45             |-0.44             |-0.47             |-0.46             |
    


![plot of chunk plot-3](/public/images/2018-10-12-volatility-gb/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-12-volatility-gb/plot-3-2.png)

Vol.12m strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |-0.5 |-0.8 |0.2  |0.2  |0.9  |-1.5 |-1.6 |-0.8 |0.9  |-0.1 |-3.3  |-4.4  |
|2007 |-1.0 |0.5  |0.1  |-0.6 |-1.2 |1.0  |0.4  |1.0  |-2.5 |0.6  |2.0  |1.2  |1.5   |-3.8  |
|2008 |2.2  |0.2  |3.4  |-1.3 |0.5  |-1.6 |3.9  |0.9  |10.5 |3.1  |0.4  |-1.2 |22.4  |-3.6  |
|2009 |-2.9 |-1.0 |-2.9 |-1.2 |-3.4 |0.4  |-0.4 |-0.3 |-1.3 |2.8  |-0.8 |-1.1 |-11.3 |-12.7 |
|2010 |-3.6 |-0.3 |-0.9 |-0.8 |1.0  |1.0  |-0.9 |-1.0 |-0.7 |1.0  |0.8  |-0.9 |-5.1  |-6.0  |
|2011 |-1.7 |-0.1 |-0.8 |0.5  |1.0  |0.1  |-0.1 |3.1  |3.9  |1.0  |2.4  |1.7  |11.4  |-3.0  |
|2012 |-1.2 |-2.0 |-0.1 |1.7  |0.4  |-0.3 |1.1  |-0.8 |-1.4 |0.0  |-0.6 |-1.4 |-4.5  |-5.8  |
|2013 |-0.8 |1.9  |0.5  |-2.6 |-0.5 |1.5  |-1.8 |-0.4 |0.4  |-1.2 |1.4  |1.3  |-0.3  |-5.7  |
|2014 |-2.8 |-1.2 |1.1  |-0.3 |0.7  |-1.5 |0.7  |0.4  |2.2  |1.6  |-0.9 |0.3  |0.1   |-5.0  |
|2015 |-1.4 |-0.1 |1.4  |-0.6 |0.3  |0.9  |2.6  |1.1  |0.7  |-0.2 |1.9  |-2.7 |3.7   |-4.7  |
|2016 |2.6  |-1.3 |-0.1 |-3.0 |1.1  |-0.9 |-1.0 |0.2  |0.4  |1.9  |2.0  |-0.1 |1.4   |-5.7  |
|2017 |-1.6 |0.0  |1.8  |-0.3 |-0.5 |0.6  |-0.6 |0.1  |1.1  |2.5  |-0.4 |-1.5 |1.2   |-2.9  |
|2018 |0.1  |-1.6 |0.9  |0.5  |0.5  |-0.6 |1.0  |0.9  |0.4  |-0.5 |     |     |1.6   |-2.5  |
|Avg  |-1.0 |-0.4 |0.3  |-0.7 |0.0  |0.1  |0.4  |0.3  |0.9  |0.9  |0.8  |-0.4 |1.4   |-5.1  |
    


![plot of chunk plot-3](/public/images/2018-10-12-volatility-gb/plot-3-3.png)

Vol.24m strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |-0.6 |-0.8 |0.5  |0.2  |1.4  |-1.6 |-1.2 |-0.9 |-0.2 |0.1  |-3.0  |-4.6  |
|2007 |-0.6 |-0.6 |0.6  |-0.6 |-0.9 |-0.8 |0.3  |0.5  |-4.1 |0.8  |1.8  |0.3  |-3.3  |-6.6  |
|2008 |2.1  |0.2  |3.2  |-0.5 |0.0  |-0.3 |4.4  |1.3  |12.0 |4.4  |-0.2 |-1.6 |27.1  |-3.4  |
|2009 |-5.3 |-1.9 |-2.4 |-1.0 |-3.9 |0.6  |1.0  |0.0  |-0.7 |3.1  |-0.9 |-1.1 |-12.1 |-14.1 |
|2010 |-1.7 |0.0  |-1.1 |-0.4 |0.6  |1.4  |-0.6 |-0.6 |0.1  |-0.1 |-1.8 |-0.7 |-4.7  |-4.8  |
|2011 |-0.4 |0.3  |-0.8 |0.6  |0.2  |0.2  |-0.2 |3.5  |4.8  |1.6  |2.0  |1.4  |13.9  |-2.1  |
|2012 |-0.8 |-1.6 |-0.2 |1.7  |0.6  |-0.9 |1.5  |-1.0 |-1.7 |-0.1 |-0.6 |-1.8 |-5.0  |-5.9  |
|2013 |-1.4 |1.7  |0.0  |-2.0 |-0.4 |1.4  |0.3  |-0.4 |0.8  |-0.9 |0.4  |1.4  |0.8   |-3.1  |
|2014 |-2.0 |-0.5 |0.4  |0.1  |1.1  |-1.9 |1.0  |-0.1 |2.1  |2.9  |-1.2 |0.9  |2.7   |-3.5  |
|2015 |-2.1 |1.2  |1.4  |-0.6 |0.7  |0.2  |1.1  |1.2  |0.7  |-0.5 |1.2  |-2.1 |2.2   |-4.0  |
|2016 |3.2  |-1.1 |0.0  |-2.4 |1.1  |-0.4 |-0.6 |0.2  |0.9  |1.3  |2.1  |-0.1 |4.2   |-4.1  |
|2017 |-2.2 |0.8  |0.8  |-0.4 |-0.7 |-0.6 |-0.6 |-0.6 |1.0  |1.2  |0.0  |-1.8 |-3.1  |-4.4  |
|2018 |-1.3 |-0.1 |0.0  |0.8  |-0.6 |-0.1 |1.0  |1.6  |0.3  |-1.5 |     |     |0.1   |-2.6  |
|Avg  |-1.0 |-0.1 |0.1  |-0.4 |-0.1 |-0.1 |0.8  |0.3  |1.1  |0.9  |0.2  |-0.4 |1.5   |-4.9  |
    


![plot of chunk plot-3](/public/images/2018-10-12-volatility-gb/plot-3-4.png)

Vol.3m strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |0.3  |-0.7 |0.5  |0.5  |0.4  |-1.0 |-1.3 |0.2  |1.5  |-0.5 |-0.2  |-3.1  |
|2007 |0.3  |0.4  |-0.5 |-0.1 |0.0  |1.3  |1.4  |0.5  |-1.6 |0.8  |2.0  |1.8  |6.4   |-3.0  |
|2008 |0.1  |0.6  |2.6  |-0.8 |0.5  |-2.9 |5.0  |-0.1 |8.1  |4.9  |0.8  |-3.0 |16.3  |-5.0  |
|2009 |-5.0 |-1.2 |-1.7 |-2.4 |0.0  |0.1  |-2.9 |-0.8 |-1.4 |2.7  |-0.1 |-1.0 |-13.1 |-15.0 |
|2010 |-1.0 |0.0  |0.1  |1.5  |1.0  |0.9  |-0.7 |0.0  |-0.9 |1.4  |0.1  |-0.6 |1.8   |-2.2  |
|2011 |-0.8 |0.0  |-0.9 |1.0  |0.1  |0.4  |-0.5 |2.4  |3.1  |1.8  |1.8  |0.8  |9.4   |-2.3  |
|2012 |-1.6 |-1.7 |0.7  |1.6  |0.5  |0.1  |1.6  |-1.4 |-1.1 |-0.4 |0.0  |-0.6 |-2.3  |-5.7  |
|2013 |-1.9 |1.4  |-0.5 |-1.5 |0.3  |1.4  |-1.5 |0.8  |0.3  |-1.6 |0.6  |0.9  |-1.4  |-3.8  |
|2014 |-2.9 |-1.6 |1.5  |-0.1 |1.0  |-1.0 |1.5  |0.9  |1.3  |1.2  |-0.7 |0.6  |1.6   |-5.2  |
|2015 |-1.5 |-0.6 |0.9  |-1.6 |1.2  |1.6  |1.9  |0.4  |1.0  |0.8  |0.4  |-2.9 |1.4   |-5.6  |
|2016 |0.9  |-0.4 |0.4  |-2.4 |1.7  |-1.3 |-3.1 |1.7  |0.2  |1.9  |1.2  |-0.3 |0.5   |-6.0  |
|2017 |-1.2 |-0.3 |1.5  |0.7  |0.4  |-0.5 |-0.7 |0.1  |1.2  |1.1  |-0.2 |-0.1 |2.0   |-2.6  |
|2018 |-0.1 |-1.5 |0.9  |0.1  |-0.1 |1.1  |0.8  |0.3  |0.3  |0.1  |     |     |1.9   |-2.1  |
|Avg  |-1.2 |-0.4 |0.4  |-0.4 |0.5  |0.1  |0.2  |0.3  |0.7  |1.1  |0.6  |-0.4 |1.9   |-4.7  |
    


![plot of chunk plot-3](/public/images/2018-10-12-volatility-gb/plot-3-5.png)

Vol.6m strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |-0.2 |-1.0 |0.6  |0.8  |0.7  |-0.9 |-2.1 |0.6  |1.7  |-0.6 |-0.4  |-4.0  |
|2007 |-0.2 |0.0  |-0.2 |0.0  |-1.1 |1.3  |0.8  |0.6  |-1.6 |0.6  |1.3  |1.2  |2.8   |-3.4  |
|2008 |1.0  |0.6  |3.3  |-0.2 |-0.1 |-2.3 |4.9  |0.2  |9.8  |5.1  |0.7  |-1.7 |22.7  |-3.3  |
|2009 |-3.3 |-0.7 |-3.2 |-1.3 |-1.6 |-0.1 |-3.1 |-0.9 |-1.2 |3.1  |-0.8 |-0.6 |-13.1 |-15.2 |
|2010 |-2.0 |0.5  |-0.9 |1.0  |1.0  |1.2  |-0.6 |-0.7 |-0.5 |1.1  |0.9  |-0.2 |0.7   |-3.0  |
|2011 |-1.6 |0.0  |0.0  |1.0  |0.5  |-0.5 |-0.2 |1.6  |3.4  |2.1  |2.1  |1.2  |10.0  |-2.5  |
|2012 |-1.3 |-1.2 |-0.6 |1.4  |0.7  |0.0  |1.6  |-1.9 |-1.3 |-0.3 |0.8  |-1.2 |-3.4  |-5.5  |
|2013 |-1.3 |1.7  |0.4  |-2.7 |0.3  |1.2  |-1.9 |-0.2 |0.2  |-1.7 |1.1  |1.2  |-1.9  |-5.6  |
|2014 |-3.1 |-1.4 |1.0  |0.1  |1.6  |-0.9 |1.0  |0.6  |2.0  |1.1  |0.0  |-0.6 |1.4   |-5.3  |
|2015 |-1.9 |0.2  |1.1  |-0.9 |1.2  |0.8  |2.9  |1.2  |1.8  |0.5  |0.6  |-2.7 |4.7   |-5.5  |
|2016 |1.9  |-1.0 |0.8  |-2.8 |1.4  |-2.0 |-2.4 |1.8  |-0.7 |1.8  |0.7  |-0.8 |-1.6  |-6.5  |
|2017 |-0.6 |-0.1 |2.0  |0.1  |0.4  |-1.1 |-0.7 |-0.5 |1.6  |2.0  |-0.9 |-1.2 |0.9   |-3.1  |
|2018 |-0.6 |-1.5 |0.7  |0.9  |0.3  |0.3  |0.6  |0.9  |0.1  |0.0  |     |     |1.7   |-2.7  |
|Avg  |-1.1 |-0.2 |0.3  |-0.3 |0.4  |-0.1 |0.3  |0.1  |0.9  |1.2  |0.7  |-0.5 |1.9   |-5.0  |
    


![plot of chunk plot-3](/public/images/2018-10-12-volatility-gb/plot-3-6.png)


*(this report was produced on: 2018-12-23)*
