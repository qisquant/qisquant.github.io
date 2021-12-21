---
layout: post
title: Quality Factor - Global
comments: true
---






Today I would like to construct long-short global quality factors using

 - [Quality Factor - North America](https://qisresearch.github.io/quality-na)
 - [Quality Factor - Europe](https://qisresearch.github.io/quality-eu)
 - [Quality Factor - Japan](https://qisresearch.github.io/quality-jp)
 - [Quality Factor - Asia](https://qisresearch.github.io/quality-ap)

Factors: 

 1. ROE  (Return on equity)
 2. OCFB (Operating cash flow to book price)

Strategy constrains:

 1. Weight each regional quality according to market capitalization
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
  qis.plot.strategy(models, plotX = T, log = '', LeftMargin = 3, main = 'Quality strategies', copyright = copyright)
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
    qis.plot.strategy.weights(models[[model.name]]$weight, name='Quality strategies')
  }
}
{% endhighlight %}



|           |Quality.Average   |Quality.OFCFB     |Quality.ROE       |
|:----------|:-----------------|:-----------------|:-----------------|
|Period     |Feb2006 - Oct2018 |Feb2006 - Oct2018 |Feb2006 - Oct2018 |
|Ave.Ret    |3.26              |2.17              |2.19              |
|Sharpe     |0.79              |0.65              |0.56              |
|Volatility |4.18              |3.4               |4                 |
|Turnover   |78.34             |73.16             |80.91             |
|MaxDD      |-9.84             |-8.33             |-9.7              |
|VaR        |-0.41             |-0.34             |-0.39             |
    


![plot of chunk plot-3](/public/images/2018-10-27-quality-gb/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-27-quality-gb/plot-3-2.png)

Quality.Average strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |     |-0.3 |1.1  |0.4  |-0.1 |0.5  |0.2  |0.8  |1.8  |-1.9 |0.3  |2.6  |-2.4  |
|2007 |0.2  |1.1  |1.3  |-0.4 |0.1  |-0.3 |-0.4 |0.1  |-0.8 |-2.3 |1.4  |0.0  |-0.2 |-4.7  |
|2008 |0.0  |-0.7 |0.7  |-1.1 |0.0  |2.1  |0.1  |-1.1 |-2.8 |-0.2 |2.5  |-0.7 |-1.2 |-5.5  |
|2009 |0.6  |3.5  |-1.9 |-0.8 |1.9  |0.6  |-0.9 |-1.7 |0.1  |2.5  |0.4  |-0.6 |3.5  |-5.0  |
|2010 |0.9  |1.3  |-0.9 |-0.2 |0.6  |-0.2 |0.6  |-1.2 |1.5  |0.4  |1.2  |-0.7 |3.3  |-1.7  |
|2011 |0.7  |-0.1 |0.5  |0.9  |1.2  |1.3  |-0.6 |1.4  |0.7  |0.4  |1.0  |1.2  |9.1  |-1.7  |
|2012 |1.7  |0.2  |2.1  |1.5  |0.9  |-1.7 |1.0  |-0.7 |-1.7 |0.3  |0.3  |-0.3 |3.5  |-4.4  |
|2013 |0.7  |0.9  |-0.4 |2.0  |0.2  |0.2  |0.6  |-0.8 |1.2  |0.2  |0.9  |-0.5 |5.2  |-2.5  |
|2014 |-2.6 |0.4  |1.3  |-0.2 |-0.5 |-1.9 |-1.2 |1.5  |1.0  |4.6  |-0.1 |0.9  |3.0  |-4.7  |
|2015 |0.1  |-0.7 |1.6  |-2.0 |0.6  |0.1  |3.7  |0.5  |1.7  |-0.4 |1.9  |0.1  |7.4  |-3.7  |
|2016 |1.8  |-1.7 |-1.4 |-4.4 |1.8  |-0.9 |-0.9 |0.2  |-1.6 |1.1  |1.1  |1.1  |-3.9 |-9.8  |
|2017 |-2.3 |1.5  |-0.2 |0.6  |1.1  |1.5  |-0.9 |0.1  |1.3  |-0.1 |2.3  |-0.6 |4.2  |-2.7  |
|2018 |1.3  |1.2  |0.6  |1.7  |1.5  |0.1  |1.5  |1.7  |-0.9 |-2.0 |     |     |6.9  |-4.0  |
|Avg  |0.2  |0.6  |0.2  |-0.1 |0.7  |0.1  |0.2  |0.0  |0.0  |0.5  |0.9  |0.0  |3.3  |-4.0  |
    


![plot of chunk plot-3](/public/images/2018-10-27-quality-gb/plot-3-3.png)

Quality.OFCFB strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |     |-0.1 |0.0  |0.2  |1.0  |-0.4 |1.1  |0.4  |0.5  |-0.4 |0.1  |2.5  |-1.0  |
|2007 |-0.1 |0.1  |1.4  |0.3  |1.5  |0.3  |1.5  |-1.0 |-0.1 |0.6  |1.8  |0.8  |7.3  |-2.3  |
|2008 |-1.8 |0.7  |-0.4 |-0.7 |-0.5 |1.2  |1.1  |-0.9 |-1.0 |1.6  |0.3  |0.0  |-0.5 |-3.6  |
|2009 |-1.0 |2.4  |-0.5 |-1.3 |2.3  |0.4  |-2.1 |0.3  |-0.1 |2.5  |0.5  |-0.8 |2.4  |-3.9  |
|2010 |0.1  |1.2  |-1.1 |0.3  |0.2  |0.6  |-0.3 |-0.3 |1.2  |-1.3 |0.1  |0.2  |0.9  |-2.3  |
|2011 |1.2  |0.8  |-0.1 |0.6  |0.9  |1.1  |0.2  |0.3  |-1.5 |1.7  |0.5  |2.6  |8.7  |-2.0  |
|2012 |1.7  |-1.3 |0.1  |0.8  |0.1  |0.7  |1.5  |0.0  |0.0  |1.0  |0.9  |-0.4 |5.2  |-2.3  |
|2013 |-0.2 |0.0  |-1.1 |1.9  |0.0  |-1.4 |0.1  |0.3  |-0.4 |0.8  |-0.3 |0.0  |-0.4 |-2.9  |
|2014 |-1.2 |0.6  |-0.5 |0.2  |-0.8 |-0.1 |-1.2 |0.8  |0.5  |2.5  |-1.0 |0.1  |-0.1 |-3.1  |
|2015 |1.1  |-0.5 |0.4  |-1.2 |0.9  |-1.0 |1.0  |-0.1 |-0.5 |-0.1 |3.2  |-1.5 |1.5  |-3.5  |
|2016 |1.4  |-3.3 |-1.0 |-2.1 |1.0  |-0.8 |-1.3 |0.4  |-0.6 |0.9  |0.3  |1.1  |-3.9 |-8.0  |
|2017 |-1.6 |1.5  |-0.7 |0.0  |0.2  |0.2  |-1.0 |-0.3 |0.1  |-0.6 |1.3  |0.6  |-0.4 |-2.9  |
|2018 |1.4  |0.9  |0.5  |0.0  |2.2  |0.6  |0.1  |1.9  |-0.6 |-0.9 |     |     |6.1  |-2.1  |
|Avg  |0.1  |0.3  |-0.2 |-0.1 |0.6  |0.2  |-0.1 |0.2  |-0.2 |0.7  |0.6  |0.2  |2.2  |-3.1  |
    


![plot of chunk plot-3](/public/images/2018-10-27-quality-gb/plot-3-4.png)

Quality.ROE strategy :
    




|     |Jan  |Feb  |Mar  |Apr  |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year |MaxDD |
|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|
|2006 |     |     |0.6  |0.0  |-0.3 |0.2  |-0.9 |1.2  |0.9  |1.0  |-1.7 |0.6  |1.6  |-2.2  |
|2007 |0.2  |1.6  |1.1  |0.5  |0.5  |-0.8 |0.0  |1.2  |-0.2 |-2.0 |2.0  |0.3  |4.4  |-3.1  |
|2008 |0.8  |-1.5 |0.1  |-1.5 |-0.3 |-0.7 |0.3  |-1.1 |-1.3 |0.2  |-1.6 |0.2  |-6.2 |-8.7  |
|2009 |-0.2 |2.5  |-0.8 |1.8  |2.1  |0.9  |-0.2 |-3.8 |-0.2 |2.8  |1.7  |-0.8 |5.7  |-5.4  |
|2010 |-0.9 |0.5  |-0.2 |-0.6 |0.4  |0.2  |-0.4 |0.1  |1.2  |0.6  |2.0  |-1.6 |1.3  |-1.9  |
|2011 |0.7  |-0.6 |0.4  |0.4  |0.6  |1.2  |0.8  |2.6  |0.8  |1.0  |1.2  |0.6  |10.1 |-1.3  |
|2012 |1.5  |-0.6 |-0.2 |1.7  |0.4  |-0.6 |2.1  |-0.5 |-0.7 |0.8  |0.1  |-1.2 |2.9  |-3.2  |
|2013 |-1.2 |0.2  |-0.3 |-1.0 |-0.7 |-0.8 |0.7  |-0.6 |0.1  |-0.7 |-0.1 |0.1  |-4.3 |-5.0  |
|2014 |-2.0 |-0.6 |0.9  |0.1  |-1.1 |-0.3 |-0.4 |0.9  |1.4  |3.6  |0.6  |0.3  |3.3  |-3.5  |
|2015 |-1.3 |-1.0 |1.1  |-1.5 |0.8  |-0.2 |1.6  |-0.4 |2.0  |1.4  |1.6  |0.1  |4.2  |-2.7  |
|2016 |1.6  |-0.4 |-0.2 |-5.4 |1.9  |-1.5 |-0.9 |1.0  |-1.1 |1.6  |0.9  |1.1  |-1.5 |-8.7  |
|2017 |-1.8 |1.4  |0.2  |0.2  |1.0  |0.9  |-0.6 |0.2  |1.6  |0.6  |0.8  |-0.3 |4.2  |-2.4  |
|2018 |0.7  |-0.1 |0.0  |0.1  |1.9  |0.6  |0.5  |1.3  |-0.5 |-0.5 |     |     |4.0  |-2.4  |
|Avg  |-0.2 |0.1  |0.2  |-0.4 |0.6  |-0.1 |0.2  |0.2  |0.3  |0.8  |0.6  |0.0  |2.3  |-3.9  |
    


![plot of chunk plot-3](/public/images/2018-10-27-quality-gb/plot-3-5.png)


*(this report was produced on: 2018-12-23)*
