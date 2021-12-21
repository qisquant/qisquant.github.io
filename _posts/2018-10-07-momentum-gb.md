---
layout: post
title: Momentum Factor  - Global
comments: true
---






Today I would like to construct long-short global momentum factors using

 - [Momentum Factor - North America](https://qisresearch.github.io/momentum-na)
 - [Momentum Factor - Europe](https://qisresearch.github.io/momentum-eu)
 - [Momentum Factor - Japan](https://qisresearch.github.io/momentum-jp)
 - [Momentum Factor - Asia](https://qisresearch.github.io/momentum-ap)

Factors: 

 1. Mom.12m    (12m rolling returns)
 2. Mom.12m.1m (12m rolling returns with skipping most recent month)
 3. Mom.6m     (6m rolling returns)
 4. Mom.3m     (3m rolling momentum)

Strategy constrains:

 1. Weight each regional momentum according to market capitalization
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
  qis.plot.strategy(models, plotX = T, log = '', LeftMargin = 3, main = 'Momentum strategies', copyright = copyright)
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
    qis.plot.strategy.weights(models[[model.name]]$weight, name='Momentum strategies')
  }
}
{% endhighlight %}



|           |Mom.12m.1m        |Mom.12m           |Mom.3m            |Mom.6m            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Feb2006 - Oct2018 |Feb2006 - Oct2018 |Feb2006 - Oct2018 |Feb2006 - Oct2018 |
|Ave.Ret    |3.51              |3.71              |1.45              |2.71              |
|Sharpe     |0.46              |0.46              |0.21              |0.36              |
|Volatility |8.23              |8.81              |8.44              |8.53              |
|Turnover   |110.15            |114.09            |107.29            |109.6             |
|MaxDD      |-44.39            |-44.44            |-46.61            |-45.8             |
|VaR        |-0.82             |-0.85             |-0.79             |-0.86             |
    


![plot of chunk plot-3](/public/images/2018-10-07-momentum-gb/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-07-momentum-gb/plot-3-2.png)

Mom.12m strategy :
    




|     |Jan  |Feb  |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |0.5  |1.3   |-1.9 |0.3  |0.7  |-2.2 |0.0  |1.0  |0.2  |0.8  |0.5   |-6.4  |
|2007 |-0.6 |-1.0 |2.9  |0.3   |0.1  |0.3  |2.6  |2.4  |5.6  |3.7  |4.8  |5.0  |29.0  |-3.4  |
|2008 |-7.5 |2.8  |1.7  |1.4   |3.3  |10.3 |-1.5 |-7.5 |2.3  |4.7  |4.0  |-7.6 |4.8   |-15.6 |
|2009 |-3.5 |0.9  |-6.6 |-17.0 |-7.7 |-1.7 |-0.6 |-6.8 |-1.7 |1.5  |1.7  |0.3  |-35.4 |-38.9 |
|2010 |-2.4 |3.4  |0.2  |0.5   |3.4  |1.9  |-2.4 |2.5  |0.1  |1.6  |3.2  |-3.6 |8.5   |-3.8  |
|2011 |-4.2 |-0.7 |1.2  |2.4   |1.9  |4.2  |3.1  |3.8  |0.9  |0.9  |3.6  |2.1  |20.7  |-6.3  |
|2012 |-4.3 |-1.4 |1.2  |4.7   |3.1  |-0.6 |3.7  |-2.1 |-1.0 |-1.0 |2.6  |-2.0 |2.6   |-7.7  |
|2013 |-0.8 |2.9  |1.7  |0.8   |-0.2 |1.8  |-0.6 |-1.7 |1.4  |1.7  |2.7  |1.3  |11.4  |-4.0  |
|2014 |-0.8 |1.2  |-1.0 |-2.0  |1.3  |0.7  |-0.2 |0.8  |1.9  |0.8  |0.7  |1.1  |4.4   |-5.0  |
|2015 |4.4  |-3.5 |2.2  |-5.6  |3.6  |2.3  |6.6  |2.1  |5.9  |-3.2 |2.0  |1.7  |19.3  |-10.3 |
|2016 |0.8  |-4.8 |-3.2 |-2.0  |2.4  |0.7  |-1.4 |-1.5 |1.7  |-1.1 |-1.8 |-3.0 |-12.7 |-15.2 |
|2017 |1.9  |-1.0 |1.5  |1.2   |3.1  |-1.9 |0.5  |4.6  |-1.2 |4.7  |-0.6 |-3.1 |9.9   |-5.4  |
|2018 |3.5  |0.5  |0.3  |-1.1  |3.6  |-1.8 |-3.1 |3.5  |-0.2 |-2.1 |     |     |2.8   |-5.4  |
|Avg  |-1.1 |-0.1 |0.2  |-1.2  |1.2  |1.3  |0.6  |-0.2 |1.2  |1.0  |1.9  |-0.6 |5.1   |-9.8  |
    


![plot of chunk plot-3](/public/images/2018-10-07-momentum-gb/plot-3-3.png)

Mom.12m.1m strategy :
    




|     |Jan  |Feb  |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |0.2  |1.1   |-1.4 |0.5  |-0.4 |-1.8 |-0.1 |0.8  |0.6  |1.5  |0.9   |-5.7  |
|2007 |-0.8 |-0.9 |2.3  |0.2   |-0.4 |-0.6 |1.6  |1.3  |5.0  |2.9  |4.6  |4.3  |21.0  |-3.3  |
|2008 |-8.5 |4.0  |1.2  |2.3   |3.4  |9.1  |-0.9 |-6.8 |-0.2 |1.4  |0.8  |-6.4 |-1.9  |-15.8 |
|2009 |-1.7 |-0.7 |-6.3 |-16.0 |-4.8 |-1.0 |-1.1 |-6.0 |-1.7 |2.2  |1.2  |0.6  |-31.2 |-34.3 |
|2010 |-1.7 |2.7  |0.6  |0.6   |3.1  |1.1  |-1.6 |2.2  |0.0  |1.4  |3.3  |-2.9 |8.9   |-3.1  |
|2011 |-3.3 |0.4  |1.5  |2.4   |2.0  |4.2  |3.0  |3.2  |0.7  |1.4  |3.8  |1.8  |22.9  |-5.3  |
|2012 |-3.7 |-1.1 |0.9  |5.0   |2.5  |-0.3 |3.4  |-1.6 |-1.4 |-0.9 |2.8  |-1.7 |3.6   |-7.6  |
|2013 |-1.0 |2.8  |1.3  |0.8   |0.4  |1.6  |0.0  |-2.0 |1.5  |1.6  |3.1  |0.7  |11.2  |-4.2  |
|2014 |-0.6 |1.1  |-0.5 |-2.0  |1.8  |0.6  |-0.5 |0.3  |1.7  |0.9  |1.0  |0.9  |4.8   |-4.5  |
|2015 |4.2  |-2.4 |1.6  |-4.9  |3.3  |1.6  |6.2  |1.5  |5.7  |-2.3 |1.9  |2.2  |19.6  |-9.5  |
|2016 |0.9  |-4.0 |-2.7 |-2.3  |2.5  |0.3  |-1.2 |-0.8 |1.6  |-0.8 |-1.9 |-3.1 |-11.1 |-14.0 |
|2017 |2.9  |-0.7 |1.4  |0.7   |2.8  |-1.7 |0.7  |3.9  |-1.4 |3.7  |-1.6 |-3.1 |7.7   |-5.3  |
|2018 |2.6  |0.6  |0.7  |-0.7  |4.1  |-2.1 |-2.6 |3.6  |-0.4 |-2.5 |     |     |3.2   |-5.3  |
|Avg  |-0.9 |0.2  |0.2  |-1.0  |1.5  |1.0  |0.5  |-0.2 |0.8  |0.7  |1.6  |-0.4 |4.6   |-9.1  |
    


![plot of chunk plot-3](/public/images/2018-10-07-momentum-gb/plot-3-4.png)

Mom.3m strategy :
    




|     |Jan  |Feb  |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |     |0.6  |1.5   |-0.2 |0.4  |0.6  |-3.2 |-0.4 |0.8  |0.4  |0.6  |1.1   |-4.3  |
|2007 |-0.4 |-1.2 |3.3  |-1.0  |-0.5 |1.5  |1.8  |1.1  |6.6  |3.8  |6.1  |5.1  |29.1  |-2.8  |
|2008 |-2.3 |0.5  |1.0  |-3.3  |1.6  |7.7  |-2.8 |-6.0 |6.4  |9.2  |5.4  |-8.8 |7.1   |-14.1 |
|2009 |-4.7 |0.6  |-7.3 |-19.0 |-6.9 |-0.1 |2.7  |-3.9 |-1.5 |-1.4 |1.6  |-0.6 |-35.2 |-36.4 |
|2010 |-2.6 |1.1  |-1.0 |0.4   |3.0  |1.7  |-5.3 |1.0  |-0.2 |1.1  |0.6  |-1.7 |-2.1  |-7.2  |
|2011 |-4.1 |-0.8 |0.2  |0.4   |1.1  |2.7  |2.2  |4.3  |1.9  |0.1  |3.4  |2.6  |14.8  |-5.8  |
|2012 |-5.5 |-2.9 |1.3  |0.3   |1.7  |-0.1 |1.9  |-2.9 |-0.2 |1.0  |0.3  |-2.0 |-7.0  |-9.1  |
|2013 |-0.4 |2.2  |2.5  |0.3   |-0.7 |2.7  |-0.2 |-1.3 |0.9  |0.4  |-1.0 |1.1  |6.8   |-3.2  |
|2014 |-0.5 |0.9  |-2.5 |-2.1  |0.3  |-0.5 |0.3  |0.5  |1.5  |1.0  |2.2  |3.2  |4.2   |-5.7  |
|2015 |3.4  |-2.8 |2.4  |-6.2  |0.6  |2.6  |0.9  |0.5  |5.5  |-2.2 |-0.3 |2.1  |6.3   |-8.0  |
|2016 |0.4  |-3.1 |-1.5 |1.1   |-0.6 |3.6  |-0.4 |-1.3 |1.6  |-1.5 |0.4  |-1.1 |-2.4  |-7.9  |
|2017 |-1.8 |-0.3 |0.9  |1.6   |2.6  |-0.1 |-1.3 |3.6  |-1.5 |3.6  |0.7  |-1.8 |6.1   |-3.9  |
|2018 |1.6  |1.2  |-1.5 |0.1   |2.9  |0.3  |-3.3 |2.3  |0.5  |0.0  |     |     |4.0   |-3.9  |
|Avg  |-1.4 |-0.4 |-0.1 |-2.0  |0.4  |1.7  |-0.2 |-0.4 |1.6  |1.2  |1.6  |-0.1 |2.5   |-8.6  |
    


![plot of chunk plot-3](/public/images/2018-10-07-momentum-gb/plot-3-5.png)

Mom.6m strategy :
    




|     |Jan  |Feb  |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov |Dec  |Year  |MaxDD |
|:----|:----|:----|:----|:-----|:----|:----|:----|:----|:----|:----|:---|:----|:-----|:-----|
|2006 |     |     |0.5  |1.3   |-0.7 |0.8  |1.3  |-3.0 |-0.7 |0.9  |0.9 |1.0  |2.2   |-5.2  |
|2007 |-0.9 |-1.2 |2.7  |-0.9  |0.1  |1.4  |2.5  |1.5  |6.9  |3.2  |7.0 |4.4  |29.7  |-3.1  |
|2008 |-4.6 |1.0  |2.1  |-0.3  |1.4  |8.7  |-1.9 |-6.4 |3.3  |6.6  |5.5 |-8.3 |5.7   |-13.2 |
|2009 |-6.2 |0.4  |-7.8 |-18.4 |-5.6 |-2.1 |-0.5 |-4.2 |-1.0 |1.2  |1.2 |-0.4 |-36.9 |-39.2 |
|2010 |-1.1 |2.8  |-0.9 |0.1   |3.6  |1.1  |-4.0 |2.3  |-0.6 |0.6  |2.2 |-3.6 |2.1   |-5.5  |
|2011 |-3.8 |-0.1 |0.4  |1.0   |1.4  |2.6  |1.3  |4.1  |2.1  |0.0  |3.8 |1.2  |14.5  |-5.7  |
|2012 |-4.4 |-2.1 |2.0  |2.9   |4.1  |-0.7 |2.6  |-2.4 |-0.6 |-0.5 |0.2 |-3.3 |-2.5  |-7.8  |
|2013 |-0.5 |2.4  |1.3  |0.3   |-0.5 |2.9  |-0.8 |-1.8 |2.3  |0.4  |1.3 |0.6  |8.1   |-4.6  |
|2014 |0.8  |2.0  |-1.3 |-2.3  |0.5  |-0.1 |-0.2 |0.2  |2.3  |2.0  |0.9 |2.2  |7.1   |-5.0  |
|2015 |2.9  |-2.5 |2.8  |-6.9  |3.3  |3.2  |3.6  |1.1  |5.0  |-3.2 |1.5 |1.2  |12.0  |-8.8  |
|2016 |0.4  |-2.1 |-1.9 |0.2   |1.1  |5.4  |0.7  |-2.6 |1.5  |-1.8 |0.0 |-2.3 |-1.7  |-8.3  |
|2017 |-0.3 |-0.2 |0.0  |1.0   |3.6  |-2.2 |0.6  |4.7  |-2.1 |4.6  |0.3 |-1.8 |8.2   |-4.0  |
|2018 |2.4  |1.3  |-0.5 |-0.6  |2.5  |-1.0 |-3.8 |3.5  |0.0  |-1.5 |    |     |2.0   |-5.4  |
|Avg  |-1.3 |0.1  |0.0  |-1.7  |1.1  |1.5  |0.1  |-0.2 |1.4  |1.0  |2.1 |-0.8 |3.9   |-8.9  |
    


![plot of chunk plot-3](/public/images/2018-10-07-momentum-gb/plot-3-6.png)


*(this report was produced on: 2018-12-23)*
