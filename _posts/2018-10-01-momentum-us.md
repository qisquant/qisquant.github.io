---
layout: post
title: Momentum Factor - US
comments: true
---




Today I would like to construct long-short momentum factors using US universe of stocks.

Universe of stocks:  large and mid cap names of the US market with approximately 700 stocks.

Factors: 

 1. Mom.12m    (12m rolling returns)
 2. Mom.12m.1m (12m rolling returns with skipping most recent month)
 3. Mom.6m     (6m rolling returns)
 4. Mom.3m     (3m rolling momentum)
 
Signal:
 
 1. Momentum Z-scored on [GICS](https://en.wikipedia.org/wiki/Global_Industry_Classification_Standard) levels.
 2. Outliers winsorized.
 3. Buy high momentum names, sell low momentum names.

Strategy constrains:
 
 1. Single name weight +-1%.
 2. Overall leverage kept at 200%.
 3. Market beta neutrality at rebalance.
 4. Compute signal every month end.
 5. Execute trades on first business day of the month.

Academic literature:

 * [Risk Premia Harvesting Through Dual Momentum, 2016](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2042750). They show that combining absolute and relative momentum
   outperform absolute or relative momentum separately across asset classes.
   
 * [Momentum; Does adjusting by risk matter?, 2015](https://us.spindices.com/documents/education/insights-summer-2015-momentum.pdf). The paper examines whether risk 
   adjusted momentum score would benefit overall properties of the global and US momentum. They found that incorporating a stock's risk profile such as using
   volatility-adjusted momentum, can potentially improve the risk/return profile of a momentum strategy and possibly provide better downside protection.
 
 * [Value and Momentum Everywhere, 2013](http://pages.stern.nyu.edu/~lpederse/papers/ValMomEverywhere.pdf). In the paper they study the momentum and value factors across asset classes.
   They found a consistent value and momentum risk premia in every asset class such as equity stocks, equity indexes, currencies, fixed income and commodities.
   They argue that momentum and value works better together then separately for the portfolio construction purposes.
   
 * [Momentum Strategies, 1996](http://www-stat.wharton.upenn.edu/~steele/Courses/434/434Context/Momentum/MomentumStrategiesJF96.pdf). The main message of this paper
   is to say that there is an economically meaningful drift in future return for at least 6 months given prior six month return and the most recent earnings surprise.



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
  qis.plot.strategy(models, plotX = T, log = '', LeftMargin = 3, main = 'Momentum strategies', copyright = copyright)
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



|           |Mom.12m.1m        |Mom.12m           |Mom.3m            |Mom.6m            |
|:----------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |Jan2006 - Oct2018 |
|Ave.Ret    |1.08              |0.36              |-0.53             |0.8               |
|Sharpe     |0.16              |0.09              |0.01              |0.13              |
|Volatility |10.3              |11.06             |10.86             |10.71             |
|MarketBeta |0                 |-0.01             |-0.03             |-0.02             |
|Turnover   |1103.11           |1081.58           |1578.39           |1278.38           |
|MaxDD      |-52.58            |-56.53            |-58.39            |-55.49            |
|VaR        |-1.05             |-1.1              |-0.93             |-1.03             |
    


![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-2.png)

Mom.12m strategy : 
    




|     |Jan   |Feb  |Mar  |Apr   |May   |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec   |Year  |MaxDD |
|:----|:-----|:----|:----|:-----|:-----|:----|:----|:----|:----|:----|:----|:-----|:-----|:-----|
|2006 |      |0.1  |0.6  |-2.0  |-2.3  |1.5  |-0.2 |-3.3 |-0.7 |-0.2 |-1.9 |1.4   |-6.8  |-9.3  |
|2007 |0.5   |-1.4 |3.6  |-1.3  |-0.1  |0.9  |3.4  |3.6  |3.8  |4.9  |5.8  |7.1   |34.9  |-4.5  |
|2008 |-10.8 |4.9  |3.3  |1.5   |0.4   |11.0 |-3.3 |-4.2 |1.4  |2.6  |5.7  |-10.5 |-0.2  |-16.7 |
|2009 |-9.5  |0.4  |-7.5 |-19.9 |-10.2 |-1.7 |-3.2 |-5.8 |-1.9 |0.4  |-0.5 |0.1   |-46.9 |-48.1 |
|2010 |-4.1  |4.0  |0.6  |-0.6  |3.0   |1.7  |-0.2 |2.5  |-2.5 |2.3  |1.8  |-5.6  |2.5   |-5.7  |
|2011 |-3.3  |0.1  |0.2  |0.6   |1.2   |3.5  |2.6  |5.0  |1.3  |-0.9 |0.8  |0.5   |11.8  |-5.7  |
|2012 |-3.1  |-0.4 |1.2  |4.1   |1.0   |-0.2 |2.3  |-0.7 |-1.3 |-2.3 |2.5  |-1.6  |1.3   |-6.2  |
|2013 |-1.4  |2.5  |1.4  |-0.2  |0.4   |-0.4 |1.1  |-0.2 |1.7  |1.6  |1.8  |0.6   |9.2   |-3.3  |
|2014 |0.7   |0.8  |-1.6 |-1.6  |2.2   |0.5  |-2.3 |0.7  |1.8  |-1.2 |3.1  |0.9   |4.0   |-5.8  |
|2015 |4.5   |-2.6 |2.0  |-6.7  |4.3   |1.6  |6.2  |1.4  |5.1  |-1.3 |2.5  |0.7   |18.2  |-8.4  |
|2016 |-0.1  |-5.2 |-3.1 |0.7   |2.0   |0.3  |-2.4 |-0.5 |1.4  |-1.8 |-2.2 |-2.0  |-12.4 |-15.3 |
|2017 |0.4   |-0.4 |2.4  |2.5   |5.6   |-3.8 |0.4  |4.9  |-0.6 |5.2  |-1.3 |-2.6  |12.7  |-6.8  |
|2018 |3.7   |0.2  |0.8  |-0.5  |4.1   |-3.3 |-2.9 |4.4  |-0.4 |-3.5 |     |      |2.3   |-7.1  |
|Avg  |-1.9  |0.2  |0.3  |-1.8  |0.9   |0.9  |0.1  |0.6  |0.7  |0.4  |1.5  |-0.9  |2.4   |-11.0 |
    


![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-6.png)

Mom.12m.1m strategy : 
    




|     |Jan   |Feb  |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:-----|:----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |      |0.8  |0.8  |-1.6  |-2.0 |0.9  |-0.3 |-1.8 |0.4  |-0.2 |-0.9 |1.3  |-2.6  |-6.5  |
|2007 |1.2   |-1.5 |3.3  |-1.7  |-0.1 |0.5  |2.3  |1.7  |2.7  |5.1  |5.5  |6.1  |27.7  |-4.4  |
|2008 |-10.3 |6.1  |3.5  |3.1   |1.0  |9.4  |-1.7 |-4.1 |-1.1 |1.1  |1.4  |-8.8 |-2.2  |-18.9 |
|2009 |-7.7  |1.5  |-6.2 |-17.4 |-7.2 |-2.1 |-2.4 |-5.8 |-1.4 |-0.3 |0.3  |0.9  |-39.7 |-41.4 |
|2010 |-3.1  |3.5  |0.2  |-0.4  |2.5  |1.5  |-0.1 |2.3  |-2.7 |2.4  |1.8  |-4.2 |3.4   |-4.3  |
|2011 |-2.5  |0.6  |0.6  |0.9   |0.9  |3.7  |2.0  |4.1  |1.4  |-1.4 |1.1  |0.1  |12.0  |-4.9  |
|2012 |-3.6  |0.2  |0.5  |3.8   |0.1  |-0.6 |2.0  |0.1  |-0.9 |-1.7 |3.0  |-1.1 |1.7   |-5.5  |
|2013 |-1.4  |2.0  |0.9  |-0.3  |0.6  |-1.1 |1.2  |-0.3 |0.9  |1.3  |2.3  |0.1  |6.3   |-2.7  |
|2014 |0.0   |0.7  |-0.6 |-1.8  |3.0  |0.1  |-1.8 |0.4  |1.3  |-0.4 |2.9  |0.6  |4.6   |-5.4  |
|2015 |5.0   |-3.1 |1.9  |-5.5  |4.2  |0.7  |5.1  |1.1  |5.0  |-0.8 |2.3  |0.7  |17.2  |-8.0  |
|2016 |0.1   |-3.0 |-2.4 |1.0   |1.1  |0.8  |-2.4 |-0.1 |1.1  |-2.1 |-1.7 |-1.6 |-9.0  |-11.8 |
|2017 |1.1   |-0.1 |2.1  |1.8   |4.4  |-3.2 |1.3  |4.1  |-1.1 |4.9  |-2.4 |-2.7 |10.4  |-7.0  |
|2018 |3.0   |0.5  |0.8  |-0.5  |4.7  |-3.0 |-3.1 |4.1  |-0.5 |-3.7 |     |     |2.0   |-7.2  |
|Avg  |-1.5  |0.6  |0.4  |-1.4  |1.0  |0.6  |0.2  |0.5  |0.4  |0.3  |1.3  |-0.7 |2.4   |-9.9  |
    


![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-10.png)

Mom.3m strategy : 
    




|     |Jan  |Feb  |Mar  |Apr   |May   |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec   |Year  |MaxDD |
|:----|:----|:----|:----|:-----|:-----|:----|:----|:----|:----|:----|:----|:-----|:-----|:-----|
|2006 |     |0.0  |0.9  |-1.3  |-0.4  |1.0  |0.8  |-3.7 |-1.9 |-0.2 |0.2  |0.5   |-4.1  |-7.5  |
|2007 |0.4  |-0.6 |2.2  |-1.7  |1.2   |2.0  |2.9  |2.4  |5.5  |4.1  |5.0  |6.7   |34.2  |-4.9  |
|2008 |-4.2 |1.6  |3.0  |-1.7  |0.5   |9.9  |-4.6 |-3.5 |8.8  |4.9  |8.4  |-13.6 |6.9   |-17.3 |
|2009 |-9.5 |-1.5 |-6.5 |-21.6 |-10.7 |0.3  |3.7  |-3.1 |-3.0 |-1.9 |2.0  |-2.2  |-44.2 |-44.4 |
|2010 |-3.7 |1.0  |-1.8 |0.7   |3.6   |0.3  |-3.0 |1.1  |-2.1 |2.3  |0.1  |-3.2  |-4.9  |-6.3  |
|2011 |-2.8 |0.3  |-1.3 |0.2   |0.0   |1.8  |1.2  |4.5  |2.7  |0.1  |1.7  |0.4   |8.8   |-5.1  |
|2012 |-4.6 |-3.6 |1.3  |-1.4  |-0.5  |0.4  |-0.1 |-1.4 |0.1  |0.5  |-0.4 |-1.7  |-11.1 |-11.9 |
|2013 |-0.9 |2.3  |3.4  |-0.9  |-0.5  |1.2  |0.0  |-0.2 |1.6  |0.0  |-1.1 |0.6   |5.6   |-3.3  |
|2014 |-0.1 |1.9  |-2.6 |-1.4  |1.6   |-0.7 |-0.2 |0.7  |1.3  |-0.9 |5.3  |3.0   |7.8   |-4.7  |
|2015 |3.0  |-0.8 |3.0  |-6.3  |1.3   |3.2  |0.8  |0.6  |4.8  |-0.5 |-0.2 |0.8   |9.8   |-7.2  |
|2016 |0.0  |-5.3 |-0.7 |-0.3  |0.5   |0.1  |-0.9 |-0.5 |1.1  |-2.5 |0.8  |0.8   |-6.9  |-11.0 |
|2017 |-2.7 |-0.5 |1.7  |1.3   |3.3   |-1.7 |-0.1 |3.0  |-0.1 |4.1  |0.8  |-1.0  |8.3   |-4.2  |
|2018 |1.7  |1.0  |-1.6 |1.3   |3.9   |-0.9 |-3.3 |2.4  |1.3  |-0.8 |     |      |4.9   |-5.7  |
|Avg  |-2.0 |-0.3 |0.1  |-2.6  |0.3   |1.3  |-0.2 |0.2  |1.6  |0.7  |1.9  |-0.7  |1.2   |-10.3 |
    


![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-11.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-12.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-13.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-14.png)

Mom.6m strategy : 
    




|     |Jan   |Feb  |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec   |Year  |MaxDD |
|:----|:-----|:----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|:-----|
|2006 |      |-0.6 |1.0  |-1.2  |-0.5 |2.3  |1.3  |-3.3 |-1.2 |0.3  |-0.8 |0.3   |-2.4  |-7.7  |
|2007 |1.1   |-2.5 |2.3  |0.0   |1.7  |3.0  |2.4  |2.7  |5.7  |4.8  |6.0  |5.3   |37.3  |-4.8  |
|2008 |-9.3  |3.2  |3.7  |-0.6  |-0.3 |10.2 |-2.4 |-3.1 |5.7  |3.4  |8.8  |-12.8 |4.1   |-16.9 |
|2009 |-12.0 |1.0  |-8.5 |-21.4 |-8.4 |-0.5 |-2.4 |-2.7 |-0.9 |1.1  |3.3  |-1.6  |-43.7 |-46.4 |
|2010 |-1.7  |3.2  |-1.3 |-0.2  |3.3  |0.6  |-2.3 |1.3  |-2.4 |1.4  |2.0  |-5.2  |-1.5  |-5.6  |
|2011 |-2.8  |0.9  |-0.5 |0.8   |0.7  |1.5  |1.5  |5.3  |2.3  |-0.7 |1.0  |-0.7  |9.6   |-4.0  |
|2012 |-3.5  |-2.7 |2.5  |0.7   |1.4  |-0.5 |1.1  |-0.4 |-0.5 |-1.9 |-0.2 |-3.5  |-7.5  |-7.7  |
|2013 |-0.8  |2.2  |2.6  |-0.8  |0.5  |0.5  |0.7  |0.5  |1.5  |0.9  |1.1  |-0.2  |9.0   |-2.3  |
|2014 |0.8   |2.3  |-1.6 |-2.0  |1.4  |0.4  |-1.3 |-0.2 |1.7  |0.1  |2.9  |2.3   |6.7   |-5.6  |
|2015 |2.6   |-1.1 |2.2  |-7.0  |4.4  |3.7  |2.2  |0.8  |4.6  |-1.2 |1.2  |0.3   |12.8  |-7.8  |
|2016 |-0.7  |-3.4 |-1.5 |-0.2  |2.0  |3.5  |-1.3 |-1.4 |0.0  |-2.9 |-0.1 |-0.8  |-6.7  |-9.4  |
|2017 |-1.5  |0.1  |0.6  |0.2   |4.9  |-3.7 |1.3  |5.8  |-1.9 |5.9  |-0.3 |-0.7  |10.7  |-5.3  |
|2018 |2.9   |0.8  |-0.1 |0.0   |3.5  |-1.9 |-3.3 |5.0  |0.4  |-2.3 |     |      |4.7   |-6.7  |
|Avg  |-2.1  |0.3  |0.1  |-2.4  |1.1  |1.5  |-0.2 |0.8  |1.2  |0.7  |2.1  |-1.5  |2.5   |-10.0 |
    


![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-15.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-16.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-17.png)![plot of chunk plot-3](/public/images/2018-10-01-momentum-us/plot-3-18.png)


*(this report was produced on: 2018-10-30)*
