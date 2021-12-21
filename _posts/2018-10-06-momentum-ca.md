---
layout: post
title: Momentum Factor - Canada
comments: true
---




Today I would like to construct different long-short momentum factors using Canada universe of stocks.

Universe of stocks:  large and mid cap names of the Canada markets with approximately 100 stocks.

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
 
 1. Single name weight +-6%.
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
    qis.plot.xts(tmp['2006-03::'], main = paste0(model.name, " # of names in the portfolio"), copyright = copyright, ylim=c(10,100))
    
    # plot running maximum and minimum weight in portfolio
    tmp1 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'long', type = 'max', single = T); names(tmp1) = 'Max.Long'
    tmp2 = qis.compute.weights.stat(models[[model.name]]$weight, side = 'short', type = 'min', single = T);names(tmp2) = 'Min.Short'
    tmp  = merge(tmp1,tmp2)
    qis.plot.xts(tmp[tmp>0], main = paste0(model.name, " single name exposure"), copyright = copyright,  ylim=c(-0.1,0.1))
    
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
|Ave.Ret    |0.83              |0.94              |-1.93             |-0.91             |
|Sharpe     |0.13              |0.14              |-0.11             |-0.01             |
|Volatility |12.43             |13.19             |11.51             |12.05             |
|MarketBeta |0                 |-0.01             |-0.02             |-0.02             |
|Turnover   |748.94            |754.29            |881.34            |802.22            |
|MaxDD      |-46.74            |-51.38            |-48.83            |-44               |
|VaR        |-1.11             |-1.13             |-1.04             |-1.11             |
    


![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-2.png)

Mom.12m strategy : 
    




|     |Jan  |Feb   |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:-----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |0.6   |-0.4 |0.6   |-4.5 |1.4  |0.7  |3.9  |0.5  |-1.4 |3.2  |-2.1 |2.3   |-6.6  |
|2007 |0.2  |0.0   |1.7  |-2.8  |-1.1 |-1.8 |1.6  |5.1  |-1.4 |-2.5 |3.0  |4.9  |6.6   |-8.0  |
|2008 |-5.2 |8.1   |2.2  |-2.7  |3.2  |7.1  |-2.1 |-2.4 |6.4  |9.7  |-1.9 |5.4  |29.8  |-9.8  |
|2009 |2.4  |-22.3 |-7.2 |-19.1 |-3.7 |0.1  |1.6  |1.7  |-0.2 |0.9  |1.2  |-1.1 |-40.1 |-49.5 |
|2010 |-2.0 |-2.0  |1.7  |-2.3  |-4.6 |1.3  |0.7  |4.9  |-2.6 |0.9  |-1.0 |-0.2 |-5.4  |-10.1 |
|2011 |0.1  |-3.0  |-0.7 |1.2   |1.3  |6.0  |0.6  |0.0  |6.9  |4.7  |4.3  |1.1  |24.5  |-4.7  |
|2012 |-2.5 |0.6   |-1.9 |1.4   |3.1  |-0.5 |3.5  |-3.5 |-2.4 |-2.0 |-1.2 |-0.5 |-6.1  |-11.3 |
|2013 |2.0  |-0.3  |-1.6 |5.5   |-4.5 |2.2  |-0.7 |-0.1 |0.4  |-0.1 |4.8  |2.4  |10.0  |-6.1  |
|2014 |0.3  |1.6   |-0.7 |1.2   |-1.8 |0.4  |0.4  |-0.8 |3.0  |5.7  |1.6  |2.6  |14.4  |-4.5  |
|2015 |9.4  |-0.8  |4.6  |-5.8  |2.1  |3.6  |14.7 |3.2  |5.3  |-8.3 |-0.7 |3.0  |32.3  |-14.3 |
|2016 |2.4  |-7.9  |-7.8 |-14.2 |6.1  |-0.4 |-4.1 |2.2  |0.3  |4.0  |-1.7 |-1.8 |-22.1 |-27.8 |
|2017 |0.3  |0.0   |1.6  |-4.0  |-0.9 |2.9  |2.9  |0.8  |-2.2 |-1.4 |0.5  |0.4  |0.6   |-6.7  |
|2018 |-0.9 |2.4   |-2.8 |-3.0  |-0.2 |-0.2 |2.0  |0.8  |-1.2 |-4.4 |     |     |-7.5  |-9.6  |
|Avg  |0.5  |-1.8  |-0.9 |-3.4  |-0.4 |1.7  |1.7  |1.2  |1.0  |0.4  |1.0  |1.2  |3.0   |-13.0 |
    


![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-6.png)

Mom.12m.1m strategy : 
    




|     |Jan  |Feb   |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct   |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:-----|:----|:-----|:----|:----|:----|:----|:----|:-----|:----|:----|:-----|:-----|
|2006 |     |0.0   |0.2  |1.2   |-4.2 |1.4  |-0.1 |3.5  |-1.0 |-2.0  |2.6  |-1.4 |0.2   |-7.0  |
|2007 |-0.4 |0.2   |2.0  |-3.1  |-1.3 |-2.0 |2.0  |4.3  |-0.9 |-1.4  |3.3  |5.1  |7.7   |-8.7  |
|2008 |-6.0 |8.2   |3.2  |-1.1  |2.0  |6.9  |-0.9 |-2.7 |5.5  |6.8   |-1.6 |7.9  |30.3  |-7.9  |
|2009 |3.1  |-19.3 |-2.1 |-17.5 |-2.6 |-1.5 |-1.0 |2.1  |0.4  |0.2   |1.4  |-1.9 |-34.9 |-42.9 |
|2010 |-2.1 |-1.6  |0.6  |-0.8  |-4.9 |0.1  |3.4  |2.6  |-1.9 |0.3   |-0.6 |2.1  |-3.1  |-10.0 |
|2011 |-0.2 |-1.9  |-1.4 |0.5   |3.5  |7.3  |-0.3 |-0.3 |5.6  |5.7   |4.4  |2.5  |27.8  |-4.7  |
|2012 |-2.4 |0.6   |-2.5 |1.5   |3.6  |-1.0 |1.3  |-0.9 |-0.6 |-1.8  |-3.1 |-1.2 |-6.5  |-11.0 |
|2013 |2.0  |-1.2  |-1.8 |4.1   |-4.7 |-0.3 |-0.3 |0.1  |0.9  |0.4   |3.3  |3.0  |5.4   |-6.4  |
|2014 |0.9  |1.2   |1.5  |1.3   |-0.7 |-0.3 |-1.1 |-1.6 |2.2  |4.6   |-0.3 |-0.9 |6.8   |-5.3  |
|2015 |7.7  |0.6   |4.5  |-5.2  |2.4  |3.4  |15.1 |2.5  |3.8  |-10.5 |-0.6 |1.7  |25.9  |-13.9 |
|2016 |2.4  |-7.5  |-8.0 |-13.5 |5.1  |-0.2 |-3.6 |-0.2 |1.6  |3.7   |-0.7 |-1.6 |-21.7 |-27.3 |
|2017 |0.9  |-0.8  |2.6  |-3.8  |-1.4 |2.9  |3.7  |1.3  |-3.2 |-1.9  |1.6  |0.5  |2.0   |-7.4  |
|2018 |-1.3 |2.6   |-1.8 |-1.7  |0.0  |-0.3 |1.2  |-0.8 |-1.1 |-3.9  |     |     |-7.0  |-8.9  |
|Avg  |0.4  |-1.5  |-0.2 |-2.9  |-0.3 |1.3  |1.5  |0.8  |0.9  |0.0   |0.8  |1.3  |2.5   |-12.4 |
    


![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-8.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-9.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-10.png)

Mom.3m strategy : 
    




|     |Jan  |Feb   |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:-----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |0.9   |-1.1 |0.2   |-3.4 |0.8  |0.9  |1.0  |0.4  |0.2  |-2.7 |-3.2 |-6.1  |-7.1  |
|2007 |1.4  |0.2   |-0.3 |-2.2  |0.6  |1.4  |0.7  |2.8  |-0.7 |0.7  |4.8  |6.6  |17.0  |-5.9  |
|2008 |0.3  |5.3   |2.9  |-7.0  |2.8  |6.1  |-6.1 |-2.3 |2.8  |4.5  |-2.2 |7.0  |13.9  |-10.9 |
|2009 |4.8  |-18.1 |-6.5 |-16.6 |-7.6 |1.2  |3.4  |-0.7 |-0.9 |0.6  |2.1  |-1.8 |-35.8 |-46.6 |
|2010 |-1.7 |1.1   |0.0  |-1.6  |-1.3 |3.7  |-5.1 |8.6  |-2.9 |-0.2 |-0.2 |1.3  |1.2   |-7.0  |
|2011 |-3.9 |-3.3  |-2.0 |0.1   |-0.2 |4.6  |-1.5 |4.4  |4.9  |4.3  |5.0  |3.7  |16.7  |-10.4 |
|2012 |-1.6 |0.0   |-1.0 |-0.4  |2.5  |1.8  |1.1  |-5.0 |-2.6 |-0.3 |-1.5 |-1.4 |-8.3  |-12.3 |
|2013 |2.7  |0.0   |0.8  |4.0   |-0.4 |-0.2 |-3.4 |-0.1 |0.0  |-1.1 |0.9  |0.4  |3.4   |-8.1  |
|2014 |1.3  |2.3   |-0.6 |-1.8  |0.8  |1.5  |-1.6 |-2.1 |0.8  |6.0  |-3.5 |0.9  |3.9   |-7.3  |
|2015 |4.6  |-5.3  |1.4  |-5.4  |1.2  |0.8  |7.1  |1.7  |-0.9 |-7.3 |-0.5 |1.5  |-2.2  |-12.1 |
|2016 |2.3  |-6.1  |-6.0 |-0.8  |0.5  |7.8  |0.7  |-2.1 |1.3  |2.5  |-0.6 |-2.7 |-3.9  |-14.1 |
|2017 |2.4  |-0.7  |0.2  |-4.2  |-3.0 |1.4  |1.6  |0.9  |-2.8 |-2.4 |1.5  |-0.1 |-5.3  |-10.2 |
|2018 |-0.2 |1.3   |-2.2 |-2.6  |-0.5 |-0.5 |1.0  |-0.8 |-1.7 |-0.8 |     |     |-7.0  |-9.1  |
|Avg  |1.0  |-1.7  |-1.1 |-2.9  |-0.6 |2.3  |-0.1 |0.5  |-0.2 |0.5  |0.3  |1.0  |-1.0  |-12.4 |
    


![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-11.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-12.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-13.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-14.png)

Mom.6m strategy : 
    




|     |Jan  |Feb   |Mar  |Apr   |May  |Jun  |Jul  |Aug  |Sep  |Oct  |Nov  |Dec  |Year  |MaxDD |
|:----|:----|:-----|:----|:-----|:----|:----|:----|:----|:----|:----|:----|:----|:-----|:-----|
|2006 |     |1.8   |-0.3 |1.6   |-2.1 |-0.7 |1.8  |2.9  |0.1  |-2.1 |-1.6 |-4.2 |-3.1  |-8.8  |
|2007 |3.1  |1.2   |1.7  |-1.4  |2.3  |-0.2 |1.2  |2.3  |-1.4 |-1.0 |2.0  |5.4  |16.1  |-4.2  |
|2008 |0.0  |7.4   |-0.7 |-5.0  |4.2  |6.7  |-2.1 |-0.3 |-0.1 |6.9  |0.1  |-0.6 |16.7  |-12.6 |
|2009 |0.0  |-16.7 |-1.8 |-20.9 |1.4  |-3.9 |-0.9 |2.4  |-0.6 |1.1  |2.6  |1.0  |-33.4 |-41.6 |
|2010 |-1.7 |1.2   |1.0  |-0.4  |-1.7 |2.6  |-1.6 |6.6  |-4.5 |-1.9 |-0.3 |1.1  |0.0   |-8.5  |
|2011 |-0.2 |-3.5  |-0.3 |-0.6  |2.0  |5.2  |-1.4 |1.0  |5.7  |4.6  |5.2  |1.9  |20.9  |-5.7  |
|2012 |-3.9 |-1.5  |-4.1 |1.5   |3.1  |0.7  |1.1  |-4.5 |-3.9 |0.4  |-0.4 |-0.4 |-11.6 |-12.0 |
|2013 |1.4  |-1.3  |0.6  |1.5   |-0.9 |-0.2 |-2.9 |-0.2 |1.2  |-0.2 |2.6  |1.4  |2.9   |-5.9  |
|2014 |1.3  |2.8   |1.8  |0.1   |0.2  |-0.1 |-0.8 |-0.8 |3.6  |7.0  |-3.3 |0.3  |12.3  |-7.2  |
|2015 |3.1  |-4.6  |3.8  |-4.9  |1.9  |2.8  |12.9 |2.2  |2.1  |-8.0 |-1.2 |2.6  |11.6  |-12.5 |
|2016 |4.6  |-9.0  |-5.6 |-6.0  |0.5  |4.9  |-0.9 |-2.1 |-0.2 |2.9  |-1.7 |-2.1 |-14.6 |-20.3 |
|2017 |2.4  |-2.0  |1.6  |-4.4  |0.1  |2.5  |1.8  |0.1  |-2.5 |-2.9 |1.2  |-0.6 |-2.9  |-7.2  |
|2018 |0.3  |3.7   |0.0  |-3.1  |-0.9 |-2.2 |1.0  |-2.0 |-4.8 |-3.7 |     |     |-11.3 |-16.0 |
|Avg  |0.9  |-1.6  |-0.2 |-3.2  |0.8  |1.4  |0.7  |0.6  |-0.4 |0.2  |0.4  |0.5  |0.3   |-12.5 |
    


![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-15.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-16.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-17.png)![plot of chunk plot-3](/public/images/2018-10-06-momentum-ca/plot-3-18.png)


*(this report was produced on: 2018-10-30)*
