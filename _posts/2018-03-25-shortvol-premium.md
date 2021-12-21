---
layout: post
title:  Volatility premium
comments: false
---




The short volatility premium refers to a situation where the volatility implied by the prices of options is higher than the volatility that the underlying (stock or index for example)
realizes over the life of those options. The implied versus realized volatility premium has historically existed because of supply/demand imbalances, 
the tendency of investors to buy options for protection or directional views and compensation demanded by option sellers for bearing risk. 
These factors tend to raise the price of options, which increases the spread between implied volatility and the volatility that is realized. 

One way to capture the volatility premium is to sell options on index or stocks with delta hedging.

Given most recent perturbations in the market, lets take a look on relation between implied (will consider ATM S&P 500 volatility versus realized volatility) and realised volatility.


{% highlight r %}
#*****************************************************************
# Indexes of interest
#*****************************************************************
symbols    = spl('spx')

for(sym in symbols) {
  # read data
  data    = read_excel('data.xlsx', sheet = sym)
  data    = as.data.frame(data)
  data    = make.xts(data[,-1], as.Date(data[,1]))
  data    = data[,c(4,5,6)]*100
  names(data) = paste0(toupper(sym), c(' Implied',' Realized',' Implied - Realized'))
  
  #*****************************************************************
  # Plot Correlations full history
  #*****************************************************************
  qis.plot(data[,1], type ='l', ylim = range(data), main = paste0(toupper(sym)," 1M Implied ATM and Realized Volatility"),col = col.add.alpha('red', 150))
  qis.plot.lines(data[,2], type='l',col = col.add.alpha('blue', 150))
  qis.plot.lines(data[,3], type='l',lwd = '1',col = col.add.alpha('black', 150))
  qis.plot.legend(names(data),'red,blue,black',as.list(data))
  
  #*****************************************************************
  #  Plot Correlation for 2017-2018
  #*****************************************************************
  data = data['2017::2018']
  qis.plot(data[,1], type ='l', ylim = range(data), main = paste0(toupper(sym)," 1M Implied ATM and Realized Volatility"),col = col.add.alpha('red', 150))
  qis.plot.lines(data[,2], type='l',col = col.add.alpha('blue', 150))
  qis.plot.lines(data[,3], type='l',lwd = '1',col = col.add.alpha('black', 150))
  qis.plot.legend(names(data),'red,blue,black',as.list(data))
  
  #*****************************************************************
  # Average Spread
  #*****************************************************************
  tmp = last(qis.apply.matrix(data,mean))
  print(to.nice(tmp))
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-03-25-shortvol-premium/plot-2-1.png)![plot of chunk plot-2](/public/images/2018-03-25-shortvol-premium/plot-2-2.png)

|           |SPX Implied         |SPX Realized        |SPX Implied - Realized |
|:----------|:-------------------|:-------------------|:----------------------|
|2018-03-26 |9.08000000000000007 |8.24000000000000021 |0.83999999999999997    |
    

It is interesting to note that realized volatility significantly increased in the most recent months given various perturbations in the US markets.
Thus there is no surprise that many short volatility strategies significantly underperformed in the current regime.
But historically, at average, implied volatility is higher than the realize which give rise to so called short volatility premiums. 

*(this report was produced on: 2018-04-04)*
