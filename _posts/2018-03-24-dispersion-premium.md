---
layout: post
title: Dispersion premium
comments: false
---




Lets take look on the structural market flows in option market. 

1. Index options buyers.
   -  Portfolio hedgers. They will typically buy index options to hedge their portfolios. 
   -  The principal protected notes will have embedded long position in the index options
2.	Exotic product buyers.
   -  Different notes which would have best of (put), worst of (call) structures. 
   -  Basket options 
3.	Stock options sellers:
   - Overwriters. (They sell out of money  options to collect premium with the hope options will not be exercised).
      One of the such strategy can be found on [CBOE](http://www.cboe.com/micro/Buywrite/VolatilityPremiumthroughCallOverwriting2012.pdf).
   - [Reverse convertible notes](https://www.investopedia.com/articles/bonds/08/reverse-convertible-note.asp). Those will have debt instrument and usually put option. 


Option dealers will be short index options and thus short correlation and long single name options.
That can be used to create a dispersion strategy to capture this premium. 
There are at least 2 ways how to capture dispersion premium:

1.	Strategy involving trading variance swaps. 
  -	Sell index variance swaps
  -	Buy stock variance swaps
2.	Strategy involving trading index options and single name options
  - Buy single names options 
  -	Sell index options

Lets take a look on different major indexes to see if correlation premium (essentially a spread between implied and subsequently realized correlation) is meaningful.





{% highlight r %}
#*****************************************************************
# Indexes of interest
#*****************************************************************
symbols    = spl('spx,sx5e,cac,ukx,smi,nky')
ave.spread = lst()

for(sym in symbols) {
  # read data
  data    = read_excel('data.xlsx', sheet = sym)
  data    = as.data.frame(data)
  data    = make.xts(data[,-1], as.Date(data[,1]))
  names(data) = paste0(toupper(sym), c(' Implied',' Realized',' Implied - Realized'))
  
  #*****************************************************************
  # Plot Correlations full history
  #*****************************************************************
  layout(matrix(1:2,2,1))
  qis.plot(data[,1], type ='l', ylim = range(data), main = paste0(toupper(sym)," 1YR Implied and Realized Correlation"),col = col.add.alpha('red', 150))
  qis.plot.lines(data[,2], type='l',col = col.add.alpha('blue', 150))
  qis.plot.lines(data[,3], type='l',lwd = '1',col = col.add.alpha('black', 150))
  qis.plot.legend(names(data),'red,blue,black',as.list(data))
  
  #*****************************************************************
  #  Plot Correlation for the last 3 years
  #*****************************************************************
  data = data['2015::']
  qis.plot(data[,1], type ='l', ylim = range(data), main = paste0(toupper(sym)," 1YR Implied and Realized Correlation"),col = col.add.alpha('red', 150))
  qis.plot.lines(data[,2], type='l',col = col.add.alpha('blue', 150))
  qis.plot.lines(data[,3], type='l',lwd = '1',col = col.add.alpha('black', 150))
  qis.plot.legend(names(data),'red,blue,black',as.list(data))
  
  #*****************************************************************
  # Average Spread
  #*****************************************************************
  tmp = last(qis.apply.matrix(data,mean))
  print(to.nice(tmp))
  
  ave.spread[[sym]] = c(sym,last(qis.apply.matrix(data,mean))[1,3])
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-03-24-dispersion-premium/plot-2-1.png)

|           |SPX Implied        |SPX Realized       |SPX Implied - Realized |
|:----------|:------------------|:------------------|:----------------------|
|2018-03-22 |50.719999999999999 |34.630000000000003 |16.090000000000000     |
    


![plot of chunk plot-2](/public/images/2018-03-24-dispersion-premium/plot-2-2.png)

|           |SX5E Implied       |SX5E Realized      |SX5E Implied - Realized |
|:----------|:------------------|:------------------|:-----------------------|
|2018-03-22 |60.299999999999997 |55.299999999999997 |5.000000000000000       |
    


![plot of chunk plot-2](/public/images/2018-03-24-dispersion-premium/plot-2-3.png)

|           |CAC Implied         |CAC Realized        |CAC Implied - Realized |
|:----------|:-------------------|:-------------------|:----------------------|
|2018-03-22 |55.0700000000000003 |52.2299999999999969 |2.8399999999999999     |
    


![plot of chunk plot-2](/public/images/2018-03-24-dispersion-premium/plot-2-4.png)

|           |UKX Implied        |UKX Realized       |UKX Implied - Realized |
|:----------|:------------------|:------------------|:----------------------|
|2018-03-22 |47.390000000000001 |36.619999999999997 |10.779999999999999     |
    


![plot of chunk plot-2](/public/images/2018-03-24-dispersion-premium/plot-2-5.png)

|           |SMI Implied         |SMI Realized        |SMI Implied - Realized |
|:----------|:-------------------|:-------------------|:----------------------|
|2018-03-22 |60.1599999999999966 |51.4299999999999997 |8.7300000000000004     |
    


![plot of chunk plot-2](/public/images/2018-03-24-dispersion-premium/plot-2-6.png)

|           |NKY Implied         |NKY Realized        |NKY Implied - Realized |
|:----------|:-------------------|:-------------------|:----------------------|
|2018-03-23 |60.3400000000000034 |52.5300000000000011 |7.8099999999999996     |
    

All major indexes has most recently relatively stable positive spread with SPX having the largest and CAC having the smallest

*(this report was produced on: 2018-03-26)*
