---
layout: post
title: Volatility using parallel computation with Rcpp
comments: true
---




Today, I would like to show how to use [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) for fast computation of equal weighted volatility
and exponential weighted volatility. [RiskMetrics](http://pascal.iseg.utl.pt/~aafonso/eif/rm/TD4ePt_2.pdf) has a good reference of how to compute those. 
I will explore several methods of computing correlation: default R technique, Rccp, Rccp with parallel mode.


{% highlight r %}
#*****************************************************************
# Get RUB/USD exchange rate
#*****************************************************************
prices  = qis.fx.sauder.data(2007, 2018, 'USD', 'RUB') 

#*****************************************************************
# Input parameters
#*****************************************************************
nwindow = 60
lambda  = 0.94
return = diff(log(prices))
return = ifna(return,0)

#*****************************************************************
# Calculate volatility using different methods
#*****************************************************************
hvol.r = sqrt(252)*qis.apply.matrix(return, runSD, n = nwindow)
hvol.c = sqrt(252)*qis.apply.matrix(return, qis.cpp.run.psd, n = nwindow)
print(qis.test.equality(hvol.r, hvol.c))
{% endhighlight %}



|Item1  |Item2  |Equal |
|:------|:------|:-----|
|hvol.r |hvol.c |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
res <- benchmark(qis.apply.matrix(return, runSD, n = nwindow),
                 qis.apply.matrix(return, qis.cpp.run.psd, n = nwindow),
                 replications = 10,
                 order="relative")
print(qis.nice(res[,1:4]))
{% endhighlight %}



|   |test                                                   |replications |elapsed |relative |
|:--|:------------------------------------------------------|:------------|:-------|:--------|
|2  |qis.apply.matrix(return, qis.cpp.run.psd, n = nwindow) |10.00        |0.00    |1.00     |
|1  |qis.apply.matrix(return, runSD, n = nwindow)           |10.00        |0.02    |5.30     |
    




{% highlight r %}
#*****************************************************************
# Calculate EWMA volatility using different methods
#*****************************************************************
hvol.w = sqrt(252) * qis.apply.matrix(return, qis.cpp.run.ewma,  lambda = lambda)
hvol.p = sqrt(252) * qis.apply.matrix(return, qis.cpp.run.pewma, lambda = lambda)
print(qis.test.equality(hvol.w, hvol.p))
{% endhighlight %}



|Item1  |Item2  |Equal |
|:------|:------|:-----|
|hvol.w |hvol.p |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
res <- benchmark(qis.apply.matrix(return, qis.cpp.run.ewma, lambda = lambda),
                 qis.apply.matrix(return, qis.cpp.run.pewma,  lambda = lambda),
                 replications = 10,
                 order="relative")
print(qis.nice(res[,1:4]))
{% endhighlight %}



|test                                                         |replications |elapsed |relative |
|:------------------------------------------------------------|:------------|:-------|:--------|
|qis.apply.matrix(return, qis.cpp.run.ewma, lambda = lambda)  |10.00        |0.65    |1.00     |
|qis.apply.matrix(return, qis.cpp.run.pewma, lambda = lambda) |10.00        |0.66    |1.00     |
    




{% highlight r %}
#*****************************************************************
# Plot volatilities
#*****************************************************************
dates = '2007::2018'
layout(1:2)
qis.plot(return[dates,1],type='h', plotX=F,col = col.add.alpha('black', 150))
qis.plot.legend(paste('Daily returns for',names(return)[1]), 'black')
qis.plot(hvol.p[dates,1],type='l',col = col.add.alpha('black', 150))
qis.plot.lines(hvol.r[dates,1], col=col.add.alpha('blue', 150))
qis.plot.legend('Exponentially Weighted Vol,Equal Weighted Vol','black,blue')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-04-16-fast-volatility/plot-2-1.png)


*(this report was produced on: 2018-05-09)*
