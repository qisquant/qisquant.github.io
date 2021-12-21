---
layout: post
title: Lagged correlations using parallel computation with Rcpp
comments: true
---




Today, I will focus on [Detecting Leaders from Correlated Time Series](https://www.researchgate.net/publication/220787832_Detecting_Leaders_from_Correlated_Time_Series) paper for fast computation of leadership score on lead-lag relationship.
I will explore several methods of computing leadership: default R technique and [Rccp](https://cran.r-project.org/web/packages/Rcpp/index.html). 
I have already published few posts related to [Rccp](https://cran.r-project.org/web/packages/Rcpp/index.html):

 - [Quantile using parallel computation with Rcpp](https://sysresearcher.github.io/fast-quantile) 
 - [Volatility using parallel computation with Rcpp](https://sysresearcher.github.io/fast-volatility)
 - [Correlation using parallel computation with Rcpp](https://sysresearcher.github.io/fast-correlation)


{% highlight r %}
#*****************************************************************
# Load data from Yahoo
#*****************************************************************
prices.orig  = qis.yahoo.data(file = 'sp500.Rdata', min.history = 3*252)

#*****************************************************************
# Small test
#*****************************************************************
nassets  = 10
nwindow  = 15
prices   = prices.orig[,1:nassets]
nperiod  = nrow(prices)
return   = prices / mlag(prices) - 1

index  =  (nperiod - 20) : nperiod
return  = return[index,]
nperiod = nrow(return)

#*****************************************************************
# Calculate Lead Lag Correlation using different methods
#*****************************************************************
r.cor  = qis.run.leadership(return, nwindow)
c.cor  = qis.cpp.run.leadership(return,nwindow,run.parallel = F)
cp.cor = qis.cpp.run.leadership(return,nwindow,run.parallel = T)
print(qis.test.equality(r.cor, c.cor, cp.cor, type='all'))
{% endhighlight %}



|Item1 |Item2  |Equal |
|:-----|:------|:-----|
|r.cor |c.cor  |TRUE  |
|r.cor |cp.cor |TRUE  |
|c.cor |cp.cor |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
res <- benchmark(qis.run.leadership(return, nwindow),
                 qis.cpp.run.leadership(return,nwindow,run.parallel = F),
                 qis.cpp.run.leadership(return,nwindow,run.parallel = T),
                 replications = 2,
                 order="relative")

print(qis.nice(res[,1:4]))
{% endhighlight %}



|test                                                      |replications |elapsed |relative |
|:---------------------------------------------------------|:------------|:-------|:--------|
|qis.run.leadership(return, nwindow)                       |2.00         |0.06    |         |
|qis.cpp.run.leadership(return, nwindow, run.parallel = F) |2.00         |0.00    |         |
|qis.cpp.run.leadership(return, nwindow, run.parallel = T) |2.00         |0.00    |         |
    




{% highlight r %}
#*****************************************************************
# Large test. Use only Rccp versions as native R version is very slow
#*****************************************************************
nassets  = 100
nwindow  = 20
prices   = prices.orig[,1:nassets]
nperiod  = nrow(prices)
return   = prices / mlag(prices) - 1

index  =  (nperiod - 3000) : nperiod
return  = return[index,]
nperiod = nrow(return)

#*****************************************************************
# Calculate Lead Lag Correlation using different methods
#*****************************************************************
c.cor  = qis.cpp.run.leadership(return,nwindow,run.parallel = F)
cp.cor = qis.cpp.run.leadership(return,nwindow,run.parallel = T)
print(qis.test.equality(c.cor, cp.cor, type='all'))
{% endhighlight %}



|Item1 |Item2  |Equal |
|:-----|:------|:-----|
|c.cor |cp.cor |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
res <- benchmark(qis.cpp.run.leadership(return,nwindow,run.parallel = F),
                 qis.cpp.run.leadership(return,nwindow,run.parallel = T),
                 replications = 1,
                 order="relative")

print(qis.nice(res[,1:4]))
{% endhighlight %}



|   |test                                                      |replications |elapsed |relative |
|:--|:---------------------------------------------------------|:------------|:-------|:--------|
|2  |qis.cpp.run.leadership(return, nwindow, run.parallel = T) |1.00         |0.55    |1.00     |
|1  |qis.cpp.run.leadership(return, nwindow, run.parallel = F) |1.00         |1.66    |3.00     |
    

The parallel version is about 3 times faster

*(this report was produced on: 2018-05-11)*
