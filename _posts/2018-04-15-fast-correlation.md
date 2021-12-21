---
layout: post
title: Correlation using parallel computation with Rcpp
comments: true
---




Today, I would like to show how to use [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) for fast computation of pair-wise correlation of S&P500
constituents. I will explore several methods of computing correlation: default R technique, Rccp, Rccp with parallel mode and Rccp with initially
pre-computing statistics. For simplicity, I will assume static composition of S&P500 back to 2000.


{% highlight r %}
#*****************************************************************
# Load data from Yahoo
#*****************************************************************
#prices = qis.yahoo.data(file = 'nasdaq.Rdata', align = 'remove.na',  min.history = 3*252)
#prices = qis.yahoo.data(file = 'nasdaq.Rdata',  min.history = 3*252, force.load  = T)
#prices = qis.yahoo.data(file = 'nasdaq.Rdata',  min.history = 3*252)
prices = qis.yahoo.data(file = 'sp500.Rdata', min.history = 3*252)

#*****************************************************************
# Calculate daily returns
#*****************************************************************
ndays   = 10*252
return  = prices / mlag(prices) - 1
return  = last(return ,ndays)
return  = coredata(na.omit(return ))

#*****************************************************************
# Compute Correlation using defualt method
#*****************************************************************
cor.default = function(x) {
  temp = cor(x)
  temp[!lower.tri(temp)] = 0
  temp
}
#*****************************************************************
# Calculate Correlation using different methods
#*****************************************************************
r.cor =  cor.default(return)
c.cor  = qis.cpp.cor(return)
cp.cor = qis.cpp.pcor(return)
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
res <- benchmark(cor(return),
                 qis.cpp.cor(return),
                 qis.cpp.pcor(return),
                 replications = 20,
                 order="relative")
print(qis.nice(res[,1:4]))
{% endhighlight %}



|   |test                 |replications |elapsed |relative |
|:--|:--------------------|:------------|:-------|:--------|
|3  |qis.cpp.pcor(return) |20.00        |0.26    |1.00     |
|2  |qis.cpp.cor(return)  |20.00        |1.75    |6.80     |
|1  |cor(return)          |20.00        |1.88    |7.30     |
    

Now, let's try to calculate running correlation with again different methods and benchmark them.


{% highlight r %}
#*****************************************************************
# Running correlation
#*****************************************************************
nassets = 100
#nassets = ncol(prices)
nwindow = 252
ndays   = 5*252
nperiod = nrow(prices)
return  = prices / mlag(prices) - 1
return  = return[,1:nassets]

# remove assets with not enough history
remove = which( count(return, side=2) < ndays )
return = return[, -remove]

nperiod = nrow(return)
index   = (nperiod-ndays):nperiod
return = return[index,]
return = ifna(return,0)

#*****************************************************************
# Calculate Runnig correlation using different methods
#*****************************************************************
r.cor       = qis.run.cor(return, nwindow)
c.cor       = qis.cpp.run.cor(return, nwindow)
cp.cor      = qis.cpp.run.pcor(return, nwindow)
c.cor.fast  = qis.cpp.run.cor.fast(return, nwindow)
cp.cor.fast = qis.cpp.run.pcor.fast(return, nwindow)

print(qis.test.equality(r.cor, c.cor, cp.cor, c.cor.fast, cp.cor.fast))
{% endhighlight %}



|Item1 |Item2       |Equal |
|:-----|:-----------|:-----|
|r.cor |c.cor       |TRUE  |
|r.cor |cp.cor      |TRUE  |
|r.cor |c.cor.fast  |TRUE  |
|r.cor |cp.cor.fast |TRUE  |
    




{% highlight r %}
# Free memory
env.rm(spl('r.cor,c.cor,cp.cor,c.cor.smart,cp.cor.smart'), globalenv())
{% endhighlight %}


{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
res <- benchmark(qis.run.cor(return, nwindow),
                 qis.cpp.run.cor(return, nwindow),
                 qis.cpp.run.pcor(return, nwindow),
                 qis.cpp.run.cor.fast(return, nwindow),
                 qis.cpp.run.pcor.fast(return, nwindow),
                 replications = 1,
                 order="relative")
print(qis.nice(res[,1:4]))
{% endhighlight %}



|   |test                                   |replications |elapsed |relative |
|:--|:--------------------------------------|:------------|:-------|:--------|
|4  |qis.cpp.run.cor.fast(return, nwindow)  |1.00         |0.03    |1.00     |
|5  |qis.cpp.run.pcor.fast(return, nwindow) |1.00         |0.05    |1.60     |
|3  |qis.cpp.run.pcor(return, nwindow)      |1.00         |0.21    |7.10     |
|2  |qis.cpp.run.cor(return, nwindow)       |1.00         |1.13    |37.70    |
|1  |qis.run.cor(return, nwindow)           |1.00         |1.66    |55.50    |
    



*(this report was produced on: 2018-05-09)*
