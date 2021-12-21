---
layout: post
title: Quantile using parallel computation with Rcpp
comments: false
---




There are several ways to compute rolling quintiles:
  
 - 	Use native R functions such as [quantile](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/quantile.html).
 -	  Use [caTools](https://cran.r-project.org/web/packages/caTools/index.html) package.
 -   Use [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) wrapper to outsource computation from R to C++.


Lets test all 3 ways here and benchmark them.


{% highlight r %}
#*****************************************************************
# Calculate rolling quantiles with different methods
#*****************************************************************
n     = 10
probs = 0.5
x     = runif(1000)
          
r.quantile = qis.run.quantile(x, n, probs)
c.quantile = qis.cpp.run.quantile(x, n, probs)
t.quantile = qis.catools.run.quantile(x,n,probs)

print(qis.test.equality(r.quantile , c.quantile, t.quantile, type='all'))
{% endhighlight %}



|Item1      |Item2      |Equal |
|:----------|:----------|:-----|
|r.quantile |c.quantile |TRUE  |
|r.quantile |t.quantile |TRUE  |
|c.quantile |t.quantile |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
print(qis.nice(summary(microbenchmark(
  qis.run.quantile(x, n, probs),
  qis.catools.run.quantile(x, n, probs),
  qis.cpp.run.quantile(x, n, probs),
  times = 10
)),0))
{% endhighlight %}



|expr                                  |min    |lq     |mean   |median |uq     |max    |neval |
|:-------------------------------------|:------|:------|:------|:------|:------|:------|:-----|
|qis.run.quantile(x, n, probs)         |60,000 |60,000 |60,000 |60,000 |60,000 |60,000 |10    |
|qis.catools.run.quantile(x, n, probs) |90     |100    |100    |100    |100    |100    |10    |
|qis.cpp.run.quantile(x, n, probs)     |40     |40     |50     |50     |50     |70     |10    |
    

 Let's test few examples from [David Varadi](https://cssanalytics.wordpress.com/2015/02/20/conditional-percentile-channels/) post.
   


{% highlight r %}
#*****************************************************************
# Test Examples
#*****************************************************************

# Example 1
x         = c(201:215,117,115,119,118,121)
n         = len(x)
low.prob  = 0.25
high.prob = 0.75

# r version
stats = qis.run.quantile.weight(x, n, low.prob, high.prob)
print(stats[nrow(stats),,drop=F])
{% endhighlight %}



|                low|               high|
|------------------:|------------------:|
| 118.35135135135135| 214.09090909090909|
    




{% highlight r %}
# c++ version
stats = qis.cpp.run.quantile.weight(x, n, low.prob, high.prob)
print(stats[nrow(stats),,drop=F])
{% endhighlight %}



|                low|               high|
|------------------:|------------------:|
| 118.35135135135135| 214.09090909090909|
    




{% highlight r %}
# Example 2
x = c(1:15,117,115,119,118,121)

# r version
stats = qis.run.quantile.weight(x, n, low.prob, high.prob)
print(stats[nrow(stats),,drop=F])
{% endhighlight %}



| low|               high|
|---:|------------------:|
|   3| 119.49056603773585|
    




{% highlight r %}
# c++ version
stats = qis.cpp.run.quantile.weight(x, n, low.prob, high.prob)
print(stats[nrow(stats),,drop=F])
{% endhighlight %}



| low|               high|
|---:|------------------:|
|   3| 119.49056603773585|
    




{% highlight r %}
#*****************************************************************
# Calculate rolling weighted quantiles with different methods
#*****************************************************************
n         = 10
low.prob  = 0.25
high.prob = 0.75
x         = runif(100)

test1 = qis.run.quantile.weight(x, n, low.prob, high.prob)
test2 = qis.cpp.run.quantile.weight(x, n, low.prob, high.prob)

print(qis.test.equality(test1, test2))
{% endhighlight %}



|Item1 |Item2 |Equal |
|:-----|:-----|:-----|
|test1 |test2 |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Benchmark performance
#*****************************************************************
print(qis.nice(summary(microbenchmark(
  qis.run.quantile.weight(x, n, low.prob, high.prob),
  qis.cpp.run.quantile.weight(x, n, low.prob, high.prob),
  times = 10
)),0))
{% endhighlight %}



|expr                                                   |min   |lq    |mean  |median |uq    |max   |neval |
|:------------------------------------------------------|:-----|:-----|:-----|:------|:-----|:-----|:-----|
|qis.run.quantile.weight(x, n, low.prob, high.prob)     |4,581 |4,920 |5,496 |5,064  |5,391 |9,546 |10    |
|qis.cpp.run.quantile.weight(x, n, low.prob, high.prob) |37    |39    |46    |48     |52    |57    |10    |
    


*(this report was produced on: 2018-05-09)*
