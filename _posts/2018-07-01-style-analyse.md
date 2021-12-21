---
layout: post
title:  Fund style attribution
comments: false
---




Style analysis is a procedure that tries to attribute funds performance to the performance of asset classes by running the constrained linear regression. I will use an example of 
[Fidelity Worldwide Fund](https://fundresearch.fidelity.com/mutual-funds/composition/315910505). [Mutual Fund Analysis](http://www.styleadvisor.com/resources/concepts/mutual_fund_analysis.html) contains a bit more info about style analysis. 




{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
prices.file        = 'style.Rdata'
dates.range        = '1997::'


symbols      = spl('FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY,EWL,EWN,EWP')      
symbol.names = spl('Fund,Australia,Canada,France,Germany,Japan,UK,USA,Switzerland,Netherlands,Spain') 

#*****************************************************************
# Get historical prices
#*****************************************************************
data   = qis.yahoo.data(tickers = symbols, file = prices.file, dates = dates.range, obj.type = 'env', debug = F)
prices = data$prices
prices = qis.apply.matrix(prices, function(x) ifna.prev(x))

# print starting dates
print(qis.env.start.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|FWWFX |1997-01-02 |
|EWU   |1997-01-02 |
|EWA   |1997-01-02 |
|EWC   |1997-01-02 |
|EWG   |1997-01-02 |
|EWJ   |1997-01-02 |
|EWL   |1997-01-02 |
|EWN   |1997-01-02 |
|EWP   |1997-01-02 |
|SPY   |1997-01-02 |
|EWQ   |1997-01-02 |
    




{% highlight r %}
# print ending dates
print(qis.env.end.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|FWWFX |2018-08-30 |
|EWU   |2018-08-30 |
|EWA   |2018-08-30 |
|EWC   |2018-08-30 |
|EWG   |2018-08-30 |
|EWJ   |2018-08-30 |
|EWL   |2018-08-30 |
|EWN   |2018-08-30 |
|EWP   |2018-08-30 |
|SPY   |2018-08-30 |
|EWQ   |2018-08-30 |
    




{% highlight r %}
# find month ends
month.ends       = endpoints(prices, 'months')
prices           = prices[month.ends,]
index(prices)    = as.Date(paste('1/', format(index(prices), '%m/%Y'), sep=''), '%d/%m/%Y')
colnames(prices) = symbol.names

# check missing prices
print(sapply(prices, count))
{% endhighlight %}



| Fund.Fund| Australia.Australia| Canada.Canada| France.France| Germany.Germany| Japan.Japan| UK.UK| USA.USA| Switzerland.Switzerland| Netherlands.Netherlands| Spain.Spain|
|---------:|-------------------:|-------------:|-------------:|---------------:|-----------:|-----:|-------:|-----------------------:|-----------------------:|-----------:|
|       260|                 260|           260|           260|             260|         260|   260|     260|                     260|                     260|         260|
    




{% highlight r %}
# compute returns
returns = prices / mlag(prices) - 1


# load 3-Month Treasury Bill from FRED
cash = getSymbols('TB3MS', src='FRED', auto.assign = FALSE) 
cash = qis.process.tbill(cash)
index(cash) = as.Date(paste('1/', format(index(cash), '%m/%Y'), sep=''), '%d/%m/%Y')
cash = ROC(Ad(cash), type = 'discrete'); colnames(cash) = 'Cash'

# merge data
returns = na.omit( merge(returns, cash))

#*****************************************************************
# Constrained OLS regression over 36 Month window
#*****************************************************************
ndates     = nrow(returns)
n          = ncol(returns)-1
window.len = 36

style.weights     = returns[, -1]
style.weights[]   = NA
  
style.r.squared   = returns[, 1]
style.r.squared[] = NA


# Setup constraints
temp = rep(0, n); names(temp) = colnames(returns)[-1]
lb      = temp
ub      = temp
ub[]    = 1


lb['Australia'] = 0
ub['Australia'] = 0.05

lb['Canada'] = 0
ub['Canada'] = 0.05

lb['France'] = 0
ub['France'] = 0.15

lb['Germany'] = 0
ub['Germany'] = 0.15

lb['Japan'] = 0
ub['Japan'] = 0.25

lb['UK'] = 0
ub['UK'] = 0.25

lb['USA'] = 0.3
ub['USA'] = 0.8

lb['Switzerland'] = 0
ub['Switzerland'] = 0.05

lb['Netherlands'] = 0
ub['Netherlands'] = 0.05

lb['Spain'] = 0
ub['Spain'] = 0.05

lb['Cash'] = 0.02
ub['Cash'] = 0.15

# 0 <= x.i <= 1
c = qis.new.constraints(n, lb = lb, ub = ub)

# SUM x.i = 1
c = qis.add.constraints(rep(1, n), 1, type = '=', c)   


for(i in window.len:ndates) {
  window.index = (i - window.len + 1) : i
  
  x =  returns[window.index, -1] # remove fund
  y =  returns[window.index, 1]  # use fund
  
  fit                 = qis.lm.constraint(x, y, c)
  style.weights[i,]   = fit$coefficients
  style.r.squared[i,] = fit$r.squared
  
  # must be
  # sum(fit$coefficients)
  
  # min(fit$coefficients)
  # max(fit$coefficients)
}


# Latest weights
weight = last(style.weights)

# Print last weight
print(to.nice(weight))
{% endhighlight %}



|           |Australia            |Canada               |France               |Germany              |Japan                |UK                   |USA                  |Switzerland          |Netherlands          |Spain                |Cash                 |
|:----------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|:--------------------|
|2018-07-01 |0.050000000000000003 |0.000000000000000000 |0.110000000000000001 |0.000000000000000000 |0.250000000000000000 |0.059999999999999998 |0.409999999999999976 |0.050000000000000003 |0.050000000000000003 |0.000000000000000000 |0.020000000000000000 |
    




{% highlight r %}
# Print r^2
qis.plot.xts(100*style.r.squared, main = 'OLS R^2')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-07-01-style-analyse/plot-2-1.png)

{% highlight r %}
# Weight history
qis.plot.strategy.weights(style.weights, name='constrained OLS')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-07-01-style-analyse/plot-2-2.png)

{% highlight r %}
#*****************************************************************
# Look at tracking error
#*****************************************************************
manager.returns = returns[, 1]
manager.returns = manager.returns[window.len:ndates,]
implied.returns = as.xts( rowSums(style.weights * returns[, -1]), index(returns))
implied.returns = implied.returns[window.len:ndates,]

tracking.error = manager.returns - implied.returns
alpha         = 12 * mean(tracking.error)

# mean error ( which is alpha)
print(alpha)
{% endhighlight %}



0.0166189222121216
    




{% highlight r %}
# std of the alpha
covar.alpha   = 12 * cov(tracking.error)
print(covar.alpha)
{% endhighlight %}



|     |                Fund|
|:----|-------------------:|
|Fund | 0.00135886269121496|
    




{% highlight r %}
# plot
layout(1)
qis.plot(cumprod(1+manager.returns), type='l')
qis.plot.lines(cumprod(1+implied.returns), col='red')
qis.plot.legend('Fund,Style', 'black,red')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-07-01-style-analyse/plot-2-3.png)

{% highlight r %}
qis.plot(100*tracking.error, type='l', main="Monthly Tracking error")
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-07-01-style-analyse/plot-2-4.png)


*(this report was produced on: 2018-08-31)*
