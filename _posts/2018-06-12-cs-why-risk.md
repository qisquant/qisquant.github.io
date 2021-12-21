---
layout: post
title:  Estimating asset covariance matrix using risk model
comments: false
---




The sample covariance matrix can be estimated using historical asset's returns,
but the use of the sample covariance matrix becomes problematic when the number of assets
similar to the time dimension of the problem. The alternative solution is to build asset level covariance
matrix using risk model.
I will re-use results from  [Credit Suisse Risk model](https://sysresearcher.github.io/cs-risk-model) to 
construct sample covariance matrix and risk model based covariance matrix for minimum variance portfolio optimization.
Here is couple links to papers:

1. [Honey, I Shrunk the Sample Covariance Matrix](http://www.ledoit.net/honey.pdf).
2. [Factor-based Expected Returns, Risks and Correlations](https://web.stanford.edu/~wfsharpe/mia/fac/mia_fac3.htm).
3. [Estimating High Dimensional Covariance Matrices and its Applications](http://aeconf.com/Articles/Nov2011/aef120201.pdf).
4. [Improved Estimation of the Covariance Matrix of Stock Returns With an Application to Portfolio Selection](http://www.ledoit.net/ole2.pdf).


{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
factor.file     = 'dj30.factors.Rdata'
riskmodel.file  = 'dj30.risk.model.Rdata'


#*****************************************************************
# Load factors
#*****************************************************************
load(file = factor.file)

#*****************************************************************
# Load risk model
#*****************************************************************
load(file = riskmodel.file)


# load factor exposures
factor.exposures = all.data[,,-1]
factor.names     = dimnames(factor.exposures)[[3]]


# dimentions
nperiods  = nrow(next.month.ret)
nstocks   = ncol(next.month.ret)
nfactors  = len(factor.names)


# dimnames(all.data)
# dimnames(factor.exposures)

#*****************************************************************
# Compute Betas from 1f model : ra = alpha + beta*rb + e
# b = cov(ra,rb) / var(rb)
# The betas are measured on a two-year rolling window
# http://en.wikipedia.org/wiki/Beta_(finance)
#******************************************************************

# this month returns
ret  = mlag(next.month.ret)
ret  = ifna(ret,0)
beta = ret * NA

# 1/n benchmark portfolio
benchmark     = qis.ntop(ret, nstocks)
benchmark.ret = rowSums(benchmark * ret, na.rm=T)


# estimate betas to benchmark with is equally weighted DJ stocks
for(t in 24:nperiods) {
  t.index = (t-23):t
  benchmark.var = var( benchmark.ret[t.index], na.rm=T )
  
  t.count = count(ret[t.index, ])
  t.cov   = cov( ifna(ret[t.index,], 0), benchmark.ret[t.index], use='complete.obs' )
  
  # require at least 20 months of history
  beta[t,] = iif(t.count > 20, t.cov / benchmark.var, NA)
}


# let's take the most recent 36 month
t       = nperiods-35
t.index = t:nperiods


# F : factor exposures as of t
factor.exposures = ifna(all.data[t,,-1] ,0)

# factor covariance COVf as of t
factor.covariance = factor.covariance[t,,]

# spesific variance  as of t
specific.variance = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T) )^2


#*****************************************************************
#  Compute sample covariance
#*****************************************************************
COV.SAMPLE = cov_shrink(ret[t.index,])

#*****************************************************************
#  Compute risk model based covariance
#*****************************************************************
stock.variance       = diag(0, ncol = nstocks, nrow = nstocks)
diag(stock.variance) <- specific.variance
COV.RISK             =  factor.exposures %*% factor.covariance  %*%  t(factor.exposures) + stock.variance

# dim(factor.exposures)
# dim(factor.covariance)


#*****************************************************************
#  Solve Mean-variance portfolio optimization problem
#*****************************************************************

#*****************************************************************
# Mean-variance portfolio optimization problem
#  Min (w'*Cov*w)
#  S.t. 
#   SUM(w) = 1
#   SUM(w*beta) = 1
#   0 <= w <= 0.1
#*****************************************************************

weight.sample  = NA * next.month.ret
weight.model   = weight.sample


# set optimization problem
s.lb    = rep(0.0,nstocks)
s.ub    = rep(0.1,nstocks)
wtot.dn = 1.0
wtot.up = 1.0
beta.up = 1.0
beta.dn = 1.0


# minimum variance objective function function
# w'*COV *w
fn        = qds.var.obj.fn

# creat input parameters for optimization
par       = rep(1 / nstocks, nstocks)
par.lower = s.lb
par.upper = s.ub
lin.l     = c(s.lb, wtot.dn, beta.dn)
lin.u     = c(s.ub, wtot.up, beta.up)
A         = rbind(diag(nstocks), rep(1,nstocks))
A         = rbind(A, as.numeric(ifna(as.vector(beta[t,]),0)))


# optimize
weight.sample = donlp2(par    = par,
                    fn        = fn(COV.SAMPLE),
                    par.lower = par.lower,
                    par.upper = par.upper,
                    A         = A,
                    lin.u     = lin.u,
                    lin.l     = lin.l,
                    control   = donlp2Control(silent = T, iterma = 10000, nstep = 100, epsx = 1e-10))$par



# volatility of the portfolio
sqrt(qds.portfolio.var(as.numeric(weight.sample), COV.SAMPLE))
{% endhighlight %}

[1] 0.02383428664668898


{% highlight r %}
# SUM(w) = 1
sum(weight.sample)
{% endhighlight %}

[1] 1.0000000000000002


{% highlight r %}
#SUM(w*beta) = 1
sum(beta[t,]*weight.sample)
{% endhighlight %}

[1] 0.99999999999999989


{% highlight r %}
# 0 <= w <= 0.1
min(weight.sample)
{% endhighlight %}

[1] 0


{% highlight r %}
max(weight.sample)
{% endhighlight %}

[1] 0.10000000000000001


{% highlight r %}
#*****************************************************************
# Plot weights using optimization with sammple COV matrix
#*****************************************************************     
x        = as.numeric(round(weight.sample,4))
names(x) = colnames(next.month.ret)
barplot(x,las = 2, main = 'Weights with sample covariance matrix', col = col.add.alpha(20))  
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-12-cs-why-risk/plot-2-1.png)

{% highlight r %}
# optimize
weight.model = donlp2(par     = par,
                    fn        = fn(COV.RISK),
                    par.lower = par.lower,
                    par.upper = par.upper,
                    A         = A,
                    lin.u     = lin.u,
                    lin.l     = lin.l,
                    control   = donlp2Control(silent = T, iterma = 10000, nstep = 100, epsx = 1e-10))$par


# volatility of the portfolio
sqrt(qds.portfolio.var(as.numeric(weight.model), COV.RISK))
{% endhighlight %}

[1] 0.026761682250500109


{% highlight r %}
# SUM(w) = 1
sum(weight.model)
{% endhighlight %}

[1] 0.99999999999999978


{% highlight r %}
#SUM(w*beta) = 1
sum(beta[t,]*weight.model)
{% endhighlight %}

[1] 0.99999999999999978


{% highlight r %}
# 0 <= w <= 0.1
min(weight.model)
{% endhighlight %}

[1] 1.3010426069826053e-18


{% highlight r %}
max(weight.model)
{% endhighlight %}

[1] 0.053582535252020322


{% highlight r %}
#*****************************************************************
# Plot weights using optimization with factor COV matrix
#*****************************************************************      
y        = as.numeric(round(weight.model,4))
names(y) = colnames(next.month.ret)
barplot(y,las = 2, main = 'Weights with factor-based covariance matrix', col = col.add.alpha(10)) 
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-12-cs-why-risk/plot-2-2.png)

 The minimum variance portfolio computed under the risk model is more diversified.

*(this report was produced on: 2018-08-31)*
