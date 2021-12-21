---
layout: post
title:  Credit Suisse Risk model
comments: false
---





I built several composite average factors in my past post outlined  in [Credit Suisse Composite factor](https://qisresearch.github.io/cs-composite).
 
I would like to build multiple factor risk model. Given that, I would like to compare how closely risk model can mimic portfolio volatility. 
Below I have few references to risk models proposed by Barra, Northfield and Axioma.

1. [Commonality In The Determinants Of Expected Stock Returns](http://www.quantitativeinvestment.com/documents/common.pdf).
2. [Barra Risk Model](http://www.alacra.com/alacra/help/barra_handbook_US.pdf).
3. [Northfield Risk Model](http://www.northinfo.com/documents/8.pdf).
4. [Axioma Risk Model](https://www.axioma.com/media/uploads/document/axus3-equityrm-201309.pdf).



{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
factor.file     = 'dj30.factors.Rdata'
riskmodel.file  = 'dj30.risk.model.Rdata'

# use GARCH(1,1) for prediction
forecast.variance = F

#*****************************************************************
# Load factors
#*****************************************************************
load(file = factor.file)

#*****************************************************************
# Run cross sectional regression to estimate factor returns
#****************************************************************** 

# remove Composite Average factor
factors.avg.monthly = factors.avg.monthly[which(names(factors.avg.monthly) != 'AVG')]

# dimentions
nperiods  = nrow(next.month.ret)
nstocks   = ncol(next.month.ret)
nfactors  = len(ls(factors.avg.monthly))
nsectors  = len(levels(sectors)) 


# create sector dummy variables:
sectors.matrix = array(double(), c(nperiods, nstocks, nsectors))
dimnames(sectors.matrix)[[3]] = levels(sectors)     

# dimnames(sectors.matrix)

# F/T depending if name belongs to the sector
for(j in levels(sectors)) {
  sectors.matrix[,,j] = matrix(sectors == j,  nr = nperiods, nc = nstocks, byrow=T)
}


#the data represented as 3D matrix with dimentions  nperiods x n x num of factors
factors.matrix = abind(factors.avg.monthly, along = 3)

# combine sector dummies and all factors
#  nperiods x nstocks x (nfactors + nsectors)
all.data = abind(sectors.matrix, factors.matrix)

# betas, nperiods x (nfactors + nsectors)
beta = all.data[,1,] * NA

# spesific returns
specific.return = next.month.ret * NA

# re-define number of factors now
nfactors    = ncol(beta)


# append next.month.ret to all.data         
all.data  = abind(next.month.ret, all.data, along = 3)
dimnames(all.data)[[3]][1] = 'Next Month Return'

# dimnames(all.data)

# Estimate betas (factors return)
# Ret = alpha + b1 * F1 + b2 * F2 + ... + bn * Fn + e
for(t in 12:(nperiods-1)) {
  temp = all.data[t:t,,]  # nasset x nfactors +1
  
  # first sector will remove from regression
  x = temp[,-c(1:2)] # this month factors
  y = temp[,1]  # next month return
  

  b = lm(y~x)$coefficients
  b[2:nsectors] = b[1] + b[2:nsectors]
  
  # robust fitting
  # summary(rlm(y~x))
  
  # quantile regression
  # summary(rq(y ~ x, tau = 0.5))
  
  # record estimated beta to the next month.
  # summary(lm(y~x))
  beta[(t+1),] = b
  
  # e = y - 1 * F1 + b2 * F2 + ... + bn * Fn - alpha
  # spesific.ret(t+1)  = e(t)
  specific.return[(t+1),] = y - rowSums(temp[,-1] * matrix(b, nstocks, nfactors, byrow = T), na.rm=T) 
}


#*****************************************************************
# Plot historical cumulative factor returns
#*****************************************************************
fac.return   = make.xts(beta, index(next.month.ret))
fac.return[] = apply(coredata(fac.return), 2, function(x) cumprod(1 + ifna(x,0)))
fac.return   = fac.return['2000::',]

# remove sector retursn
fac.return = fac.return[,-c(1:nsectors)]
n          = ncol(fac.return)

qis.plot(fac.return, ylim = range(fac.return), log='y', main = "Cumulative Factor Returns")
for(i in 1:n) qis.plot.lines(fac.return[,i], col = i)
qis.plot.legend(colnames(fac.return), 1:n, as.list(fac.return))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-10-cs-risk-model/plot-2-1.png)

{% highlight r %}
#*****************************************************************
# Estimate factror covariance matrix
#****************************************************************** 

factor.covariance = array(double(), c(nperiods, nfactors, nfactors))
dimnames(factor.covariance)[[2]] = colnames(beta)
dimnames(factor.covariance)[[3]] = colnames(beta)

# estimate factor covariance
#beta = ifna(beta,0)
for(t in 36:nperiods) {
  factor.covariance[t,,] = var.shrink.eqcor(beta[(t-23):t,])
}

# dimnames(factor.covariance)

#*****************************************************************
# Forecast stock spesific variance using  GARCH(1,1)
#****************************************************************** 
if(forecast.variance) {
  specific.variance = next.month.ret * NA
  for(i in 1:nstocks) {
    ret = ifna(specific.return[,i],0)
    #https://cims.nyu.edu/~almgren/timeseries/Vol_Forecast1.pdf
    specific.variance[,i] = qis.forecast.garch.volatility(ret, 24) 
  }
  
  #*****************************************************************
  # Save multiple factor risk model to be used later during portfolio construction
  #****************************************************************** 
  save(all.data, factor.covariance, specific.variance, file = riskmodel.file)
} else (
  load(riskmodel.file)
)
{% endhighlight %}

[1] "all.data"          "factor.covariance" "specific.variance"


{% highlight r %}
# Portfolio Risk         = SQRT(common factor variance + specific variance)
# common factor variance = (portfolio factor exposure) * factor covariance matrix * (portfolio factor exposure)'
#	specific variance      = (specific.variance)^2 * (portfolio weights)^2


#*****************************************************************
# Compute portfolio risk
#****************************************************************** 
portfolio = rep(1 / nstocks, nstocks) # equally weighted 
portfolio = matrix(portfolio, nstocks, nfactors)

portfolio.risk = next.month.ret[,1] * NA
for(t in 36:(nperiods-1)) { 
  portfolio.exposure = colSums(portfolio * all.data[t,,-1], na.rm=T)
  
  portfolio.risk[t] = sqrt(
    portfolio.exposure %*% factor.covariance[t,,] %*% (portfolio.exposure) + 
      sum(specific.variance[t,]^2 * portfolio[,1]^2, na.rm=T)
  )
}

#*****************************************************************
# Compute historical portfolio risk
#******************************************************************  
portfolio = rep(1 / nstocks, nstocks)
portfolio = t(matrix(portfolio, nstocks, nperiods))

portfolio.returns  = next.month.ret[,1] * NA
portfolio.returns[] = rowSums(mlag(next.month.ret) * portfolio, na.rm=T)
hist.portfolio.risk = runSD(portfolio.returns, 24)


#*****************************************************************
# Plot risks
#******************************************************************             
ylim = range( c(coredata(portfolio.risk), coredata(hist.portfolio.risk)),na.rm=T)

qis.plot(portfolio.risk['2000::',], type='l',col="red", ylim = ylim)
qis.plot.lines(hist.portfolio.risk['2000::',], col='blue')
qis.plot.legend('Estimated Vol,Historical Vol', 'red,blue')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-06-10-cs-risk-model/plot-2-2.png)

The estimation of portfolio volatility using risk model and using historical estimation is decently close to each other.

*(this report was produced on: 2018-12-23)*
