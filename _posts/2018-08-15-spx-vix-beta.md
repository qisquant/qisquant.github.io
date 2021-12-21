---
layout: post
title:  VIX squeezing 
comments: false
---






After reaching a post Feb-2018 low level of about 11 last week VIX has been squeezing higher, on average moving
2.5 pts for every 1% move in the SPX since then, nearly double the typical behaviour of a 1-1.5 pt move per 1%. 
Let's create z-scored betas historically and see where we stand as of August-2018.


{% highlight r %}
#*****************************************************************
# Parameters
#*****************************************************************
beta.window   = 3*21
zscore.window = 252

#*****************************************************************
# Read data
#*****************************************************************
data = read.xts('data.csv')
x    = data[,'vix']
y    = data[,'spx']

#*****************************************************************
# Plot data
#*****************************************************************
qis.plot(x, type = 'l', LeftMargin=3, col='blue')
qis.plot.2y(y, type='l', las=1, col='red', col.axis = 'red')
qis.plot.legend(paste( names(x)[1], '(rhs),', names(y)[1], '(lhs)'), 'blue,red', list(x,y))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-08-15-spx-vix-beta/plot-2-1.png)

{% highlight r %}
#*****************************************************************
# Compute SPX daily returns and VIX daily changes
#*****************************************************************
beta.window = 3*21
z.window = 252

x = coredata(x); x = x/ mlag(x) -1; x[1] = 0
y = coredata(y); y = c(0,diff(y));

dates    = index(data)
# use only negative returns of SPX
#dates    = index(data)[x < 0]
#x        = x[x < 0]
#y        = y[x < 0]
nperiods = len(x)

#*****************************************************************
# Rolling 3 beta of VIX to S&P using only negative returns in SPX
#*****************************************************************
out        = xts(matrix(NA, nr = nperiods, 3), dates)
names(out) =  c('alpha', 'beta', 'R2')


for(i in beta.window:nperiods) {
  window.index = (i - beta.window + 1) : i

  
  xtemp   = x[window.index]; names(xtemp) = c('beta')
  ytemp   = y[window.index]
  fit     = lm(ytemp~xtemp)

  out[i,] = c(fit$coefficients, summary(fit)$r.squared )
}

#*****************************************************************
# Rolling 3M betas
#*****************************************************************

# invert sign and scale by 1% move in SPX
beta          =  -out$beta *0.01
#beta          = -out$beta *0.01

# Compute 3M SMA on betas
beta = SMA(beta, n = beta.window); names(beta) = "Rolling 3M beta to SPX"

#*****************************************************************
# Plot rolling beta
#*****************************************************************
qis.plot.xts(beta)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-08-15-spx-vix-beta/plot-2-2.png)

{% highlight r %}
#*****************************************************************
# Compute Z-scored betas
#*****************************************************************
z.scored.beta = out$beta;  z.scored.beta []= NA; names(z.scored.beta) = "Rolling 12M Zscored beta to SPX"

for(i in zscore.window:nperiods) {
  window.index = (i - zscore.window + 1) : i
  
  tmp = beta[window.index]
  z.scored.beta[i,] = (beta[i] - mean(tmp)) / sd(tmp)
}

#*****************************************************************
# Plot Z-scored betas
#*****************************************************************
qis.plot.xts(z.scored.beta)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-08-15-spx-vix-beta/plot-2-3.png)

Right now we are sitting on about 2 z-score for betas of VIX to SPX meaning we have unusually high convexity. This is partly because skew is steeper nowadays. 
 

*(this report was produced on: 2018-08-18)*
