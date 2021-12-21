---
layout: post
title: Fundamental data from ADVFN
comments: true
---




Here I will show how to get historical fundamental data from [ADVFN](https://www.advfn.com/) 
and calculate historical EPS and PE ratios for Amazon and Google.





{% highlight r %}
#******************************************************************
# Load historical fundamental and pricing data
#****************************************************************** 
load.data        = F
tickers          = spl('AAPL,GOOG')
tickers.exchange = spl('NASDAQ:AAPL,NASDAQ:GOOG')

#*****************************************************************
# Get fundamental data
#*****************************************************************	
if(load.data) {
  dataf <- new.env()
  for(i in 1:len(tickers)) {
    if(is.null(dataf[[tickers[i]]])) {
      cat(tickers[i],'\n')
      dataf[[tickers[i]]] = qis.advfn.fund.data(tickers.exchange[i], 60)
    }
  }
  save(dataf, file='data.advfn.Rdata')
} else {
  load(file='data.advfn.Rdata')
}

#*****************************************************************
# How many quarters we got so far per ticker
#*****************************************************************
#sapply(dataf, function(x) ncol(x))

#*****************************************************************
# Get pricing data
#*****************************************************************
data    = new.env()
symbols = getSymbols(tickers, src = 'yahoo', from = '2010-01-01', env = data, auto.assign = T)
for(i in symbols) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
{% endhighlight %}

{% highlight r %}
#*****************************************************************
# Calculate EPS
#*****************************************************************				
for(i in tickers) {
  f     = dataf[[i]]
  dates = qis.advfn.get.dates(f)
  
  # Earnings per Share		
  EPS = 4 * qis.advfn.get.label('Diluted EPS from Total Operations', f, dates)
  if(nrow(EPS) > 3)
    EPS = rbind(EPS[1:3], qis.advfn.get.label('Diluted EPS from Total Operations', f, dates, is.12m.rolling=T)[-c(1:3)])
  
  data[[i]] = merge(data[[i]], EPS)
}
qis.prep(data, align='keep.all', dates='2010::')
prices = qis.apply.matrix(data$prices, function(x) qds.ifna.prev.next(x))
EPS    =  qis.apply(data, function(x) ifna.prev.next(x[, 'EPS']))

#*****************************************************************
# Calculate PE
#*****************************************************************
PE  = ifna(prices / EPS, NA)
PE[ abs(EPS) < 0.001 ] = NA

#*****************************************************************
# Plot prices
#*****************************************************************
qis.plot.xts(prices)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2018-02-09-data-advfn/plot-3-1.png)

{% highlight r %}
#*****************************************************************
# Plot EPS
#*****************************************************************
qis.plot.xts(EPS)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2018-02-09-data-advfn/plot-3-2.png)

{% highlight r %}
#*****************************************************************
# PE prices
#*****************************************************************
qis.plot.xts(PE)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2018-02-09-data-advfn/plot-3-3.png)


*(this report was produced on: 2018-02-10)*
