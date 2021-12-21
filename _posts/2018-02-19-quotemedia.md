---
layout: post
title: Historical prices from Quotemedia
comments: true
---




Here I will show how to extract data from  [Quotemedia](http://quotemedia.com/).



{% highlight r %}
ticker = 'AMZN'

#*****************************************************************
# Load data
#*****************************************************************
out  = qis.quotemedia.data(ticker, '1990-01-01', '2018-02-17')

#*****************************************************************
# Plot data
#*****************************************************************
qis.plot.xts(out[,'adjclose'])
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-19-quotemedia/plot-2-1.png)


*(this report was produced on: 2018-02-19)*
