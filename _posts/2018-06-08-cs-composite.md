---
layout: post
title:  Credit Suisse Composite factor
comments: false
---




I am going to build **Composite average factor** and **Theme average factor** which I already discussed in my previous posts:

 1. [Credit Suisse Traditional value factor](https://qisresearch.github.io/cs-traditional-value)
 2. [Credit Suisse Relative value factor](https://qisresearch.github.io/cs-relative-value)
 3. [Credit Suisse Historical growth factor](https://qisresearch.github.io/cs-historical-growth)
 4. [Credit Suisse Profitability factor](https://qisresearch.github.io/cs-profitability)
 5. [Credit Suisse Price momentum factor](https://qisresearch.github.io/cs-price-momentum)
 6. [Credit Suisse Price reversal factor](https://qisresearch.github.io/cs-price-reversal)
 7. [Credit Suisse Small size factor](https://qisresearch.github.io/cs-small-size)

Just to remind, I have collected several references to the methodologies here:

 1. [Credit Suisse 130/30 Large Cap Index : Alpha Factors](http://www.proshares.com/media/documents/CS_Alpha_Factors.pdf)
 2. [Quantitative Investing](https://research-doc.credit-suisse.com/docView?language=ENG&format=PDF&document_id=868261101&source_id=em&serialid=wepCP0bZJlqL0WAXPLGDABEtH%2Fcpw1iaTTNAYHkPLWM%3D)




{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
load.fundamentals = F
prices.file        = 'dj30.Rdata'
fundamentals.file  = 'dj30.fundamental.Rdata'
dates.range        = '1995::'

#*****************************************************************
# Get tickers
#*****************************************************************
tickers          = qis.dj30.components()
tickers.exchange = paste(iif(nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')

#*****************************************************************
# Find Sectors for each company given http://www.sectorspdr.com/sectorspdr/
#****************************************************************** 
sector.map = qis.sector.universe('spdr')
# map our universe against sectors universe
sectors        = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers

#*****************************************************************
# Get historical prices
#*****************************************************************
data = qis.yahoo.data(tickers = tickers, file = prices.file, dates = dates.range, obj.type = 'env', debug = F)

#*****************************************************************
# Get fundamental data
#*****************************************************************
if(load.fundamentals) {
  fundamentals <- new.env()
  for(i in 1:len(tickers)) {
    if(is.null(fundamentals[[tickers[i]]])) {
      cat(tickers[i],'\n')
      fundamentals[[tickers[i]]] = qis.advfn.fund.data(tickers.exchange[i], 100)
    }
  }
  save(fundamentals, file = fundamentals.file)
} else {
  load(file = fundamentals.file)
}

#*****************************************************************
# Create Credit Suisse alpha factors
#*****************************************************************
qis.cs.alpha.factors(fundamentals, data, dates.range, F)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-1.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-2.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-3.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-4.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-5.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-6.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-7.png)![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-8.png)

|             |Q.1               |Q.2               |Q.3               |Q.4               |Q.5               |Q.5.minus.Q.1     |
|:------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period       |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |Jan1995 - Aug2018 |
|Cagr         |9.17              |13.31             |13.92             |17.62             |26.23             |14.3              |
|AveRet       |8.66              |12.55             |13.13             |16.6              |24.66             |13.49             |
|Sharpe       |0.51              |0.72              |0.74              |0.91              |1.24              |0.87              |
|Volatility   |20.64             |19.06             |19.23             |18.74             |19.23             |16.07             |
|Ave.Turnover |0                 |0                 |0                 |0                 |0                 |0                 |
|MaxDD        |-74.46            |-47.82            |-54.58            |-36.54            |-48.08            |-37.73            |
|AveDD        |-2.85             |-2.62             |-2.25             |-2.36             |-2.2              |-2.71             |
|VaR          |-2.01             |-1.89             |-1.83             |-1.72             |-1.77             |-1.48             |
|CVaR         |-3.05             |-2.81             |-2.77             |-2.71             |-2.73             |-2.25             |
    


![plot of chunk plot-3](/public/images/2018-06-08-cs-composite/plot-3-9.png)

There is tendency of quantile 5 (Q5) to outperform quantile 1 (Q1) in most cases. 
The relationship between quantiles is not perfect, but the spread between Q5-Q1 is positive.


*(this report was produced on: 2018-12-23)*
