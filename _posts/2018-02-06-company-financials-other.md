---
layout: post
title: Financial data from Morningstar, Barchart, Bigcharts
comments: true
---




To continue my previous post about getting financial information from Yahoo [Financial information about companies from Yahoo](https://sysresearcher.github.io/company-financials-yahoo)
here I will show how to get information from [Morningstar](http://www.morningstar.com/) and [Barchart](https://www.barchart.com/). Let's get performance from Morningstar for 3 most 
traded ETF's by volume. For reference, this information can be obtained from [ETFdb](http://etfdb.com/compare/volume/). For example, the performace for SPY can ge seen
[here](http://performance.morningstar.com/Performance/cef/trailing-total-returns.action?t=SPY).


{% highlight r %}
out = qis.morningstar.info('SPY')
print(to.nice(out))
{% endhighlight %}



|                        |1-Day |1-Week |1-Month |3-Month |YTD  |1-Year |3-Year |5-Year |10-Year |15-Year |
|:-----------------------|:-----|:------|:-------|:-------|:----|:------|:------|:------|:-------|:-------|
|SPY (Price)             |1.5   |-5.1   |-4.8    |1.8     |-2.0 |15.6   |10.8   |13.8   |9.2     |10.1    |
|SPY (  V)               |1.5   |-5.1   |-4.7    |1.9     |-1.8 |15.7   |10.8   |13.8   |9.2     |10.1    |
|Large Blend (  V)       |1.4   |-5.0   |-4.7    |2.0     |-2.1 |14.3   |9.1    |12.3   |8.4     |9.5     |
|Rank in Category(Price) |      |       |        |        |     |       |       |       |        |        |
|Rank in Category(  V)   |29.0  |62.0   |43.0    |50.0    |35.0 |33.0   |16.0   |17.0   |25.0    |28.0    |
    




{% highlight r %}
out = qis.morningstar.info('XLF')
print(to.nice(out))
{% endhighlight %}



|                        |1-Day |1-Week |1-Month |3-Month |YTD   |1-Year |3-Year |5-Year |10-Year |15-Year |
|:-----------------------|:-----|:------|:-------|:-------|:-----|:------|:------|:------|:-------|:-------|
|XLF (Price)             |1.91  |-5.72  |-3.18   |6.30    |-0.82 |18.65  |14.49  |16.18  |4.34    |5.56    |
|XLF (  V)               |1.92  |-5.77  |-3.17   |6.33    |-0.86 |18.64  |14.46  |16.16  |4.37    |5.57    |
|Financial (  V)         |1.41  |-4.60  |-2.96   |4.82    |-0.98 |13.32  |12.04  |13.41  |5.91    |6.90    |
|Rank in Category(Price) |      |       |        |        |      |       |       |       |        |        |
|Rank in Category(  V)   |20.00 |90.00  |57.00   |35.00   |44.00 |17.00  |33.00  |18.00  |75.00   |78.00   |
    




{% highlight r %}
out = qis.morningstar.info('EEM')
print(to.nice(out))
{% endhighlight %}



|                                |1-Day |1-Week |1-Month |3-Month |YTD   |1-Year |3-Year |5-Year |10-Year |15-Year |
|:-------------------------------|:-----|:------|:-------|:-------|:-----|:------|:------|:------|:-------|:-------|
|EEM (Price)                     |1.60  |-5.36  |-5.36   |1.37    |-1.49 |25.03  |7.49   |3.30   |2.54    |        |
|EEM (  V)                       |-1.71 |-7.12  |-5.15   |0.99    |-1.34 |25.40  |7.35   |3.35   |2.51    |        |
|Diversified Emerging Mkts (  V) |-0.23 |-5.63  |-4.41   |1.97    |-1.09 |23.54  |7.19   |3.55   |2.79    |11.95   |
|Rank in Category(Price)         |      |       |        |        |      |       |       |       |        |        |
|Rank in Category(  V)           |98.00 |97.00  |73.00   |71.00   |59.00 |39.00  |54.00  |61.00  |56.00   |        |
    

The [Barchart momentum]( https://www.barchart.com/stocks/momentum) has daily stock activity per exchange. Let's get that statistics.


{% highlight r %}
out = qis.barchart.momentum()
print(to.nice(out))
{% endhighlight %}



|       |high52w |low52w |newHighs |newLows |advancingVolume |unchangedVolume |decliningVolume |advancingIssues |percentAdvancingIssues |unchangedIssues |percentUnchangedIssues |decliningIssues |percentDecliningIssues |
|:------|:-------|:------|:--------|:-------|:---------------|:---------------|:---------------|:---------------|:----------------------|:---------------|:----------------------|:---------------|:----------------------|
|  SDAQ |13      |195    |16       |137     |1,819,150,000   |41,480,000      |882,270,000     |1,412           |57                     |93              |4                      |987             |40                     |
|NYSE   |6       |217    |4        |203     |3,789,630,000   |53,820,000      |1,702,630,000   |1,299           |64                     |46              |2                      |690             |34                     |
|OTC    |0       |0      |1        |1       |2,870,000       |1,950,000       |70,000          |7               |35                     |1               |5                      |12              |60                     |
|AMEX   |0       |21     |0        |11      |44,830,000      |3,190,000       |77,680,000      |81              |34                     |22              |9                      |133             |56                     |
    

The [Bigcharts]( http://bigcharts.marketwatch.com/default.asp) has daily stock prices.


{% highlight r %}
out = qis.bigcharts.quote('AMZN,FB')
print(to.nice(out$AMZN))
{% endhighlight %}



|           |Open  |High  |Low   |Close |Volume     |Adjusted |
|:----------|:-----|:-----|:-----|:-----|:----------|:--------|
|2018-02-12 |1,329 |1,384 |1,266 |1,340 |14,034,205 |1,340    |
    




{% highlight r %}
print(to.nice(out$FB))
{% endhighlight %}



|           |Open |High |Low |Close |Volume     |Adjusted |
|:----------|:----|:----|:---|:-----|:----------|:--------|
|2018-02-12 |181  |177  |167 |176   |38,544,630 |176      |
    


*(this report was produced on: 2018-02-12)*
