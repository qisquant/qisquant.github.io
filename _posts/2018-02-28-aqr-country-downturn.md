---
layout: post
title: Country downturns and future returns
comments: true
---




Today I woud like to show that if country is down few years in a row it historically recovers in the next few years. I will use a country returns data from 
[AQR](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)


{% highlight r %}
#*****************************************************************
# Load country returns from AQR data library
#******************************************************************

# monthly market returns in excess of t-bills
market.excess.aqr = qis.aqr.data('betting-against-beta-equity-factors', 'monthly', 'MKT')

# monthly U.S. Treasury bill rates.
risk.free.aqr     =  qis.aqr.data('betting-against-beta-equity-factors', 'monthly', 'RF', last.col2extract = 2)

# total market return accoridng to AQR data
aqr.market        = market.excess.aqr + as.vector(risk.free.aqr)

# check data starting dates
print(qis.xts.start.dates(aqr.market))
{% endhighlight %}



|USA        |CAN        |AUS        |AUT        |BEL        |CHE        |DEU        |DNK        |ESP        |FIN        |FRA        |GBR        |HKG        |IRL        |ITA        |JPN        |NLD        |NOR        |NZL        |SGP        |SWE        |PRT        |GRC        |ISR        |
|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|
|1926-07-31 |1982-03-31 |1985-11-30 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1986-01-31 |1988-02-29 |1988-09-30 |1994-12-31 |
    




{% highlight r %}
# compute equity 
country.index = qis.apply.matrix(1 + ifna(aqr.market,0),cumprod)
country.index[is.na(aqr.market)] = NA

#*****************************************************************
# Clean and normalize data
#******************************************************************
year.ends = qis.date.ends(country.index,"year")

# create back-test environment with annual prices 
data = env()
for(i in names(aqr.market))
  data[[i]] = make.stock.xts(country.index[year.ends,i])

qis.prep(data, align='keep.all', fill.gaps = F, dates='1986::')
qis.plot.xts( scale.one(data$prices), main='Normalized Country Performance')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-28-aqr-country-downturn/plot-2-1.png)

{% highlight r %}
#*****************************************************************
# Signal for country being down 1,2,3,4,5 yers in the row
#*****************************************************************
prices = data$prices
n      = ncol(prices)
ret    = prices / mlag(prices) - 1

# Signals
signal  = ret < 0
signals = list(d1y  = signal,
               d2y  = signal & mlag(signal),
               d3yr = signal & mlag(signal) & mlag(signal,2)
               )

#*****************************************************************
# Signal performance calculations
#*****************************************************************
signal.performance = function(signals, ret) {
  stats = lst()
  for(signal in names(signals)) {
    temp  = signals[[signal]]
    # get next year return if prev year it was down
    stats[[paste0(signal,'-h1y')]] = coredata(ret)[ifna(mlag(temp),F)]
    # get average of 2 years return if prev year it was down
    stats[[paste0(signal,'-h2y')]] = 1/2 * coredata(ret + mlag(ret,-1))[ifna(mlag(temp),F)]
    # get average of 3 years return if prev year it was down
    stats[[paste0(signal,'-h3y')]] = 1/3 * coredata(ret + mlag(ret,-1) + mlag(ret,-2))[ifna(mlag(temp),F)]
  }
  stats = lapply(stats,na.omit)
  # mean returns
  #print(sapply(stats,mean)) # mean
  
  # make a barplot
  par(mar = c(8, 4, 2, 1))
  boxplot(stats,las=2, col = col.add.alpha('green', 100), main = "Strategy Average Forward Return")
  abline(h=0,col='gray')
}

#*****************************************************************
# Show average forward return per signal
#*****************************************************************
signal.performance(signals, ret)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-28-aqr-country-downturn/plot-2-2.png)

The longer is downturn, the better average forward return.

*(this report was produced on: 2018-02-28)*
