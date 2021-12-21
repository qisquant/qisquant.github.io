---
layout: post
title: Country returns from AQR and Fama French data library
comments: true
---




Today I woud like to calculate cumulative country returns using [AQR](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)
and [Fama-French](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/) data library.


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
#*****************************************************************
# Read historical data from Kenneth R. French - Data Library
#*****************************************************************
data                = qis.fama.french.data('F-F_Research_Data_Factors', 'months')
market.excess.ff.us = data[[1]]$Mkt.RF
risk.free.ff.us     = data[[1]]$RF

data                = qis.fama.french.data('F-F_International_Countries', 'months', file.suffix='') 

temp = env()
for(n in ls(data))
  temp[[n]] = make.stock.xts(data[[n]][[1]]$.Mkt)
temp$US = make.stock.xts(market.excess.ff.us + risk.free.ff.us)

qis.prep(temp, align='keep.all', fill.gaps = F)

# total market return accoridng to FF data
ff.market = temp$prices

print(qis.xts.start.dates(ff.market))
{% endhighlight %}



|US         |Austrlia   |Belgium    |France     |Germany    |HongKong   |Italy      |Japan      |Nethrlnd   |Norway     |Singapor   |Spain      |Sweden     |Swtzrlnd   |UK         |Canada     |Austria    |Finland    |NewZland   |Denmark    |Ireland    |Malaysia   |
|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|:----------|
|1926-07-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1975-01-01 |1977-01-01 |1987-01-01 |1988-01-01 |1988-01-01 |1989-01-01 |1991-01-01 |1994-01-01 |
    




{% highlight r %}
#*****************************************************************
# Merge AQR and FF data
#*****************************************************************

# convert data into same monthly format	
aqr.market = qis.xts.format.dates(aqr.market, "yyyymm")
ff.market  = qis.xts.format.dates(ff.market / 100, "yyyymm")

# map country codes between AQR and FF	
codes          = qis.country.code()
aqr.map        = codes[,'Country']
names(aqr.map) = codes[,'Code2']

ff.map             = names(ff.market)	
names(ff.map)      = ff.map
ff.map['Austrlia'] = 'Australia'
ff.map['HongKong'] = "Hong Kong, SAR China"
ff.map['Swtzrlnd'] = 'Switzerland'
ff.map['Singapor'] = 'Singapore'
ff.map['NewZland'] = "New Zealand"
ff.map['UK']       = "United Kingdom"
ff.map['US']       = "United States of America"
ff.map['Nethrlnd'] = "Netherlands"
ff.map['Nethrlnd'] = "Netherlands"

# print differences
setdiff( aqr.map[names(aqr.market)] , ff.map[names(ff.market)] )
{% endhighlight %}

[1] "Greece"   "Israel"   "Portugal"


{% highlight r %}
setdiff( ff.map[names(ff.market)]   , aqr.map[names(aqr.market)] )
{% endhighlight %}

[1] "Malaysia"


{% highlight r %}
#*****************************************************************
# Plot cumulative country returns using AQR and FF data
#*****************************************************************
layout(matrix(1:4,2,2))
common = intersect( aqr.map[names(aqr.market)] , ff.map[names(ff.market)] )

for(i in common) {
  aqr.index = which(aqr.map[names(aqr.market)] ==i)
  ff.index  = which(ff.map[names(ff.market)] == i)
  
  temp = merge(na.omit(aqr.market[,aqr.index]), na.omit(ff.market[,ff.index]))
  temp = temp[rowSums(!is.na(temp)) == ncol(temp),]
  temp = qis.apply.matrix(1+temp,cumprod)
  
  qis.plot(temp[,1], type='l', main = i, ylim = range(temp), col = col.add.alpha('red', 100))
  qis.plot.lines(temp[,2], type='l',col = col.add.alpha('blue', 100))
  qis.plot.legend(paste(spl('AQR,FF'), i),'red,blue',as.list(temp))
}
{% endhighlight %}

![plot of chunk plot-2](/public/images/2018-02-20-aqr-ff-country-returns/plot-2-1.png)![plot of chunk plot-2](/public/images/2018-02-20-aqr-ff-country-returns/plot-2-2.png)![plot of chunk plot-2](/public/images/2018-02-20-aqr-ff-country-returns/plot-2-3.png)![plot of chunk plot-2](/public/images/2018-02-20-aqr-ff-country-returns/plot-2-4.png)![plot of chunk plot-2](/public/images/2018-02-20-aqr-ff-country-returns/plot-2-5.png)![plot of chunk plot-2](/public/images/2018-02-20-aqr-ff-country-returns/plot-2-6.png)


*(this report was produced on: 2018-02-21)*
