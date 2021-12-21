---
layout: post
title: Financial data from Yahoo
comments: true
---




Here I will show how one can get different key statistics about different companies from the [Yahoo](https://finance.yahoo.com/).
For example, AMZN key statistics can be viewed here [AMZN statistics](https://finance.yahoo.com/quote/AMZN/key-statistics?p=AMZN).
 It is not that complicated to create an R script which would get tables from the web page. Here is an example.



{% highlight r %}
#*****************************************************************
# Get key statistics for AMZN
#*****************************************************************
symbol = 'AMZN'
{% endhighlight %}

 Here we can get to the [Statistics](https://finance.yahoo.com/quote/AMZN/key-statistics?p=AMZN) of the AMZN


{% highlight r %}
out = qis.yahoo.info(symbol, "statistics")
{% endhighlight %}

 Here we have Valuation Measures


{% highlight r %}
print(to.nice(out[['Valuation Measures']]))
{% endhighlight %}



|2018-02-12                  |AMZN    |
|:---------------------------|:-------|
|Market Cap (intraday) 5     |648.51B |
|Enterprise Value 3          |661.67B |
|Trailing P/E                |217.8   |
|Forward P/E 1               |88.2    |
|PEG Ratio (5 yr expected) 1 |5.9     |
|Price/Sales (ttm)           |3.6     |
|Price/Book (mrq)            |23.4    |
|Enterprise Value/Revenue 3  |3.7     |
|Enterprise Value/EBITDA 6   |44.0    |
    

 Here we have Financial Highlights


{% highlight r %}
print(to.nice(out[['Financial Highlights']]))
{% endhighlight %}



|2018-02-12                      |AMZN         |
|:-------------------------------|:------------|
|Fiscal Year Ends                |Dec 31, 2017 |
|Most Recent Quarter (mrq)       |Dec 31, 2017 |
|Profit Margin                   |1.70%        |
|Operating Margin (ttm)          |2.31%        |
|Return on Assets (ttm)          |2.39%        |
|Return on Equity (ttm)          |12.91%       |
|Revenue (ttm)                   |177.87B      |
|Revenue Per Share (ttm)         |370.6        |
|Quarterly Revenue Growth (yoy)  |38.20%       |
|Gross Profit (ttm)              |65.93B       |
|EBITDA                          |15.04B       |
|Net Income Avi to Common (ttm)  |3.03B        |
|Diluted EPS (ttm)               |6.2          |
|Quarterly Earnings Growth (yoy) |147.90%      |
|Total Cash (mrq)                |30.99B       |
|Total Cash Per Share (mrq)      |64.0         |
|Total Debt (mrq)                |44.15B       |
|Total Debt/Equity (mrq)         |159.3        |
|Current Ratio (mrq)             |1.0          |
|Book Value Per Share (mrq)      |57.2         |
|Operating Cash Flow (ttm)       |18.43B       |
|Levered Free Cash Flow (ttm)    |9.41B        |
    

 Here we have Trading Information


{% highlight r %}
print(to.nice(out[['Trading Information']]))
{% endhighlight %}



|2018-02-12                        |AMZN        |
|:---------------------------------|:-----------|
|Beta                              |1.8         |
|52-Week Change 3                  |61.44%      |
|SP500 52-Week Change 3            |10.86%      |
|52 Week High 3                    |1,498.00    |
|52 Week Low 3                     |828.5       |
|50-Day Moving Average 3           |1,311.36    |
|200-Day Moving Average 3          |1,104.40    |
|Avg Vol (3 month) 3               |4.56M       |
|Avg Vol (10 day) 3                |10.38M      |
|Shares Outstanding 5              |484.11M     |
|Float                             |404.81M     |
|% Held by Insiders 1              |16.43%      |
|% Held by Institutions 1          |60.79%      |
|Shares Short 3                    |5.6M        |
|Short Ratio 3                     |1.4         |
|Short % of Float 3                |1.41%       |
|Shares Short (prior month) 3      |5.93M       |
|Forward Annual Dividend Rate 4    |N/A         |
|Forward Annual Dividend Yield 4   |N/A         |
|Trailing Annual Dividend Rate 3   |N/A         |
|Trailing Annual Dividend Yield 3  |N/A         |
|5 Year Average Dividend Yield 4   |N/A         |
|Payout Ratio 4                    |N/A         |
|Dividend Date 3                   |N/A         |
|Ex-Dividend Date 4                |N/A         |
|Last Split Factor (new per old) 2 |2/1         |
|Last Split Date 3                 |Sep 2, 1999 |
    

 Here we can get to the [Financials](https://finance.yahoo.com/quote/AMZN/financials?p=AMZN) of the AMZN


{% highlight r %}
out = qis.yahoo.info(symbol, "financials") 
{% endhighlight %}

 Here we have Income Statement


{% highlight r %}
print(to.nice(out[['Income Statement']]))
{% endhighlight %}



|2018-02-12                             |AMZN        |AMZN        |AMZN        |
|:--------------------------------------|:-----------|:-----------|:-----------|
|Revenue                                |12/31/2017  |12/31/2016  |12/31/2015  |
|Total Revenue                          |177,866,000 |135,987,000 |107,006,000 |
|Cost of Revenue                        |111,934,000 |88,265,000  |71,651,000  |
|Gross Profit                           |65,932,000  |47,722,000  |35,355,000  |
|Operating Expenses                     |            |            |            |
|Research Development                   |-           |-           |-           |
|Selling General and Administrative     |61,826,000  |43,536,000  |33,122,000  |
|Non Recurring                          |-           |-           |-           |
|Others                                 |-           |-           |-           |
|Total Operating Expenses               |-           |-           |-           |
|Operating Income or Loss               |4,106,000   |4,186,000   |2,233,000   |
|Income from Continuing Operations      |            |            |            |
|Total Other Income/Expenses Net        |548,000     |190,000     |-206,000    |
|Earnings Before Interest and Taxes     |4,654,000   |4,376,000   |2,027,000   |
|Interest Expense                       |848,000     |484,000     |459,000     |
|Income Before Tax                      |3,806,000   |3,892,000   |1,568,000   |
|Income Tax Expense                     |769,000     |1,425,000   |950,000     |
|Minority Interest                      |-           |-           |-           |
|Net Income From Continuing Ops         |3,033,000   |2,371,000   |596,000     |
|Non-recurring Events                   |            |            |            |
|Discontinued Operations                |-           |-           |-           |
|Extraordinary Items                    |-           |-           |-           |
|Effect Of Accounting Changes           |-           |-           |-           |
|Other Items                            |-           |-           |-           |
|Net Income                             |            |            |            |
|Net Income                             |3,033,000   |2,371,000   |596,000     |
|Preferred Stock And Other Adjustments  |-           |-           |-           |
|Net Income Applicable To Common Shares |3,033,000   |2,371,000   |596,000     |
    

 Here we have Balance Sheet


{% highlight r %}
print(to.nice(out[['Balance Sheet']]))
{% endhighlight %}



|2018-02-12                           |AMZN        |AMZN       |AMZN       |
|:------------------------------------|:-----------|:----------|:----------|
|Period Ending                        |12/31/2017  |12/31/2016 |12/31/2015 |
|Current Assets                       |            |           |           |
|Cash And Cash Equivalents            |20,522,000  |19,334,000 |15,890,000 |
|Short Term Investments               |10,464,000  |6,647,000  |3,918,000  |
|Net Receivables                      |13,164,000  |8,339,000  |5,654,000  |
|Inventory                            |16,047,000  |11,461,000 |10,243,000 |
|Other Current Assets                 |-           |-          |-          |
|Total Current Assets                 |60,197,000  |45,781,000 |35,705,000 |
|Long Term Investments                |-           |-          |-          |
|Property Plant and Equipment         |48,866,000  |29,114,000 |21,838,000 |
|Goodwill                             |13,350,000  |3,784,000  |3,759,000  |
|Intangible Assets                    |-           |-          |-          |
|Accumulated Amortization             |-           |-          |-          |
|Other Assets                         |8,897,000   |4,723,000  |3,445,000  |
|Deferred Long Term Asset Charges     |-           |-          |-          |
|Total Assets                         |131,310,000 |83,402,000 |64,747,000 |
|Current Liabilities                  |            |           |           |
|Accounts Payable                     |52,786,000  |39,048,000 |30,769,000 |
|Short/Current Long Term Debt         |-           |-          |-          |
|Other Current Liabilities            |5,097,000   |4,768,000  |3,118,000  |
|Total Current Liabilities            |57,883,000  |43,816,000 |33,887,000 |
|Long Term Debt                       |24,743,000  |7,694,000  |8,227,000  |
|Other Liabilities                    |20,975,000  |12,607,000 |9,249,000  |
|Deferred Long Term Liability Charges |-           |-          |-          |
|Minority Interest                    |-           |-          |-          |
|Negative Goodwill                    |-           |-          |-          |
|Total Liabilities                    |103,601,000 |64,117,000 |51,363,000 |
|Stockholders&#x27; Equity            |            |           |           |
|Misc. Stocks Options Warrants        |-           |-          |-          |
|Redeemable Preferred Stock           |-           |-          |-          |
|Preferred Stock                      |-           |-          |-          |
|Common Stock                         |5,000       |5,000      |5,000      |
|Retained Earnings                    |8,636,000   |4,916,000  |2,545,000  |
|Treasury Stock                       |-1,837,000  |-1,837,000 |-1,837,000 |
|Capital Surplus                      |21,389,000  |17,186,000 |13,394,000 |
|Other Stockholder Equity             |-484,000    |-985,000   |-723,000   |
|Total Stockholder Equity             |27,709,000  |19,285,000 |13,384,000 |
|Net Tangible Assets                  |14,359,000  |15,501,000 |9,625,000  |
    

 Here we have Cash Flow


{% highlight r %}
print(to.nice(out[['Cash Flow']]))
{% endhighlight %}



|2018-02-12                                              |AMZN        |AMZN       |AMZN       |
|:-------------------------------------------------------|:-----------|:----------|:----------|
|Period Ending                                           |12/31/2017  |12/31/2016 |12/31/2015 |
|Net Income                                              |3,033,000   |2,371,000  |596,000    |
|Operating Activities, Cash Flows Provided By or Used In |            |           |           |
|Depreciation                                            |11,478,000  |8,116,000  |6,281,000  |
|Adjustments To Net Income                               |4,096,000   |2,869,000  |2,605,000  |
|Changes In Accounts Receivables                         |-4,786,000  |-3,367,000 |-1,755,000 |
|Changes In Liabilities                                  |8,196,000   |8,709,000  |6,499,000  |
|Changes In Inventories                                  |-3,583,000  |-1,426,000 |-2,187,000 |
|Changes In Other Operating Activities                   |-           |-          |-          |
|Total Cash Flow From Operating Activities               |18,434,000  |17,272,000 |12,039,000 |
|Investing Activities, Cash Flows Provided By or Used In |            |           |           |
|Capital Expenditures                                    |-11,955,000 |-7,804,000 |-5,387,000 |
|Investments                                             |-3,789,000  |-3,023,000 |-1,066,000 |
|Other Cash flows from Investing Activities              |-12,075,000 |951,000    |3,000      |
|Total Cash Flows From Investing Activities              |-27,819,000 |-9,876,000 |-6,450,000 |
|Financing Activities, Cash Flows Provided By or Used In |            |           |           |
|Dividends Paid                                          |-           |-          |-          |
|Sale Purchase of Stock                                  |-           |-          |-          |
|Net Borrowings                                          |9,860,000   |-3,740,000 |-3,882,000 |
|Other Cash Flows from Financing Activities              |-           |-          |-          |
|Total Cash Flows From Financing Activities              |9,860,000   |-3,740,000 |-3,882,000 |
|Effect Of Exchange Rate Changes                         |713,000     |-212,000   |-374,000   |
|Change In Cash and Cash Equivalents                     |1,188,000   |3,444,000  |1,333,000  |
    

 Here we can get to the [Summary](https://finance.yahoo.com/quote/AMZN/financials?p=AMZN) of the AMZN


{% highlight r %}
out = qis.yahoo.info(symbol, "summary") 
{% endhighlight %}

 Here we have Summary


{% highlight r %}
print(to.nice(out[['Summary']]))
{% endhighlight %}



|2018-02-12              |AMZN                        |
|:-----------------------|:---------------------------|
|Previous Close          |1,350.50                    |
|Open                    |1,373.49                    |
|Bid                     |1,373.50 x 100              |
|Ask                     |1,376.00 x 200              |
|Day&#x27;s Range        |1,265.93 - 1,383.50         |
|52 Week Range           |828.55 - 1,498.00           |
|Volume                  |14,141,524                  |
|Avg. Volume             |4,560,477                   |
|Market Cap              |648.51B                     |
|Beta                    |1.8                         |
|PE Ratio (TTM)          |217.8                       |
|EPS (TTM)               |6.2                         |
|Earnings Date           |Apr 25, 2018 - Apr 30, 2018 |
|Forward Dividend  Yield |N/A (N/A)                   |
|Ex-Dividend Date        |N/A                         |
|1y Target Est           |1,640.59                    |
    

Here we can get portfolio view of AMZN


{% highlight r %}
out = qis.yahoo.portfolio('AMZN')
{% endhighlight %}

 Here we have a detailed summary


{% highlight r %}
print(to.nice(out))
{% endhighlight %}



|                              |1                  |
|:-----------------------------|:------------------|
|language                      |en-US              |
|quoteType                     |EQUITY             |
|currency                      |USD                |
|regularMarketPrice            |1,339.60           |
|regularMarketTime             |1,518,210,000.00   |
|regularMarketChange           |-10.90             |
|regularMarketOpen             |1,373.49           |
|regularMarketDayHigh          |1,383.50           |
|regularMarketDayLow           |1,265.93           |
|regularMarketVolume           |14,141,524.00      |
|exchange                      |NMS                |
|shortName                     |Amazon.com, Inc.   |
|sharesOutstanding             |484,107,008.00     |
|targetPriceMean               |1,640.59           |
|priceHint                     |2.00               |
|revenue                       |177,865,998,000.00 |
|priceToSales                  |3.65               |
|epsTrailingTwelveMonths       |6.15               |
|exchangeDataDelayedBy         |0.00               |
|marketState                   |CLOSED             |
|market                        |us_market          |
|fiftyTwoWeekLowChangePercent  |0.06               |
|fiftyTwoWeekHighChange        |-43.90             |
|fiftyTwoWeekHighChangePercent |-0.03              |
|fiftyTwoWeekLow               |1,265.93           |
|fiftyTwoWeekHigh              |1,383.50           |
|trailingPE                    |217.82             |
|marketCap                     |648,509,718,528.00 |
|sourceInterval                |15.00              |
|exchangeTimezoneName          |America/New_York   |
|exchangeTimezoneShortName     |EST                |
|gmtOffSetMilliseconds         |-18,000,000.00     |
|esgPopulated                  |FALSE              |
|tradeable                     |TRUE               |
|regularMarketPreviousClose    |1,350.50           |
|bid                           |1,373.50           |
|ask                           |1,376.00           |
|fullExchangeName              |NasdaqGS           |
|longName                      |Amazon.com, Inc.   |
|averageDailyVolume3Month      |4,560,477.00       |
|fiftyTwoWeekLowChange         |73.67              |
|symbol                        |AMZN               |
    

Let's plot historical performance for AMZN combining most recent valuation details.


{% highlight r %}
out = qis.yahoo.info(symbol, "statistics")[['Valuation Measures']]

# download price history from Yahoo
data = getSymbols(symbol, from = '2010-01-01', auto.assign = FALSE)

# Compute 50,100 days moving average
y      = data['2010::2018']
sma50  = SMA(Cl(y), 50)
sma100 = SMA(Cl(y), 100)


# Plot financial information with  historical price and volume levels
layout(c(1,1,2,3,3))
qis.plot(y, type = 'candle', main = symbol, plotX = F)
qis.plot.lines(sma50,  col='blue')
qis.plot.lines(sma100, col='red')
qis.plot.legend(c(symbol,'SMA 50','SMA 100'), 'green,blue,red', list(y,sma50,sma100))
qis.plot(qis.plot.scale.volume(y), type = 'volume')
qis.plot.table(out)
{% endhighlight %}

![plot of chunk plot-15](/public/images/2018-01-22-company-financials-yahoo/plot-15-1.png)


*(this report was produced on: 2018-02-12)*
