---
layout: post
title: Extract index compositions from the web
comments: true
---




Today I woud like to show how to load data for different indexes such as [DOW JONES](http://money.cnn.com/data/dow30/),
[NASDAQ 100](https://www.nasdaq.com/markets/indices/nasdaq-100.aspx), [S&P 500](http://en.wikipedia.org/wiki/List_of_S%26P_500_companies),
[SPDR ETF](http://www.sectorspdr.com/sectorspdr/). 



{% highlight r %}
#******************************************************************
# Extract components from Dow Jones Industrial Average
#****************************************************************** 
{% endhighlight %}

{% highlight r %}
qis.dj30.components()
qis.dj30.components("prices")
{% endhighlight %}

{% highlight r %}
print(to.nice(qis.dj30.components("all")))
{% endhighlight %}



|Company |Description             |Price  |Change |% Change |Volume      |YTDchange |
|:-------|:-----------------------|:------|:------|:--------|:-----------|:---------|
|MMM     |3M                      |225.21 |2.32   |+1.04%   |4,518,613   |-4.32%    |
|AXP     |American Express        |91.72  |3.38   |+3.83%   |8,017,551   |-7.64%    |
|AAPL    |Apple                   |156.41 |1.89   |+1.22%   |70,672,608  |-7.58%    |
|BA      |Boeing                  |332.83 |3.17   |+0.96%   |8,832,974   |+12.86%   |
|CAT     |Caterpillar             |149.21 |3.22   |+2.21%   |9,528,067   |-5.31%    |
|CVX     |Chevron                 |113.50 |1.20   |+1.07%   |10,922,392  |-9.34%    |
|CSCO    |Cisco                   |39.53  |0.76   |+1.96%   |51,304,217  |+3.21%    |
|KO      |Coca-Cola               |43.13  |0.03   |+0.07%   |22,853,126  |-5.99%    |
|DIS     |Disney                  |103.09 |1.74   |+1.72%   |12,479,727  |-4.11%    |
|DWDP    |DowDuPont Inc           |69.48  |1.27   |+1.86%   |13,137,503  |-2.44%    |
|XOM     |Exxon Mobil             |75.78  |0.48   |+0.64%   |29,491,592  |-9.40%    |
|GE      |General Electric        |14.94  |0.49   |+3.39%   |129,055,811 |-14.38%   |
|GS      |Goldman Sachs           |249.30 |2.95   |+1.20%   |5,978,553   |-2.14%    |
|HD      |Home Depot              |184.12 |2.90   |+1.60%   |9,059,239   |-2.85%    |
|IBM     |IBM                     |149.51 |1.92   |+1.30%   |7,828,290   |-2.55%    |
|INTC    |Intel                   |43.95  |1.20   |+2.81%   |49,301,141  |-4.79%    |
|JNJ     |Johnson & Johnson       |129.53 |3.17   |+2.51%   |15,030,881  |-7.29%    |
|JPM     |JPMorgan Chase          |110.04 |2.16   |+2.00%   |28,188,027  |+2.90%    |
|MCD     |McDonald's              |160.80 |1.83   |+1.15%   |7,187,079   |-6.58%    |
|MRK     |Merck                   |54.87  |0.14   |+0.26%   |18,580,528  |-2.49%    |
|MSFT    |Microsoft               |88.18  |3.17   |+3.73%   |63,499,065  |+3.09%    |
|NKE     |Nike                    |65.49  |3.00   |+4.80%   |13,733,337  |+4.70%    |
|PFE     |Pfizer                  |34.16  |0.53   |+1.58%   |47,297,681  |-5.69%    |
|PG      |Procter & Gamble        |79.92  |-0.30  |-0.37%   |18,838,829  |-13.02%   |
|TRV     |Travelers Companies Inc |137.08 |2.07   |+1.53%   |3,655,399   |+1.06%    |
|UTX     |United Technologies     |125.03 |-2.45  |-1.92%   |12,116,595  |-1.99%    |
|UNH     |UnitedHealth            |220.96 |4.50   |+2.08%   |6,225,326   |+0.23%    |
|VZ      |Verizon                 |49.88  |0.84   |+1.71%   |27,707,860  |-5.76%    |
|V       |Visa                    |116.32 |2.46   |+2.16%   |17,695,515  |+2.02%    |
|WMT     |Wal-Mart                |99.37  |-0.65  |-0.65%   |14,184,609  |+0.63%    |
    




{% highlight r %}
#******************************************************************
# Extract components from The Nasdaq-100 Index
#****************************************************************** 
{% endhighlight %}

{% highlight r %}
qis.nasdaq100.components()
qis.nasdaq100.components("prices")
{% endhighlight %}

{% highlight r %}
print(to.nice(qis.nasdaq100.components("all")))
{% endhighlight %}



|Symbol |Name                                       |lastsale |netchange |pctchange |share_volume  |Nasdaq100_points |
|:------|:------------------------------------------|:--------|:---------|:---------|:-------------|:----------------|
|ATVI   |Activision Blizzard Inc                    |67.10    |1.25      |1.90      |18,582,250.00 |0.80             |
|ADBE   |Adobe Systems Incorporated                 |188.00   |2.83      |1.53      |3,818,835.00  |1.20             |
|ALXN   |Alexion Pharmaceuticals Inc.               |108.60   |0.13      |0.12      |3,039,212.00  |0.00             |
|ALGN   |Align Technology Inc.                      |229.40   |8.66      |3.92      |1,549,902.00  |0.60             |
|GOOG   |Alphabet Inc.                              |1,037.80 |36.26     |3.62      |3,505,862.00  |11.10            |
|GOOGL  |Alphabet Inc.                              |1,046.30 |38.56     |3.83      |4,917,970.00  |10.10            |
|AMZN   |Amazon.com Inc.                            |1,339.60 |-10.90    |-0.81     |14,141,524.00 |-4.60            |
|AAL    |American Airlines Group Inc.               |48.40    |-0.24     |-0.49     |6,837,776.00  |-0.10            |
|AMGN   |Amgen Inc.                                 |173.50   |0.34      |0.20      |6,093,941.00  |0.20             |
|ADI    |Analog Devices Inc.                        |83.90    |1.25      |1.51      |5,649,666.00  |0.40             |
|AAPL   |Apple Inc.                                 |156.40   |1.89      |1.22      |70,672,608.00 |8.50             |
|AMAT   |Applied Materials Inc.                     |48.10    |2.33      |5.09      |22,648,164.00 |2.20             |
|ASML   |ASML Holding N.V.                          |183.70   |3.77      |2.10      |2,070,846.00  |0.20             |
|ADSK   |Autodesk Inc.                              |105.90   |1.13      |1.08      |3,923,885.00  |0.20             |
|ADP    |Automatic Data Processing Inc.             |111.40   |3.18      |2.94      |3,516,051.00  |1.20             |
|BIDU   |Baidu Inc.                                 |215.70   |3.60      |1.70      |4,768,365.00  |0.90             |
|BIIB   |Biogen Inc.                                |316.80   |4.99      |1.60      |2,180,365.00  |0.90             |
|BMRN   |BioMarin Pharmaceutical Inc.               |80.30    |-1.83     |-2.23     |2,215,705.00  |-0.30            |
|AVGO   |Broadcom Limited                           |235.50   |5.93      |2.58      |4,328,248.00  |2.10             |
|CA     |CA Inc.                                    |33.50    |0.79      |2.42      |4,416,064.00  |0.30             |
|CDNS   |Cadence Design Systems Inc.                |37.70    |0.86      |2.34      |3,471,576.00  |0.20             |
|CELG   |Celgene Corporation                        |92.50    |1.49      |1.64      |11,150,259.00 |1.00             |
|CERN   |Cerner Corporation                         |61.30    |0.09      |0.15      |3,790,825.00  |0.00             |
|CHTR   |Charter Communications Inc.                |349.40   |0.78      |0.22      |1,908,166.00  |0.20             |
|CHKP   |Check Point Software Technologies Ltd.     |97.80    |-0.36     |-0.37     |2,309,258.00  |-0.10            |
|CTAS   |Cintas Corporation                         |152.70   |3.41      |2.28      |991,331.00    |0.30             |
|CSCO   |Cisco Systems Inc.                         |39.50    |0.76      |1.96      |51,304,217.00 |3.30             |
|CTXS   |Citrix Systems Inc.                        |85.80    |1.22      |1.44      |2,010,487.00  |0.20             |
|CTSH   |Cognizant Technology Solutions Corporation |76.30    |1.13      |1.50      |6,550,172.00  |0.60             |
|CMCSA  |Comcast Corporation                        |38.60    |0.38      |1.00      |37,376,044.00 |1.60             |
|COST   |Costco Wholesale Corporation               |180.70   |2.11      |1.18      |2,777,218.00  |0.80             |
|CSX    |CSX Corporation                            |50.90    |0.42      |0.83      |12,278,165.00 |0.30             |
|CTRP   |Ctrip.com International Ltd.               |43.80    |-0.79     |-1.77     |7,614,701.00  |-0.30            |
|XRAY   |DENTSPLY SIRO   Inc.                       |57.20    |0.33      |0.58      |3,480,729.00  |0.10             |
|DISH   |DISH Network Corporation                   |43.40    |-0.37     |-0.85     |3,599,719.00  |-0.10            |
|DLTR   |Dollar Tree Inc.                           |107.60   |6.04      |5.95      |4,168,825.00  |1.30             |
|EBAY   |eBay Inc.                                  |41.70    |0.64      |1.56      |15,976,151.00 |0.60             |
|EA     |Electronic Arts Inc.                       |120.60   |4.10      |3.52      |5,945,147.00  |1.10             |
|EXPE   |Expedia Inc.                               |104.00   |-19.03    |-15.47    |17,183,644.00 |-2.30            |
|ESRX   |Express Scripts Holding Company            |71.70    |-1.64     |-2.24     |9,262,811.00  |-0.80            |
|FB     |Facebook Inc.                              |176.10   |4.53      |2.64      |39,887,626.00 |9.40             |
|FAST   |Fastenal Company                           |53.50    |1.33      |2.55      |3,700,891.00  |0.30             |
|FISV   |Fiserv Inc.                                |137.50   |4.41      |3.31      |2,656,329.00  |0.80             |
|GILD   |Gilead Sciences Inc.                       |79.30    |1.10      |1.41      |13,746,392.00 |1.30             |
|HAS    |Hasbro Inc.                                |97.70    |1.25      |1.30      |2,911,493.00  |0.10             |
|HSIC   |Henry Schein Inc.                          |71.80    |0.90      |1.27      |1,865,371.00  |0.10             |
|HOLX   |Hologic Inc.                               |37.80    |-1.05     |-2.71     |8,335,009.00  |-0.30            |
|IDXX   |IDEXX Laboratories Inc.                    |172.90   |3.67      |2.17      |726,185.00    |0.30             |
|ILMN   |Illumina Inc.                              |217.20   |7.65      |3.65      |1,798,295.00  |1.00             |
|INCY   |Incyte Corporation                         |84.50    |0.58      |0.69      |2,399,153.00  |0.10             |
|INTC   |Intel Corporation                          |44.00    |1.20      |2.81      |49,301,141.00 |4.90             |
|INTU   |Intuit Inc.                                |157.10   |4.31      |2.82      |2,175,685.00  |1.00             |
|ISRG   |Intuitive Surgical Inc.                    |392.90   |11.04     |2.89      |1,301,214.00  |1.10             |
|JBHT   |J.B. Hunt Transport Services Inc.          |115.10   |0.30      |0.26      |1,792,012.00  |0.00             |
|JD     |JD.com Inc.                                |42.90    |0.68      |1.61      |18,325,512.00 |0.60             |
|KLAC   |KLA-Tencor Corporation                     |100.90   |2.36      |2.39      |1,734,594.00  |0.30             |
|LRCX   |Lam Research Corporation                   |165.90   |3.64      |2.24      |5,937,091.00  |0.50             |
|LBTYA  |Liberty Global plc                         |35.10    |0.02      |0.06      |2,266,529.00  |0.00             |
|LBTYK  |Liberty Global plc                         |34.00    |0.13      |0.38      |3,748,957.00  |0.10             |
|LVNTA  |Liberty Interactive Corporation            |54.50    |-0.33     |-0.60     |687,582.00    |0.00             |
|QVCA   |Liberty Interactive Corporation            |26.60    |0.23      |0.87      |3,640,094.00  |0.10             |
|MAR    |Marriott International                     |136.30   |2.41      |1.80      |2,885,138.00  |0.80             |
|MXIM   |Maxim Integrated Products Inc.             |58.00    |2.35      |4.23      |4,376,125.00  |0.60             |
|MELI   |MercadoLibre Inc.                          |335.90   |-9.45     |-2.74     |1,575,893.00  |-0.40            |
|MCHP   |Microchip Technology Incorporated          |81.00    |1.14      |1.43      |5,716,768.00  |0.20             |
|MU     |Micron Technology Inc.                     |40.40    |0.41      |1.02      |65,189,252.00 |0.40             |
|MSFT   |Microsoft Corporation                      |88.20    |3.17      |3.73      |63,499,065.00 |21.40            |
|MDLZ   |Mondelez International Inc.                |42.10    |-0.54     |-1.27     |13,993,492.00 |-0.70            |
|MNST   |Monster Beverage Corporation               |62.90    |0.90      |1.45      |2,679,822.00  |0.40             |
|MYL    |Mylan N.V.                                 |39.30    |0.03      |0.08      |7,204,391.00  |0.00             |
|NTES   |NetEase Inc.                               |291.60   |2.02      |0.70      |2,421,496.00  |0.10             |
|NFLX   |Netflix Inc.                               |249.50   |-0.63     |-0.25     |16,906,942.00 |-0.20            |
|NVDA   |NVIDIA Corporation                         |232.10   |14.56     |6.69      |41,865,077.00 |7.70             |
|ORLY   |O'Reilly Automotive Inc.                   |249.80   |-2.39     |-0.95     |1,837,037.00  |-0.20            |
|PCAR   |PACCAR Inc.                                |67.50    |0.52      |0.78      |2,382,083.00  |0.20             |
|PAYX   |Paychex Inc.                               |64.20    |2.30      |3.72      |3,338,805.00  |0.70             |
|PYPL   |PayPal Holdings Inc.                       |74.80    |2.43      |3.36      |14,339,365.00 |2.60             |
|QCOM   |QUALCOMM Incorporated                      |64.00    |1.57      |2.52      |23,094,960.00 |2.00             |
|REGN   |Regeneron Pharmaceuticals Inc.             |336.40   |13.76     |4.27      |2,088,286.00  |1.30             |
|ROST   |Ross Stores Inc.                           |77.30    |2.26      |3.01      |3,907,830.00  |0.80             |
|STX    |Seagate Technology PLC                     |47.80    |0.38      |0.80      |5,633,152.00  |0.10             |
|SHPG   |Shire plc                                  |127.40   |1.20      |0.95      |2,068,157.00  |0.10             |
|SIRI   |Sirius XM Holdings Inc.                    |5.80     |-0.03     |-0.51     |33,656,437.00 |-0.10            |
|SWKS   |Skyworks Solutions Inc.                    |102.60   |3.60      |3.63      |3,294,917.00  |0.60             |
|SBUX   |Starbucks Corporation                      |54.60    |0.81      |1.51      |19,382,011.00 |1.00             |
|SYMC   |Symantec Corporation                       |25.90    |0.32      |1.25      |10,722,976.00 |0.20             |
|SNPS   |Synopsys Inc.                              |84.30    |1.72      |2.08      |1,718,714.00  |0.20             |
|TMUS   |T-Mobile US Inc.                           |59.20    |0.32      |0.54      |5,143,931.00  |0.20             |
|TTWO   |Take-Two Interactive Software Inc.         |108.50   |4.94      |4.77      |5,299,785.00  |0.50             |
|TSLA   |Tesla Inc.                                 |310.40   |-4.81     |-1.53     |12,930,726.00 |-0.70            |
|TXN    |Texas Instruments Incorporated             |100.50   |2.83      |2.90      |11,599,319.00 |2.40             |
|KHC    |The Kraft Heinz Company                    |71.40    |-0.20     |-0.28     |6,871,009.00  |-0.20            |
|PCLN   |The Priceline Group Inc.                   |1,765.00 |-41.06    |-2.27     |1,137,502.00  |-1.80            |
|FOX    |Twenty-First Century Fox Inc.              |35.20    |1.10      |3.23      |5,539,756.00  |0.80             |
|FOXA   |Twenty-First Century Fox Inc.              |35.70    |1.17      |3.39      |16,932,410.00 |1.10             |
|ULTA   |Ulta Beauty Inc.                           |218.60   |9.46      |4.52      |1,325,542.00  |0.50             |
|VRSK   |Verisk Analytics Inc.                      |92.90    |0.63      |0.68      |1,134,681.00  |0.10             |
|VRTX   |Vertex Pharmaceuticals Incorporated        |153.00   |1.36      |0.90      |2,719,763.00  |0.30             |
|VOD    |Vodafone Group Plc                         |28.50    |0.14      |0.49      |8,569,701.00  |0.10             |
|WBA    |Walgreens Boots Alliance Inc.              |68.50    |0.24      |0.35      |7,292,534.00  |0.20             |
|WDC    |Western Digital Corporation                |80.60    |0.50      |0.62      |4,628,793.00  |0.10             |
|WDAY   |Workday Inc.                               |113.00   |1.99      |1.79      |1,833,788.00  |0.20             |
|WYNN   |Wynn Resorts Limited                       |166.20   |-3.06     |-1.81     |5,511,089.00  |-0.30            |
|XLNX   |Xilinx Inc.                                |65.40    |2.56      |4.08      |5,380,579.00  |0.60             |
    




{% highlight r %}
#******************************************************************
# Extract components from S&P 500 Index
#****************************************************************** 
{% endhighlight %}

{% highlight r %}
qis.sp500.components("tickers")
qis.sp500.components("sectors")
{% endhighlight %}

{% highlight r %}
print(to.nice(qis.sp500.components("all")))
{% endhighlight %}



|Ticker symbol |Security                               |GICS Sector                |GICS Sub Industry                           |Date first added |
|:-------------|:--------------------------------------|:--------------------------|:-------------------------------------------|:----------------|
|MMM           |3M Company                             |Industrials                |Industrial Conglomerates                    |                 |
|ABT           |Abbott Laboratories                    |Health Care                |Health Care Equipment                       |1964-03-31       |
|ABBV          |AbbVie Inc.                            |Health Care                |Pharmaceuticals                             |2012-12-31       |
|ACN           |Accenture plc                          |Information Technology     |IT Consulting  Other Services               |2011-07-06       |
|ATVI          |Activision Blizzard                    |Information Technology     |Home Entertainment Software                 |2015-08-31       |
|AYI           |Acuity Brands Inc                      |Industrials                |Electrical Components  Equipment            |2016-05-03       |
|ADBE          |Adobe Systems Inc                      |Information Technology     |Application Software                        |1997-05-05       |
|AMD           |Advanced Micro Devices Inc             |Information Technology     |Semiconductors                              |2017-03-20       |
|AAP           |Advance Auto Parts                     |Consumer Discretionary     |Automotive Retail                           |2015-07-09       |
|AES           |AES Corp                               |Utilities                  |Independent Power Producers  Energy Traders |                 |
|AET           |Aetna Inc                              |Health Care                |Managed Health Care                         |1976-06-30       |
|AMG           |Affiliated Managers Group Inc          |Financials                 |Asset Management  Custody Banks             |2014-07-01       |
|AFL           |AFLAC Inc                              |Financials                 |Life  Health Insurance                      |                 |
|A             |Agilent Technologies Inc               |Health Care                |Health Care Equipment                       |2000-06-05       |
|APD           |Air Products  Chemicals Inc            |Materials                  |Industrial Gases                            |1985-04-30       |
|AKAM          |Akamai Technologies Inc                |Information Technology     |Internet Software  Services                 |2007-07-12       |
|ALK           |Alaska Air Group Inc                   |Industrials                |Airlines                                    |2016-05-13       |
|ALB           |Albemarle Corp                         |Materials                  |Specialty Chemicals                         |2016-07-01       |
|ARE           |Alexandria Real Estate Equities Inc    |Real Estate                |Office REITs                                |2017-03-20       |
|ALXN          |Alexion Pharmaceuticals                |Health Care                |Biotechnology                               |2012-05-25       |
|ALGN          |Align Technology                       |Health Care                |Health Care Supplies                        |2017-06-19       |
|ALLE          |Allegion                               |Industrials                |Building Products                           |2013-12-02       |
|AGN           |Allergan, Plc                          |Health Care                |Pharmaceuticals                             |                 |
|ADS           |Alliance Data Systems                  |Information Technology     |Data Processing  Outsourced Services        |2013-12-23       |
|LNT           |Alliant Energy Corp                    |Utilities                  |Electric Utilities                          |2016-07-01       |
|ALL           |Allstate Corp                          |Financials                 |Property  Casualty Insurance                |                 |
|GOOGL         |Alphabet Inc Class A                   |Information Technology     |Internet Software  Services                 |2014-04-03       |
|GOOG          |Alphabet Inc Class C                   |Information Technology     |Internet Software  Services                 |                 |
|MO            |Altria Group Inc                       |Consumer Staples           |Tobacco                                     |                 |
|AMZN          |Amazon.com Inc                         |Consumer Discretionary     |Internet  Direct Marketing Retail           |2005-11-18       |
|AEE           |Ameren Corp                            |Utilities                  |Multi-Utilities                             |1991-09-19       |
|AAL           |American Airlines Group                |Industrials                |Airlines                                    |2015-03-23       |
|AEP           |American Electric Power                |Utilities                  |Electric Utilities                          |                 |
|AXP           |American Express Co                    |Financials                 |Consumer Finance                            |1976-06-30       |
|AIG           |American International Group, Inc.     |Financials                 |Property  Casualty Insurance                |1980-03-31       |
|AMT           |American Tower Corp A                  |Real Estate                |Specialized REITs                           |2007-11-19       |
|AWK           |American Water Works Company Inc       |Utilities                  |Water Utilities                             |2016-03-04       |
|AMP           |Ameriprise Financial                   |Financials                 |Asset Management  Custody Banks             |2005-10-03       |
|ABC           |AmerisourceBergen Corp                 |Health Care                |Health Care Distributors                    |2001-08-30       |
|AME           |AMETEK Inc                             |Industrials                |Electrical Components  Equipment            |2013-09-23       |
|AMGN          |Amgen Inc                              |Health Care                |Biotechnology                               |1992-01-02       |
|APH           |Amphenol Corp                          |Information Technology     |Electronic Components                       |2008-09-30       |
|APC           |Anadarko Petroleum Corp                |Energy                     |Oil  Gas Exploration  Production            |                 |
|ADI           |Analog Devices, Inc.                   |Information Technology     |Semiconductors                              |                 |
|ANDV          |Andeavor                               |Energy                     |Oil  Gas Refining  Marketing                |2007-09-27       |
|ANSS          |ANSYS                                  |Information Technology     |Application Software                        |2017-06-19       |
|ANTM          |Anthem Inc.                            |Health Care                |Managed Health Care                         |                 |
|AON           |Aon plc                                |Financials                 |Insurance Brokers                           |                 |
|AOS           |A.O. Smith Corp                        |Industrials                |Building Products                           |2017-07-26       |
|APA           |Apache Corporation                     |Energy                     |Oil  Gas Exploration  Production            |                 |
|AIV           |Apartment Investment  Management       |Real Estate                |Residential REITs                           |                 |
|AAPL          |Apple Inc.                             |Information Technology     |Technology Hardware, Storage  Peripherals   |1982-11-30       |
|AMAT          |Applied Materials Inc                  |Information Technology     |Semiconductor Equipment                     |                 |
|APTV          |Aptiv Plc                              |Consumer Discretionary     |Auto Parts  Equipment                       |2012-12-24       |
|ADM           |Archer-Daniels-Midland Co              |Consumer Staples           |Agricultural Products                       |1981-07-29       |
|ARNC          |Arconic Inc                            |Industrials                |Aerospace  Defense                          |1964-03-31       |
|AJG           |Arthur J. Gallagher  Co.               |Financials                 |Insurance Brokers                           |2016-05-31       |
|AIZ           |Assurant Inc                           |Financials                 |Multi-line Insurance                        |2007-04-10       |
|T             |ATT Inc                                |Telecommunication Services |Integrated Telecommunication Services       |1983-11-30       |
|ADSK          |Autodesk Inc                           |Information Technology     |Application Software                        |1989-12-01       |
|ADP           |Automatic Data Processing              |Information Technology     |Internet Software  Services                 |1981-03-31       |
|AZO           |AutoZone Inc                           |Consumer Discretionary     |Specialty Stores                            |                 |
|AVB           |AvalonBay Communities, Inc.            |Real Estate                |Residential REITs                           |2007-01-10       |
|AVY           |Avery Dennison Corp                    |Materials                  |Paper Packaging                             |1987-12-31       |
|BHGE          |Baker Hughes, a GE Company             |Energy                     |Oil  Gas Equipment  Services                |                 |
|BLL           |Ball Corp                              |Materials                  |Metal  Glass Containers                     |1984-10-31       |
|BAC           |Bank of America Corp                   |Financials                 |Diversified Banks                           |1976-06-30       |
|BK            |The Bank of New York Mellon Corp.      |Financials                 |Asset Management  Custody Banks             |                 |
|BAX           |Baxter International Inc.              |Health Care                |Health Care Equipment                       |1972-09-30       |
|BBT           |BBT Corporation                        |Financials                 |Regional Banks                              |                 |
|BDX           |Becton Dickinson                       |Health Care                |Health Care Equipment                       |1972-09-30       |
|BRK.B         |Berkshire Hathaway                     |Financials                 |Multi-Sector Holdings                       |2010-02-16       |
|BBY           |Best Buy Co. Inc.                      |Consumer Discretionary     |Computer  Electronics Retail                |1999-06-29       |
|BIIB          |Biogen Inc.                            |Health Care                |Biotechnology                               |                 |
|BLK           |BlackRock                              |Financials                 |Asset Management  Custody Banks             |2011-04-04       |
|HRB           |Block HR                               |Financials                 |Consumer Finance                            |1986-11-30       |
|BA            |Boeing Company                         |Industrials                |Aerospace  Defense                          |                 |
|BWA           |BorgWarner                             |Consumer Discretionary     |Auto Parts  Equipment                       |2011-12-19       |
|BXP           |Boston Properties                      |Real Estate                |Office REITs                                |2006-04-03       |
|BSX           |Boston Scientific                      |Health Care                |Health Care Equipment                       |1995-02-24       |
|BHF           |Brighthouse Financial Inc              |Financials                 |Life  Health Insurance                      |2017-08-08       |
|BMY           |Bristol-Myers Squibb                   |Health Care                |Health Care Distributors                    |                 |
|AVGO          |Broadcom                               |Information Technology     |Semiconductors                              |2014-05-08       |
|BF.B          |Brown-Forman Corp.                     |Consumer Staples           |Distillers  Vintners                        |1982-10-31       |
|CHRW          |C. H. Robinson Worldwide               |Industrials                |Air Freight  Logistics                      |2007-03-02       |
|CA            |CA, Inc.                               |Information Technology     |Systems Software                            |1987-07-31       |
|COG           |Cabot Oil  Gas                         |Energy                     |Oil  Gas Exploration  Production            |2008-06-23       |
|CDNS          |Cadence Design Systems                 |Information Technology     |Application Software                        |2017-09-18       |
|CPB           |Campbell Soup                          |Consumer Staples           |Packaged Foods  Meats                       |                 |
|COF           |Capital One Financial                  |Financials                 |Consumer Finance                            |                 |
|CAH           |Cardinal Health Inc.                   |Health Care                |Health Care Distributors                    |                 |
|CBOE          |CBOE Holdings                          |Financials                 |Financial Exchanges  Data                   |2017-03-01       |
|KMX           |Carmax Inc                             |Consumer Discretionary     |Specialty Stores                            |2010-06-28       |
|CCL           |Carnival Corp.                         |Consumer Discretionary     |Hotels, Resorts  Cruise Lines               |                 |
|CAT           |Caterpillar Inc.                       |Industrials                |Construction Machinery  Heavy Trucks        |                 |
|CBG           |CBRE Group                             |Real Estate                |Real Estate Services                        |2006-11-10       |
|CBS           |CBS Corp.                              |Consumer Discretionary     |Broadcasting                                |1994-09-01       |
|CELG          |Celgene Corp.                          |Health Care                |Biotechnology                               |2006-11-06       |
|CNC           |Centene Corporation                    |Health Care                |Managed Health Care                         |2016-03-30       |
|CNP           |CenterPoint Energy                     |Utilities                  |Multi-Utilities                             |1985-07-31       |
|CTL           |CenturyLink Inc                        |Telecommunication Services |Integrated Telecommunication Services       |                 |
|CERN          |Cerner                                 |Health Care                |Health Care Technology                      |2010-04-30       |
|CF            |CF Industries Holdings Inc             |Materials                  |Fertilizers  Agricultural Chemicals         |2008-08-27       |
|SCHW          |Charles Schwab Corporation             |Financials                 |Investment Banking  Brokerage               |                 |
|CHTR          |Charter Communications                 |Consumer Discretionary     |Cable  Satellite                            |2016-09-08       |
|CHK           |Chesapeake Energy                      |Energy                     |Oil  Gas Exploration  Production            |                 |
|CVX           |Chevron Corp.                          |Energy                     |Integrated Oil  Gas                         |                 |
|CMG           |Chipotle Mexican Grill                 |Consumer Discretionary     |Restaurants                                 |2011-04-28       |
|CB            |Chubb Limited                          |Financials                 |Property  Casualty Insurance                |2010-07-15       |
|CHD           |Church  Dwight                         |Consumer Staples           |Household Products                          |2015-12-29       |
|CI            |CIG   Corp.                            |Health Care                |Managed Health Care                         |1976-06-30       |
|XEC           |Cimarex Energy                         |Energy                     |Oil  Gas Exploration  Production            |2014-06-21       |
|CINF          |Cincinnati Financial                   |Financials                 |Property  Casualty Insurance                |                 |
|CTAS          |Cintas Corporation                     |Industrials                |Diversified Support Services                |2001-03-01       |
|CSCO          |Cisco Systems                          |Information Technology     |Communications Equipment                    |1993-12-01       |
|C             |Citigroup Inc.                         |Financials                 |Diversified Banks                           |1988-05-31       |
|CFG           |Citizens Financial Group               |Financials                 |Regional Banks                              |2016-01-29       |
|CTXS          |Citrix Systems                         |Information Technology     |Internet Software  Services                 |                 |
|CLX           |The Clorox Company                     |Consumer Staples           |Household Products                          |1969-03-31       |
|CME           |CME Group Inc.                         |Financials                 |Financial Exchanges  Data                   |                 |
|CMS           |CMS Energy                             |Utilities                  |Multi-Utilities                             |                 |
|KO            |Coca-Cola Company (The)                |Consumer Staples           |Soft Drinks                                 |                 |
|CTSH          |Cognizant Technology Solutions         |Information Technology     |IT Consulting  Other Services               |2006-11-17       |
|CL            |Colgate-Palmolive                      |Consumer Staples           |Household Products                          |                 |
|CMCSA         |Comcast Corp.                          |Consumer Discretionary     |Cable  Satellite                            |2015-09-18       |
|CMA           |Comerica Inc.                          |Financials                 |Diversified Banks                           |                 |
|CAG           |Conagra Brands                         |Consumer Staples           |Packaged Foods  Meats                       |1983-08-31       |
|CXO           |Concho Resources                       |Energy                     |Oil  Gas Exploration  Production            |2016-02-22       |
|COP           |ConocoPhillips                         |Energy                     |Oil  Gas Exploration  Production            |                 |
|ED            |Consolidated Edison                    |Utilities                  |Electric Utilities                          |                 |
|STZ           |Constellation Brands                   |Consumer Staples           |Distillers  Vintners                        |2005-07-05       |
|COO           |The Cooper Companies                   |Health Care                |Health Care Supplies                        |2016-09-23       |
|GLW           |Corning Inc.                           |Information Technology     |Electronic Components                       |                 |
|COST          |Costco Wholesale Corp.                 |Consumer Staples           |Hypermarkets  Super Centers                 |1993-10-01       |
|COTY          |Coty, Inc                              |Consumer Staples           |Personal Products                           |2016-10-03       |
|CCI           |Crown Castle International Corp.       |Real Estate                |Specialized REITs                           |2012-03-14       |
|CSRA          |CSRA Inc.                              |Information Technology     |IT Consulting  Other Services               |2015-12-01       |
|CSX           |CSX Corp.                              |Industrials                |Railroads                                   |1967-09-30       |
|CMI           |Cummins Inc.                           |Industrials                |Industrial Machinery                        |1965-03-31       |
|CVS           |CVS Health                             |Consumer Staples           |Drug Retail                                 |                 |
|DHI           |D. R. Horton                           |Consumer Discretionary     |Homebuilding                                |                 |
|DHR           |Danaher Corp.                          |Health Care                |Health Care Equipment                       |                 |
|DRI           |Darden Restaurants                     |Consumer Discretionary     |Restaurants                                 |                 |
|DVA           |DaVita Inc.                            |Health Care                |Health Care Facilities                      |2008-07-31       |
|DE            |Deere  Co.                             |Industrials                |Agricultural  Farm Machinery                |                 |
|DAL           |Delta Air Lines Inc.                   |Industrials                |Airlines                                    |2013-09-11       |
|XRAY          |Dentsply Sirona                        |Health Care                |Health Care Supplies                        |2008-11-14       |
|DVN           |Devon Energy Corp.                     |Energy                     |Oil  Gas Exploration  Production            |2000-08-30       |
|DLR           |Digital Realty Trust Inc               |Real Estate                |Specialized REITs                           |2016-05-18       |
|DFS           |Discover Financial Services            |Financials                 |Consumer Finance                            |2007-07-02       |
|DISCA         |Discovery Communications-A             |Consumer Discretionary     |Cable  Satellite                            |2010-03-01       |
|DISCK         |Discovery Communications-C             |Consumer Discretionary     |Cable  Satellite                            |2014-08-07       |
|DISH          |Dish Network                           |Consumer Discretionary     |Cable  Satellite                            |2017-03-13       |
|DG            |Dollar General                         |Consumer Discretionary     |General Merchandise Stores                  |2012-12-03       |
|DLTR          |Dollar Tree                            |Consumer Discretionary     |General Merchandise Stores                  |2011-12-19       |
|D             |Dominion Energy                        |Utilities                  |Electric Utilities                          |                 |
|DOV           |Dover Corp.                            |Industrials                |Industrial Machinery                        |1985-10-31       |
|DWDP          |DowDuPont                              |Materials                  |Diversified Chemicals                       |                 |
|DPS           |Dr Pepper Snapple Group                |Consumer Staples           |Soft Drinks                                 |2008-10-07       |
|DTE           |DTE Energy Co.                         |Utilities                  |Multi-Utilities                             |                 |
|DRE           |Duke Realty Corp                       |Real Estate                |Industrial REITs                            |2017-07-26       |
|DUK           |Duke Energy                            |Utilities                  |Electric Utilities                          |1976-06-30       |
|DXC           |DXC Technology                         |Information Technology     |IT Consulting  Other Services               |2017-04-04       |
|ETFC          |E*Trade                                |Financials                 |Investment Banking  Brokerage               |2004-03-31       |
|EMN           |Eastman Chemical                       |Materials                  |Diversified Chemicals                       |1994-01-01       |
|ETN           |Eaton Corporation                      |Industrials                |Electrical Components  Equipment            |                 |
|EBAY          |eBay Inc.                              |Information Technology     |Internet Software  Services                 |                 |
|ECL           |Ecolab Inc.                            |Materials                  |Specialty Chemicals                         |1989-01-31       |
|EIX           |Edison Int'l                           |Utilities                  |Electric Utilities                          |                 |
|EW            |Edwards Lifesciences                   |Health Care                |Health Care Equipment                       |2011-04-01       |
|EA            |Electronic Arts                        |Information Technology     |Home Entertainment Software                 |2002-07-22       |
|EMR           |Emerson Electric Company               |Industrials                |Electrical Components  Equipment            |1965-03-31       |
|ETR           |Entergy Corp.                          |Utilities                  |Electric Utilities                          |                 |
|EVHC          |Envision Healthcare                    |Health Care                |Health Care Services                        |2016-12-02       |
|EOG           |EOG Resources                          |Energy                     |Oil  Gas Exploration  Production            |2000-11-02       |
|EQT           |EQT Corporation                        |Energy                     |Oil  Gas Exploration  Production            |2008-12-19       |
|EFX           |Equifax Inc.                           |Industrials                |Research  Consulting Services               |1997-06-19       |
|EQIX          |Equinix                                |Real Estate                |Specialized REITs                           |2015-03-20       |
|EQR           |Equity Residential                     |Real Estate                |Residential REITs                           |2001-12-03       |
|ESS           |Essex Property Trust, Inc.             |Real Estate                |Residential REITs                           |2014-04-02       |
|EL            |Estee Lauder Cos.                      |Consumer Staples           |Personal Products                           |2006-01-05       |
|ES            |Eversource Energy                      |Utilities                  |Multi-Utilities                             |                 |
|RE            |Everest Re Group Ltd.                  |Financials                 |Reinsurance                                 |2017-06-19       |
|EXC           |Exelon Corp.                           |Utilities                  |Multi-Utilities                             |                 |
|EXPE          |Expedia Inc.                           |Consumer Discretionary     |Internet  Direct Marketing Retail           |2007-10-02       |
|EXPD          |Expeditors International               |Industrials                |Air Freight  Logistics                      |2007-10-10       |
|ESRX          |Express Scripts                        |Health Care                |Health Care Distributors                    |2003-09-25       |
|EXR           |Extra Space Storage                    |Real Estate                |Specialized REITs                           |2016-01-19       |
|XOM           |Exxon Mobil Corp.                      |Energy                     |Integrated Oil  Gas                         |                 |
|FFIV          |F5 Networks                            |Information Technology     |Communications Equipment                    |2010-12-20       |
|FB            |Facebook, Inc.                         |Information Technology     |Internet Software  Services                 |2013-12-23       |
|FAST          |Fastenal Co                            |Industrials                |Building Products                           |2009-09-15       |
|FRT           |Federal Realty Investment Trust        |Real Estate                |Retail REITs                                |2016-02-01       |
|FDX           |FedEx Corporation                      |Industrials                |Air Freight  Logistics                      |1980-12-31       |
|FIS           |Fidelity National Information Services |Information Technology     |Internet Software  Services                 |2006-11-10       |
|FITB          |Fifth Third Bancorp                    |Financials                 |Regional Banks                              |                 |
|FE            |FirstEnergy Corp                       |Utilities                  |Electric Utilities                          |                 |
|FISV          |Fiserv Inc                             |Information Technology     |Internet Software  Services                 |2001-04-02       |
|FLIR          |FLIR Systems                           |Information Technology     |Electronic Equipment  Instruments           |2009-01-02       |
|FLS           |Flowserve Corporation                  |Industrials                |Industrial Machinery                        |2008-10-02       |
|FLR           |Fluor Corp.                            |Industrials                |Construction  Engineering                   |1980-03-31       |
|FMC           |FMC Corporation                        |Materials                  |Fertilizers  Agricultural Chemicals         |2009-08-19       |
|FL            |Foot Locker Inc                        |Consumer Discretionary     |Apparel Retail                              |2016-04-04       |
|F             |Ford Motor                             |Consumer Discretionary     |Automobile Manufacturers                    |                 |
|FTV           |Fortive Corp                           |Industrials                |Industrial Machinery                        |2016-07-01       |
|FBHS          |Fortune Brands Home  Security          |Industrials                |Building Products                           |2016-06-22       |
|BEN           |Franklin Resources                     |Financials                 |Asset Management  Custody Banks             |                 |
|FCX           |Freeport-McMoRan Inc.                  |Materials                  |Copper                                      |                 |
|GPS           |Gap Inc.                               |Consumer Discretionary     |Apparel Retail                              |1986-08-31       |
|GRMN          |Garmin Ltd.                            |Consumer Discretionary     |Consumer Electronics                        |2012-12-12       |
|IT            |Gartner Inc                            |Information Technology     |IT Consulting  Other Services               |2017-04-05       |
|GD            |General Dynamics                       |Industrials                |Aerospace  Defense                          |                 |
|GE            |General Electric                       |Industrials                |Industrial Conglomerates                    |                 |
|GGP           |General Growth Properties Inc.         |Real Estate                |Retail REITs                                |2013-12-10       |
|GIS           |General Mills                          |Consumer Staples           |Packaged Foods  Meats                       |1969-03-31       |
|GM            |General Motors                         |Consumer Discretionary     |Automobile Manufacturers                    |2013-06-06       |
|GPC           |Genuine Parts                          |Consumer Discretionary     |Specialty Stores                            |1973-12-31       |
|GILD          |Gilead Sciences                        |Health Care                |Biotechnology                               |2004-07-01       |
|GPN           |Global Payments Inc.                   |Information Technology     |Data Processing  Outsourced Services        |2016-04-25       |
|GS            |Goldman Sachs Group                    |Financials                 |Investment Banking  Brokerage               |2002-07-22       |
|GT            |Goodyear Tire  Rubber                  |Consumer Discretionary     |Tires  Rubber                               |                 |
|GWW           |Grainger (W.W.) Inc.                   |Industrials                |Industrial Machinery                        |1981-06-30       |
|HAL           |Halliburton Co.                        |Energy                     |Oil  Gas Equipment  Services                |                 |
|HBI           |Hanesbrands Inc                        |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2015-03-20       |
|HOG           |Harley-Davidson                        |Consumer Discretionary     |Motorcycle Manufacturers                    |                 |
|HRS           |Harris Corporation                     |Information Technology     |Communications Equipment                    |2008-09-22       |
|HIG           |Hartford Financial Svc.Gp.             |Financials                 |Property  Casualty Insurance                |                 |
|HAS           |Hasbro Inc.                            |Consumer Discretionary     |Leisure Products                            |1984-09-30       |
|HCA           |HCA Holdings                           |Health Care                |Health Care Facilities                      |2015-01-27       |
|HCP           |HCP Inc.                               |Real Estate                |Health Care REITs                           |2008-03-31       |
|HP            |Helmerich  Payne                       |Energy                     |Oil  Gas Drilling                           |2010-03-01       |
|HSIC          |Henry Schein                           |Health Care                |Health Care Distributors                    |2015-03-17       |
|HSY           |The Hershey Company                    |Consumer Staples           |Packaged Foods  Meats                       |                 |
|HES           |Hess Corporation                       |Energy                     |Integrated Oil  Gas                         |1984-05-31       |
|HPE           |Hewlett Packard Enterprise             |Information Technology     |Technology Hardware, Storage  Peripherals   |2015-11-02       |
|HLT           |Hilton Worldwide Holdings Inc          |Consumer Discretionary     |Hotels, Resorts  Cruise Lines               |2017-06-19       |
|HOLX          |Hologic                                |Health Care                |Health Care Equipment                       |2016-03-30       |
|HD            |Home Depot                             |Consumer Discretionary     |Home Improvement Retail                     |1988-03-31       |
|HON           |Honeywell Int'l Inc.                   |Industrials                |Industrial Conglomerates                    |1964-03-31       |
|HRL           |Hormel Foods Corp.                     |Consumer Staples           |Packaged Foods  Meats                       |2009-03-04       |
|HST           |Host Hotels  Resorts                   |Real Estate                |Hotel  Resort REITs                         |2007-03-20       |
|HPQ           |HP Inc.                                |Information Technology     |Technology Hardware, Storage  Peripherals   |1974-12-31       |
|HUM           |Humana Inc.                            |Health Care                |Managed Health Care                         |                 |
|HBAN          |Huntington Bancshares                  |Financials                 |Regional Banks                              |                 |
|HII           |Huntington Ingalls Industries          |Industrials                |Aerospace  Defense                          |2018-01-03       |
|IDXX          |IDEXX Laboratories                     |Health Care                |Health Care Equipment                       |2017-01-05       |
|INFO          |IHS Markit Ltd.                        |Industrials                |Research  Consulting Services               |2017-06-02       |
|ITW           |Illinois Tool Works                    |Industrials                |Industrial Machinery                        |1986-02-28       |
|ILMN          |Illumina Inc                           |Health Care                |Life Sciences Tools  Services               |2015-11-19       |
|IR            |Ingersoll-Rand PLC                     |Industrials                |Industrial Machinery                        |2010-11-17       |
|INTC          |Intel Corp.                            |Information Technology     |Semiconductors                              |1976-12-31       |
|ICE           |Intercontinental Exchange              |Financials                 |Financial Exchanges  Data                   |2007-09-26       |
|IBM           |International Business Machines        |Information Technology     |IT Consulting  Other Services               |                 |
|INCY          |Incyte                                 |Health Care                |Biotechnology                               |2017-02-28       |
|IP            |International Paper                    |Materials                  |Paper Packaging                             |                 |
|IPG           |Interpublic Group                      |Consumer Discretionary     |Advertising                                 |1992-10-01       |
|IFF           |Intl Flavors  Fragrances               |Materials                  |Specialty Chemicals                         |1976-03-31       |
|INTU          |Intuit Inc.                            |Information Technology     |Internet Software  Services                 |2000-12-05       |
|ISRG          |Intuitive Surgical Inc.                |Health Care                |Health Care Equipment                       |2008-06-02       |
|IVZ           |Invesco Ltd.                           |Financials                 |Asset Management  Custody Banks             |2008-08-21       |
|IQV           |IQVIA Holdings Inc.                    |Health Care                |Life Sciences Tools  Service                |2017-08-29       |
|IRM           |Iron Mountain Incorporated             |Real Estate                |Specialized REITs                           |2009-01-06       |
|JEC           |Jacobs Engineering Group               |Industrials                |Construction  Engineering                   |2007-10-26       |
|JBHT          |J. B. Hunt Transport Services          |Industrials                |Trucking                                    |2015-07-01       |
|SJM           |JM Smucker                             |Consumer Staples           |Packaged Foods  Meats                       |2008-11-06       |
|JNJ           |Johnson  Johnson                       |Health Care                |Health Care Equipment                       |1973-06-30       |
|JCI           |Johnson Controls International         |Industrials                |Building Products                           |2010-08-27       |
|JPM           |JPMorgan Chase  Co.                    |Financials                 |Diversified Banks                           |1975-06-30       |
|JNPR          |Juniper Networks                       |Information Technology     |Communications Equipment                    |                 |
|KSU           |Kansas City Southern                   |Industrials                |Railroads                                   |2013-05-24       |
|K             |Kellogg Co.                            |Consumer Staples           |Packaged Foods  Meats                       |                 |
|KEY           |KeyCorp                                |Financials                 |Regional Banks                              |1994-03-01       |
|KMB           |Kimberly-Clark                         |Consumer Staples           |Household Products                          |                 |
|KIM           |Kimco Realty                           |Real Estate                |Retail REITs                                |2006-04-04       |
|KMI           |Kinder Morgan                          |Energy                     |Oil  Gas Storage  Transportation            |2012-05-25       |
|KLAC          |KLA-Tencor Corp.                       |Information Technology     |Semiconductor Equipment                     |                 |
|KSS           |Kohl's Corp.                           |Consumer Discretionary     |General Merchandise Stores                  |                 |
|KHC           |Kraft Heinz Co                         |Consumer Staples           |Packaged Foods  Meats                       |2015-07-06       |
|KR            |Kroger Co.                             |Consumer Staples           |Food Retail                                 |                 |
|LB            |L Brands Inc.                          |Consumer Discretionary     |Apparel Retail                              |1983-09-30       |
|LLL           |L-3 Communications Holdings            |Industrials                |Aerospace  Defense                          |                 |
|LH            |Laboratory Corp. of America Holding    |Health Care                |Health Care Services                        |2004-11-01       |
|LRCX          |Lam Research                           |Information Technology     |Semiconductor Equipment                     |2012-06-29       |
|LEG           |Leggett  Platt                         |Consumer Discretionary     |Home Furnishings                            |                 |
|LEN           |Lennar Corp.                           |Consumer Discretionary     |Homebuilding                                |2005-10-04       |
|LUK           |Leucadia National Corp.                |Financials                 |Multi-Sector Holdings                       |2007-08-27       |
|LLY           |Lilly (Eli)  Co.                       |Health Care                |Pharmaceuticals                             |1970-12-31       |
|LNC           |Lincoln National                       |Financials                 |Multi-line Insurance                        |1976-06-30       |
|LKQ           |LKQ Corporation                        |Consumer Discretionary     |Distributors                                |2016-05-23       |
|LMT           |Lockheed Martin Corp.                  |Industrials                |Aerospace  Defense                          |1984-07-31       |
|L             |Loews Corp.                            |Financials                 |Multi-line Insurance                        |                 |
|LOW           |Lowe's Cos.                            |Consumer Discretionary     |Home Improvement Retail                     |1984-02-29       |
|LYB           |LyondellBasell                         |Materials                  |Specialty Chemicals                         |2012-09-05       |
|MTB           |MT Bank Corp.                          |Financials                 |Regional Banks                              |                 |
|MAC           |Macerich                               |Real Estate                |Retail REITs                                |2013-05-09       |
|M             |Macy's Inc.                            |Consumer Discretionary     |Department Stores                           |                 |
|MRO           |Marathon Oil Corp.                     |Energy                     |Oil  Gas Exploration  Production            |1991-05-01       |
|MPC           |Marathon Petroleum                     |Energy                     |Oil  Gas Refining  Marketing                |2011-07-01       |
|MAR           |Marriott Int'l.                        |Consumer Discretionary     |Hotels, Resorts  Cruise Lines               |                 |
|MMC           |Marsh  McLennan                        |Financials                 |Insurance Brokers                           |1987-08-31       |
|MLM           |Martin Marietta Materials              |Materials                  |Construction Materials                      |2014-07-02       |
|MAS           |Masco Corp.                            |Industrials                |Building Products                           |1981-06-30       |
|MA            |Mastercard Inc.                        |Information Technology     |Internet Software  Services                 |2008-07-18       |
|MAT           |Mattel Inc.                            |Consumer Discretionary     |Leisure Products                            |1982-03-31       |
|MKC           |McCormick  Co.                         |Consumer Staples           |Packaged Foods  Meats                       |                 |
|MCD           |McDonald's Corp.                       |Consumer Discretionary     |Restaurants                                 |1970-06-30       |
|MCK           |McKesson Corp.                         |Health Care                |Health Care Distributors                    |                 |
|MDT           |Medtronic plc                          |Health Care                |Health Care Equipment                       |1986-10-31       |
|MRK           |Merck  Co.                             |Health Care                |Pharmaceuticals                             |                 |
|MET           |MetLife Inc.                           |Financials                 |Life  Health Insurance                      |                 |
|MTD           |Mettler Toledo                         |Health Care                |Life Sciences Tools  Services               |2016-09-06       |
|MGM           |MGM Resorts International              |Consumer Discretionary     |Casinos  Gaming                             |2017-07-26       |
|KORS          |Michael Kors Holdings                  |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2013-11-13       |
|MCHP          |Microchip Technology                   |Information Technology     |Semiconductors                              |2007-09-07       |
|MU            |Micron Technology                      |Information Technology     |Semiconductors                              |1994-09-27       |
|MSFT          |Microsoft Corp.                        |Information Technology     |Systems Software                            |1994-06-01       |
|MAA           |Mid-America Apartments                 |Real Estate                |Residential REITs                           |2016-12-02       |
|MHK           |Mohawk Industries                      |Consumer Discretionary     |Home Furnishings                            |2013-12-23       |
|TAP           |Molson Coors Brewing Company           |Consumer Staples           |Brewers                                     |1976-06-30       |
|MDLZ          |Mondelez International                 |Consumer Staples           |Packaged Foods  Meats                       |2012-10-02       |
|MON           |Monsanto Co.                           |Materials                  |Fertilizers  Agricultural Chemicals         |2002-08-07       |
|MNST          |Monster Beverage                       |Consumer Staples           |Soft Drinks                                 |2012-06-28       |
|MCO           |Moody's Corp                           |Financials                 |Financial Exchanges  Data                   |                 |
|MS            |Morgan Stanley                         |Financials                 |Investment Banking  Brokerage               |                 |
|MOS           |The Mosaic Company                     |Materials                  |Fertilizers  Agricultural Chemicals         |2011-09-26       |
|MSI           |Motorola Solutions Inc.                |Information Technology     |Communications Equipment                    |                 |
|MYL           |Mylan N.V.                             |Health Care                |Pharmaceuticals                             |2004-04-23       |
|NDAQ          |Nasdaq, Inc.                           |Financials                 |Financial Exchanges  Data                   |2008-10-22       |
|NOV           |National Oilwell Varco Inc.            |Energy                     |Oil  Gas Equipment  Services                |2005-03-14       |
|  VI          |Navient                                |Financials                 |Consumer Finance                            |2014-05-01       |
|NTAP          |NetApp                                 |Information Technology     |Internet Software  Services                 |1999-06-25       |
|NFLX          |Netflix Inc.                           |Information Technology     |Internet Software  Services                 |2010-12-20       |
|NWL           |Newell Brands                          |Consumer Discretionary     |Housewares  Specialties                     |1989-04-30       |
|NFX           |Newfield Exploration Co                |Energy                     |Oil  Gas Exploration  Production            |2010-12-20       |
|NEM           |Newmont Mining Corporation             |Materials                  |Gold                                        |1969-06-30       |
|NWSA          |News Corp. Class A                     |Consumer Discretionary     |Publishing                                  |2013-08-01       |
|NWS           |News Corp. Class B                     |Consumer Discretionary     |Publishing                                  |2015-09-18       |
|NEE           |NextEra Energy                         |Utilities                  |Multi-Utilities                             |1976-06-30       |
|NLSN          |Nielsen Holdings                       |Industrials                |Research  Consulting Services               |2013-07-09       |
|NKE           |Nike                                   |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |1988-11-30       |
|NI            |NiSource Inc.                          |Utilities                  |Multi-Utilities                             |                 |
|NBL           |Noble Energy Inc                       |Energy                     |Oil  Gas Exploration  Production            |2007-10-08       |
|JWN           |Nordstrom                              |Consumer Discretionary     |Department Stores                           |1986-08-31       |
|NSC           |Norfolk Southern Corp.                 |Industrials                |Railroads                                   |                 |
|NTRS          |Northern Trust Corp.                   |Financials                 |Asset Management  Custody Banks             |                 |
|NOC           |Northrop Grumman Corp.                 |Industrials                |Aerospace  Defense                          |1985-06-30       |
|NCLH          |Norwegian Cruise Line                  |Consumer Discretionary     |Hotels, Resorts  Cruise Lines               |2017-10-13       |
|NRG           |NRG Energy                             |Utilities                  |Independent Power Producers  Energy Traders |2010-01-29       |
|NUE           |Nucor Corp.                            |Materials                  |Steel                                       |1985-04-30       |
|NVDA          |Nvidia Corporation                     |Information Technology     |Semiconductors                              |2001-11-30       |
|ORLY          |O'Reilly Automotive                    |Consumer Discretionary     |Specialty Stores                            |2009-03-27       |
|OXY           |Occidental Petroleum                   |Energy                     |Oil  Gas Exploration  Production            |1982-12-31       |
|OMC           |Omnicom Group                          |Consumer Discretionary     |Advertising                                 |                 |
|OKE           |ONEOK                                  |Energy                     |Oil  Gas Storage  Transportation            |2010-03-15       |
|ORCL          |Oracle Corp.                           |Information Technology     |Application Software                        |1989-08-31       |
|PCAR          |PACCAR Inc.                            |Industrials                |Construction Machinery  Heavy Trucks        |1980-12-31       |
|PKG           |Packaging Corporation of America       |Materials                  |Paper Packaging                             |2017-07-26       |
|PH            |Parker-Hannifin                        |Industrials                |Industrial Machinery                        |1985-11-30       |
|PDCO          |Patterson Companies                    |Health Care                |Health Care Supplies                        |2005-10-11       |
|PAYX          |Paychex Inc.                           |Information Technology     |Internet Software  Services                 |                 |
|PYPL          |PayPal                                 |Information Technology     |Data Processing  Outsourced Services        |2015-07-20       |
|PNR           |Pentair Ltd.                           |Industrials                |Industrial Machinery                        |2012-10-01       |
|PBCT          |People's United Financial              |Financials                 |Thrifts  Mortgage Finance                   |2008-11-13       |
|PEP           |PepsiCo Inc.                           |Consumer Staples           |Soft Drinks                                 |                 |
|PKI           |PerkinElmer                            |Health Care                |Health Care Equipment                       |1985-05-31       |
|PRGO          |Perrigo                                |Health Care                |Pharmaceuticals                             |2011-12-19       |
|PFE           |Pfizer Inc.                            |Health Care                |Pharmaceuticals                             |                 |
|PCG           |PGE Corp.                              |Utilities                  |Multi-Utilities                             |                 |
|PM            |Philip Morris International            |Consumer Staples           |Tobacco                                     |2008-03-31       |
|PSX           |Phillips 66                            |Energy                     |Oil  Gas Refining  Marketing                |2012-05-01       |
|PNW           |Pinnacle West Capital                  |Utilities                  |Multi-Utilities                             |                 |
|PXD           |Pioneer Natural Resources              |Energy                     |Oil  Gas Exploration  Production            |2008-09-24       |
|PNC           |PNC Financial Services                 |Financials                 |Regional Banks                              |1988-04-30       |
|RL            |Polo Ralph Lauren Corp.                |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2007-02-02       |
|PPG           |PPG Industries                         |Materials                  |Specialty Chemicals                         |                 |
|PPL           |PPL Corp.                              |Utilities                  |Electric Utilities                          |                 |
|PX            |Praxair Inc.                           |Materials                  |Industrial Gases                            |1992-07-01       |
|PCLN          |Priceline.com Inc                      |Consumer Discretionary     |Internet  Direct Marketing Retail           |2009-11-06       |
|PFG           |Principal Financial Group              |Financials                 |Life  Health Insurance                      |2002-07-22       |
|PG            |Procter  Gamble                        |Consumer Staples           |Personal Products                           |                 |
|PGR           |Progressive Corp.                      |Financials                 |Property  Casualty Insurance                |1997-08-04       |
|PLD           |Prologis                               |Real Estate                |Industrial REITs                            |2003-07-17       |
|PRU           |Prudential Financial                   |Financials                 |Life  Health Insurance                      |2002-07-22       |
|PEG           |Public Serv. Enterprise Inc.           |Utilities                  |Electric Utilities                          |                 |
|PSA           |Public Storage                         |Real Estate                |Specialized REITs                           |2005-08-19       |
|PHM           |Pulte Homes Inc.                       |Consumer Discretionary     |Homebuilding                                |1984-04-30       |
|PVH           |PVH Corp.                              |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2013-02-15       |
|QRVO          |Qorvo                                  |Information Technology     |Semiconductors                              |2015-06-11       |
|PWR           |Quanta Services Inc.                   |Industrials                |Construction  Engineering                   |2009-07-01       |
|QCOM          |QUALCOMM Inc.                          |Information Technology     |Semiconductors                              |                 |
|DGX           |Quest Diagnostics                      |Health Care                |Health Care Services                        |2002-12-12       |
|RRC           |Range Resources Corp.                  |Energy                     |Oil  Gas Exploration  Production            |2007-12-21       |
|RJF           |Raymond James Financial Inc.           |Financials                 |Investment Banking  Brokerage               |2017-03-20       |
|RTN           |Raytheon Co.                           |Industrials                |Aerospace  Defense                          |                 |
|O             |Realty Income Corporation              |Real Estate                |Retail REITs                                |2015-04-07       |
|RHT           |Red Hat Inc.                           |Information Technology     |Systems Software                            |2009-07-27       |
|REG           |Regency Centers Corporation            |Real Estate                |Retail REITs                                |2017-03-02       |
|REGN          |Regeneron                              |Health Care                |Biotechnology                               |2013-05-01       |
|RF            |Regions Financial Corp.                |Financials                 |Regional Banks                              |1998-08-28       |
|RSG           |Republic Services Inc                  |Industrials                |Environmental  Facilities Services          |2008-12-05       |
|RMD           |ResMed                                 |Health Care                |Health Care Equipment                       |2017-07-26       |
|RHI           |Robert Half International              |Industrials                |Human Resource  Employment Services         |2000-12-05       |
|ROK           |Rockwell Automation Inc.               |Industrials                |Electrical Components  Equipment            |                 |
|COL           |Rockwell Collins                       |Industrials                |Aerospace  Defense                          |2001-07-02       |
|ROP           |Roper Technologies                     |Industrials                |Industrial Conglomerates                    |2009-12-23       |
|ROST          |Ross Stores                            |Consumer Discretionary     |Apparel Retail                              |2009-12-21       |
|RCL           |Royal Caribbean Cruises Ltd            |Consumer Discretionary     |Hotels, Resorts  Cruise Lines               |2014-12-05       |
|CRM           |Salesforce.com                         |Information Technology     |Internet Software  Services                 |2008-09-15       |
|SBAC          |SBA Communications                     |Real Estate                |Specialized REITs                           |2017-09-01       |
|SCG           |SCA   Corp                             |Utilities                  |Multi-Utilities                             |2009-01-02       |
|SLB           |Schlumberger Ltd.                      |Energy                     |Oil  Gas Equipment  Services                |1965-03-31       |
|SNI           |Scripps Networks Interactive Inc.      |Consumer Discretionary     |Cable  Satellite                            |2008-07-01       |
|STX           |Seagate Technology                     |Information Technology     |Technology Hardware, Storage  Peripherals   |2012-07-02       |
|SEE           |Sealed Air                             |Materials                  |Paper Packaging                             |                 |
|SRE           |Sempra Energy                          |Utilities                  |Multi-Utilities                             |                 |
|SHW           |Sherwin-Williams                       |Materials                  |Specialty Chemicals                         |1964-06-30       |
|SIG           |Signet Jewelers                        |Consumer Discretionary     |Specialty Stores                            |2015-07-29       |
|SPG           |Simon Property Group Inc               |Real Estate                |Retail REITs                                |2002-06-26       |
|SWKS          |Skyworks Solutions                     |Information Technology     |Semiconductors                              |2015-03-12       |
|SLG           |SL Green Realty                        |Real Estate                |Office REITs                                |2015-03-20       |
|S             |Snap-On Inc.                           |Consumer Discretionary     |Household Appliances                        |1982-09-30       |
|SO            |Southern Co.                           |Utilities                  |Electric Utilities                          |                 |
|LUV           |Southwest Airlines                     |Industrials                |Airlines                                    |1994-07-01       |
|SPGI          |SP Global, Inc.                        |Financials                 |Financial Exchanges  Data                   |                 |
|SWK           |Stanley Black  Decker                  |Consumer Discretionary     |Household Appliances                        |1982-09-30       |
|SBUX          |Starbucks Corp.                        |Consumer Discretionary     |Restaurants                                 |                 |
|STT           |State Street Corp.                     |Financials                 |Asset Management  Custody Banks             |                 |
|SRCL          |Stericycle Inc                         |Industrials                |Environmental  Facilities Services          |2008-11-19       |
|SYK           |Stryker Corp.                          |Health Care                |Health Care Equipment                       |2000-12-12       |
|STI           |SunTrust Banks                         |Financials                 |Regional Banks                              |1988-05-31       |
|SYMC          |Symantec Corp.                         |Information Technology     |Application Software                        |2003-03-25       |
|SYF           |Synchrony Financial                    |Financials                 |Consumer Finance                            |2015-11-18       |
|SNPS          |Synopsys Inc.                          |Information Technology     |Application Software                        |2017-03-16       |
|SYY           |Sysco Corp.                            |Consumer Staples           |Food Distributors                           |1986-12-31       |
|TROW          |T. Rowe Price Group                    |Financials                 |Asset Management  Custody Banks             |                 |
|TPR           |Tapestry, Inc.                         |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |                 |
|TGT           |Target Corp.                           |Consumer Discretionary     |General Merchandise Stores                  |1976-12-31       |
|TEL           |TE Connectivity Ltd.                   |Information Technology     |Electronic Manufacturing Services           |2011-10-17       |
|FTI           |TechnipFMC                             |Energy                     |Oil  Gas Equipment  Services                |2009-06-05       |
|TXN           |Texas Instruments                      |Information Technology     |Semiconductors                              |                 |
|TXT           |Textron Inc.                           |Industrials                |Aerospace  Defense                          |1978-12-31       |
|TMO           |Thermo Fisher Scientific               |Health Care                |Health Care Equipment                       |                 |
|TIF           |Tiffany  Co.                           |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2000-06-21       |
|TWX           |Time Warner Inc.                       |Consumer Discretionary     |Cable  Satellite                            |                 |
|TJX           |TJX Companies Inc.                     |Consumer Discretionary     |Apparel Retail                              |1985-09-30       |
|TMK           |Torchmark Corp.                        |Financials                 |Life  Health Insurance                      |1989-04-30       |
|TSS           |Total System Services                  |Information Technology     |Internet Software  Services                 |2008-01-02       |
|TSCO          |Tractor Supply Company                 |Consumer Discretionary     |Specialty Stores                            |2014-01-24       |
|TDG           |TransDigm Group                        |Industrials                |Aerospace  Defense                          |2016-06-03       |
|TRV           |The Travelers Companies Inc.           |Financials                 |Property  Casualty Insurance                |2002-08-21       |
|TRIP          |TripAdvisor                            |Consumer Discretionary     |Internet  Direct Marketing Retail           |2011-12-21       |
|FOXA          |Twenty-First Century Fox Class A       |Consumer Discretionary     |Publishing                                  |2013-07-01       |
|FOX           |Twenty-First Century Fox Class B       |Consumer Discretionary     |Publishing                                  |2015-09-18       |
|TSN           |Tyson Foods                            |Consumer Staples           |Packaged Foods  Meats                       |                 |
|UDR           |UDR Inc                                |Real Estate                |Residential REITs                           |2016-03-07       |
|ULTA          |Ulta Salon Cosmetics  Fragrance Inc    |Consumer Discretionary     |Specialty Stores                            |2016-04-18       |
|USB           |U.S. Bancorp                           |Financials                 |Diversified Banks                           |                 |
|UAA           |Under Armour Class A                   |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2016-03-25       |
|UA            |Under Armour Class C                   |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |2014-05-01       |
|UNP           |Union Pacific                          |Industrials                |Railroads                                   |                 |
|UAL           |United Continental Holdings            |Industrials                |Airlines                                    |2015-09-03       |
|UNH           |United Health Group Inc.               |Health Care                |Managed Health Care                         |1994-07-01       |
|UPS           |United Parcel Service                  |Industrials                |Air Freight  Logistics                      |2002-07-22       |
|URI           |United Rentals, Inc.                   |Industrials                |Trading Companies  Distributors             |2014-09-20       |
|UTX           |United Technologies                    |Industrials                |Aerospace  Defense                          |                 |
|UHS           |Universal Health Services, Inc.        |Health Care                |Health Care Facilities                      |2014-09-20       |
|UNM           |Unum Group                             |Financials                 |Life  Health Insurance                      |1994-03-01       |
|VFC           |V.F. Corp.                             |Consumer Discretionary     |Apparel, Accessories  Luxury Goods          |1979-06-30       |
|VLO           |Valero Energy                          |Energy                     |Oil  Gas Refining  Marketing                |                 |
|VAR           |Varian Medical Systems                 |Health Care                |Health Care Equipment                       |2007-02-12       |
|VTR           |Ventas Inc                             |Real Estate                |Health Care REITs                           |2009-03-04       |
|VRSN          |Verisign Inc.                          |Information Technology     |Internet Software  Services                 |2006-02-01       |
|VRSK          |Verisk Analytics                       |Industrials                |Research  Consulting Services               |2015-10-08       |
|VZ            |Verizon Communications                 |Telecommunication Services |Integrated Telecommunication Services       |1983-11-30       |
|VRTX          |Vertex Pharmaceuticals Inc             |Health Care                |Biotechnology                               |2013-09-23       |
|VIAB          |Viacom Inc.                            |Consumer Discretionary     |Cable  Satellite                            |                 |
|V             |Visa Inc.                              |Information Technology     |Internet Software  Services                 |2009-12-21       |
|VNO           |Vornado Realty Trust                   |Real Estate                |Office REITs                                |                 |
|VMC           |Vulcan Materials                       |Materials                  |Construction Materials                      |1999-06-30       |
|WMT           |Wal-Mart Stores                        |Consumer Staples           |Hypermarkets  Super Centers                 |1982-08-31       |
|WBA           |Walgreens Boots Alliance               |Consumer Staples           |Drug Retail                                 |1979-12-31       |
|DIS           |The Walt Disney Company                |Consumer Discretionary     |Cable  Satellite                            |1976-06-30       |
|WM            |Waste Management Inc.                  |Industrials                |Environmental  Facilities Services          |                 |
|WAT           |Waters Corporation                     |Health Care                |Health Care Distributors                    |                 |
|WEC           |Wec Energy Group Inc                   |Utilities                  |Electric Utilities                          |2008-10-31       |
|WFC           |Wells Fargo                            |Financials                 |Diversified Banks                           |1976-06-30       |
|HCN           |Welltower Inc.                         |Real Estate                |Health Care REITs                           |2009-01-30       |
|WDC           |Western Digital                        |Information Technology     |Technology Hardware, Storage  Peripherals   |2009-07-01       |
|WU            |Western Union Co                       |Information Technology     |Internet Software  Services                 |                 |
|WRK           |WestRock Company                       |Materials                  |Paper Packaging                             |                 |
|WY            |Weyerhaeuser Corp.                     |Real Estate                |Specialized REITs                           |                 |
|WHR           |Whirlpool Corp.                        |Consumer Discretionary     |Household Appliances                        |                 |
|WMB           |Williams Cos.                          |Energy                     |Oil  Gas Storage  Transportation            |1975-03-31       |
|WLTW          |Willis Towers Watson                   |Financials                 |Insurance Brokers                           |2016-01-05       |
|WYN           |Wyndham Worldwide                      |Consumer Discretionary     |Hotels, Resorts  Cruise Lines               |                 |
|WYNN          |Wynn Resorts Ltd                       |Consumer Discretionary     |Casinos  Gaming                             |2008-11-14       |
|XEL           |Xcel Energy Inc                        |Utilities                  |Multi-Utilities                             |                 |
|XRX           |Xerox Corp.                            |Information Technology     |Technology Hardware, Storage  Peripherals   |                 |
|XLNX          |Xilinx Inc                             |Information Technology     |Semiconductors                              |1999-11-08       |
|XL            |XL Capital                             |Financials                 |Property  Casualty Insurance                |                 |
|XYL           |Xylem Inc.                             |Industrials                |Industrial Machinery                        |2011-11-01       |
|YUM           |Yum! Brands Inc                        |Consumer Discretionary     |Restaurants                                 |1997-10-06       |
|ZBH           |Zimmer Biomet Holdings                 |Health Care                |Health Care Equipment                       |2001-08-07       |
|ZION          |Zions Bancorp                          |Financials                 |Regional Banks                              |                 |
|ZTS           |Zoetis                                 |Health Care                |Pharmaceuticals                             |2013-06-21       |
    




{% highlight r %}
#******************************************************************
# Extract components from SPDR ETF which track S&P 500 Index.
# The list of available sectors are : XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU.
#****************************************************************** 
{% endhighlight %}

{% highlight r %}
qis.sector.spdr.components('XLE')
qis.sector.spdr.components('XLE', "weights")
qis.sector.spdr.components('XLE', "prices")
{% endhighlight %}

{% highlight r %}
print(to.nice(qis.sector.spdr.components('XLE', "all")))
{% endhighlight %}



|Symbol |Company.Name               |Index.Weight |Last   |Change |X.Change |Volume  |X52.Week.Range    |
|:------|:--------------------------|:------------|:------|:------|:--------|:-------|:-----------------|
|XOM    |Exxon Mobil Corp           |22.61%       |75.80  |0.48   |+0.64%   |29.49 M |73.90 - 89.30     |
|CVX    |Chevron Corp               |16.96%       |113.50 |1.20   |+1.07%   |10.92 M |102.55 - 133.88   |
|SLB    |Schlumberger Ltd           |7.16%        |65.20  |-2.21  |-3.28%   |19.73 M |61.02 - 82.71     |
|COP    |ConocoPhillips             |4.69%        |52.00  |-0.94  |-1.77%   |9.69 M  |42.27 - 61.32     |
|EOG    |EOG Resources              |4.62%        |100.10 |-0.97  |-0.96%   |5.34 M  |81.99 - 119.00    |
|OXY    |Occidental Petroleum       |4.18%        |68.20  |-0.29  |-0.42%   |5.44 M  |57.20 - 78.09     |
|HAL    |Halliburton Co             |3.29%        |46.70  |-0.71  |-1.50%   |17.77 M |38.18 - 56445.00  |
|PSX    |Phillips 66                |3.21%        |92.60  |0.16   |+0.17%   |2.68 M  |75.14 - 107.47    |
|VLO    |Valero Energy Corp         |3.12%        |88.20  |1.45   |+1.67%   |5.46 M  |60.69 - 99.95     |
|KMI    |Kinder Morgan Inc          |2.69%        |17.20  |0.44   |+2.62%   |30.84 M |16.56 - 22.71     |
|MPC    |Marathon Petroleum Corp.   |2.54%        |63.80  |1.05   |+1.67%   |4.87 M  |47.78 - 73.53     |
|APC    |Anadarko Petroleum Corp    |2.51%        |56.10  |-0.07  |-0.12%   |8.75 M  |39.96 - 70.00     |
|PXD    |Pioneer Natural Resources  |2.30%        |165.10 |-4.11  |-2.43%   |2.85 M  |125.46 - 199.83   |
|WMB    |The Williams Companies Inc |1.95%        |28.60  |0.05   |+0.18%   |10.99 M |26.82 - 33.67     |
|OKE    |ONEOK Inc                  |1.86%        |54.30  |-0.07  |-0.13%   |3.60 M  |47.14 - 61.36     |
|CXO    |Concho Resources Inc       |1.71%        |139.00 |-1.11  |-0.79%   |2.35 M  |106.73 - 162.91   |
|DVN    |Devon Energy Corp          |1.51%        |34.70  |-0.26  |-0.74%   |11.52 M |28.79 - 47.25     |
|APA    |Apache Corp                |1.20%        |37.20  |-0.50  |-1.33%   |5.90 M  |35.70 - 57.88     |
|ANDV   |Andeavor                   |1.18%        |97.30  |0.41   |+0.42%   |1.74 M  |75.11 - 121.71    |
|MRO    |Marathon Oil Corp          |1.12%        |15.50  |-0.17  |-1.08%   |17.71 M |10.55 - 19.52     |
|FTI    |TechnipFMC Ltd             |1.09%        |29.10  |0.04   |+0.14%   |6.52 M  |24.53 - 35.00     |
|NOV    |National Oilwell Varco Inc |1.08%        |33.10  |0.45   |+1.38%   |5.52 M  |29.90 - 41.90     |
|NBL    |Noble Energy Inc           |1.07%        |25.70  |0.35   |+1.38%   |12.05 M |22.99 - 39.60     |
|HES    |Hess Corp                  |0.98%        |42.10  |-0.88  |-2.05%   |6.92 M  |37.25 - 55.48     |
|BHGE   |Baker Hughes, a GE company |0.97%        |26.50  |-1.00  |-3.64%   |6.34 M  |27.45 - 35755.00  |
|EQT    |EQT Corporation            |0.96%        |45.70  |-0.52  |-1.12%   |4.45 M  |43.70 - 67.84     |
|COG    |Cabot Oil & Gas A          |0.94%        |23.60  |0.63   |+2.74%   |9.29 M  |21.40 - 29.57     |
|XEC    |Cimarex Energy Co          |0.83%        |100.20 |0.00   |0.00%    |1.77 M  |89.49 - 126815.00 |
|HP     |Helmerich & Payne Inc      |0.64%        |63.60  |-0.55  |-0.86%   |3.05 M  |42.16 - 75.02     |
|NFX    |Newfield Exploration Co    |0.49%        |26.00  |-0.40  |-1.52%   |5.41 M  |24.41 - 43.74     |
|RRC    |Range Resources Corp       |0.30%        |12.70  |-0.11  |-0.86%   |12.94 M |12.70 - 34.09     |
|CHK    |Chesapeake Energy Corp     |0.28%        |2.90   |0.03   |+1.06%   |66.47 M |2.80 - 6.59       |
    

{% highlight r %}
qis.sector.spdr.components('XLE')
qis.sector.spdr.components('XLY','all')
qis.sector.spdr.components('XLP','all')
qis.sector.spdr.components('XLF','all')
qis.sector.spdr.components('XLV','all')
qis.sector.spdr.components('XLI','all')
qis.sector.spdr.components('XLB','all')
qis.sector.spdr.components('XLK','all')
qis.sector.spdr.components('XLU','all')
{% endhighlight %}



*(this report was produced on: 2018-02-11)*
