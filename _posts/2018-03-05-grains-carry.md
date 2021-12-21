---
layout: post
title: Carry Strategy in Grain & Oilseed products
comments: false
---




This is a continuation of the [Carry Strategy in Energy commodities](https://sysresearcher.github.io/energy-carry),
[Carry Strategy in Livestock commodities](https://sysresearcher.github.io/livestock-carry)  and [Carry Strategy in Industrial metals](https://sysresearcher.github.io/metals-carry) posts.
Here we will take a look on grain and oilseed products. The information about futures contracts used in the strategies can be found here:
 
 * [Corn](http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/corn.html)
 * [Wheat](http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/wheat.html)
 * [Soybean](http://www.cmegroup.com/trading/agricultural/grain-and-oilseed/soybean.html)


{% highlight r %}
#*****************************************************************
# Input parameters
#*****************************************************************
config.file = "commodity.carry.yaml"
{% endhighlight %}


{% highlight r %}
# save trade report
save.report = T

# save strategy level
save.index = T

# save expiration table
save.expiration = F

# calculate strategy
run.strategy = F

# add trading cost
trading.cost = T

# analyse strategy
analyse.strategy = T

#*****************************************************************
# Set input and output directories
#*****************************************************************

# set working directory
qis.proj = getwd()

# load configuration
config = yaml.load_file(paste0(qis.proj,'/config/', config.file))

# define input and output directories
qis.data   = paste0(qis.data,'futures/cmdty/')
qis.out    = paste0(qis.proj,'/out/',config$strategy, iif(trading.cost,'/wc/','/nc/'))

# check if directory with data is available
if(!file.exists(qis.data)) stop(paste0(qis.data, " does not exist"))

# create output directory
if(!file.exists(qis.out))  dir.create(qis.out, showWarnings = FALSE, recursive = FALSE, mode = "0777")

# create storage for strategy levels
dir.create(paste0(qis.out,"index/"), showWarnings = FALSE, recursive = FALSE, mode = "0777")

# create storage for trading report
if(save.report) dir.create(paste0(qis.out,"report/"), showWarnings = FALSE, recursive = FALSE, mode = "0777")

# calculate strategies
if(run.strategy) {
  
  cat("running",config.file, "...\n")
  
  # read data
  rdata  = qis.read.cmdty.futures.data(qis.data)
  
  # store all strategies
  index = lst()
  
  # LOOP over commodities
  for(i in 1:len(config$underlying)) {
    
    # LOCAL clock
    tic(1)
    
    cat("running",config$underlying[i], "...\n")
    
    # subset data 
    data   = subset(rdata, underlying == config$underlying[i])
    
    # create dates according to evolution calendar and available dates in the data
    dates.common = qis.intersect.all(unique(data$date), expiry.schedule("DAILY", spl(config$evol.calendar[i]), min(data$date), max(data$date)))
    
    # adjust data
    data = data[data$date %in%  dates.common,]
    
    # Cast data to matrix
    data                      = qis.acast(data, "date", "symbols", cast.names)
    data$save.report          = save.report
    data$save.expiration      = save.expiration
    data$save.index           = save.index
    
    # Current index of commodity to process
    data$cur.index = i
    
    # Create schedule for rolling future strategies
    data = qis.future.rfs.schedule(data,config)
    
    # Create rolling futures strategy
    data = qis.future.rfs.index(data,config,"front")
    data = qis.future.rfs.index(data,config,"back")
    
    # Create schedule for LS strategy
    data = qis.future.ls.schedule(data,config)
    
    # Create LS strategy
    data = qis.future.ls.index(data,config)
    
    # Store index
    write.xts(data$index, paste0(qis.out,"index/",names(data$index),".csv"))
    
    # Store report 
    if(save.report)  write.xts(data$report, paste0(qis.out,"report/",names(data$index),".csv"))
      
    # LOCAL clock
    toc(1)
  }
  
  # merge LS strategies
  out = qis.merge.csv.files(paste0(qis.out,"index/"))
  write.xts(out,paste0(qis.out,"output.csv"))
}

# analyse strategies
if(analyse.strategy) {
  
  # define input directories with strategies
  qis.out.nc   = paste0(qis.proj,'/out/',config$strategy, '/nc/')
  qis.out.wc   = paste0(qis.proj,'/out/',config$strategy, '/wc/')
  
  # Read pre-computed strategy 
  strategy.index.nc = read.xts(paste0(qis.out.nc,"output.csv"))
  strategy.index.wc = read.xts(paste0(qis.out.wc,"output.csv"))
  
  # Scale strategy to start from one
  strategy.index.nc = scale.one(strategy.index.nc)
  strategy.index.wc = scale.one(strategy.index.wc)
  
  #*****************************************************************
  # Plot Strategies
  #*****************************************************************
  names = names(strategy.index.nc)
  layout(matrix(1:4,2,2))
  for(i in 1:len(names)) {
    name   = names[i]
    tmp.nc = strategy.index.nc[,name]
    tmp.nc = iif(name == "rbob", tmp.nc['2006::'], tmp.nc)
    tmp.wc = strategy.index.wc[,name]
    tmp.wc = iif(name == "rbob", tmp.wc['2006::'], tmp.wc)
    
    qis.plot(tmp.nc, type='l', main = paste0("Carry strategy on ", toupper(name)), ylim = range(tmp.nc), col = col.add.alpha('blue', 150) )
    qis.plot.lines(tmp.wc, type='l',col = col.add.alpha('red', 150))
    qis.plot.legend(spl('NC,WC'),'blue,red',as.list(merge(tmp.nc,tmp.wc)))
  }
  
  #*****************************************************************
  # Create summary
  #*****************************************************************
  models.nc = lst()
  models.wc = lst()
  for(i in 1:len(names)) {
    name = names[i]
    tmp.nc  = strategy.index.nc[,name]
    tmp.nc  = iif(name == "rbob", tmp.nc['2006::'], tmp.nc)
    tmp.wc  = strategy.index.wc[,name]
    tmp.wc  = iif(name == "rbob", tmp.wc['2006::'], tmp.wc)
    
    # calculate return
    return.nc = tmp.nc / mlag(tmp.nc) -1
    return.nc = ifna(return.nc,0)
    return.wc = tmp.wc / mlag(tmp.wc) -1
    return.wc = ifna(return.wc,0)
   
    # create model
    models.nc[[paste0(name,".NC")]]  = lst(ret = return.nc, equity = tmp.nc, weight = NA)
    models.wc[[paste0(name,".WC")]]  = lst(ret = return.wc, equity = tmp.wc, weight = NA)
  }
  
  print(qis.plot.strategy.sidebyside(models.nc, make.plot=F, return.table=T, perfromance.fn = qis.strategy.stat.default))
  print(qis.plot.strategy.sidebyside(models.wc, make.plot=F, return.table=T, perfromance.fn = qis.strategy.stat.default))
  
  
  #*****************************************************************
  # Create weights
  #*****************************************************************
  layout(matrix(1:4,2,2))
  for(i in 1:len(names)) {
    name = names[i]
  
    # Read trading report
    report = read.csv(paste0(qis.out.nc, "report/", name, ".csv"))
    
    # Group weights by front and back leg
    weights = make.xts( cbind(Weight.Front  = report$Weight1 + report$Weight2, Weight.Back = report$Weight3 + report$Weight4), as.Date(report[,'Date']))
    
    front.weight =  make.xts( cbind(Weight.Front  = report$Weight1 + report$Weight2), as.Date(report[,'Date']))
    back.weight  =  make.xts( cbind(Weight.Back   = report$Weight3 + report$Weight4), as.Date(report[,'Date']))
    net.weight   =  make.xts( cbind(Weight.Net    = report$Weight1 + report$Weight2 + report$Weight3 + report$Weight4), as.Date(report[,'Date']))
    
    weights       = iif(name == "rbob", weights['2006::'],      iif(name == "hog", weights['2001-03::'], weights['2000-03::']))
    front.weight  = iif(name == "rbob", front.weight['2006::'], iif(name == "hog", front.weight['2001-03::'],front.weight['2000-03::']))
    back.weight   = iif(name == "rbob", back.weight['2006::'],  iif(name == "hog", back.weight['2001-03::'],back.weight['2000-03::']))
    net.weight    = iif(name == "rbob", net.weight['2006::'],   iif(name == "hog", net.weight['2001-03::'],net.weight['2000-03::']))
    
    
    ylim = c(range(weights,na.rm = T)[1] -1, range(weights,na.rm = T)[2] + 1)
    
    qis.plot(front.weight, type='l', main = paste0("Carry strategy on ", toupper(name)), ylim = ylim, col = col.add.alpha('red', 150) )
    qis.plot.lines(back.weight, type='l',col = col.add.alpha('blue', 150))
    qis.plot.lines(net.weight, type='l',col = col.add.alpha('green', 150))
    qis.plot.legend(spl('Front,Back,Net'),'red,blue,green',as.list(merge(front.weight,back.weight,net.weight)))
  }
}
{% endhighlight %}

![plot of chunk plot-4](/public/images/2018-03-05-grains-carry/plot-4-1.png)

|           |kansas.corn.NC    |soybeans.NC       |wheat.NC          |
|:----------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2000 - Mar2018 |Jan2000 - Mar2018 |Jan2000 - Mar2018 |
|Cagr       |4.57              |-0.1              |5.69              |
|AveRet     |4.7               |-0.11             |5.86              |
|Sharpe     |0.79              |0.02              |0.76              |
|Volatility |6.06              |6.8               |7.86              |
|MaxDD      |-16.29            |-24.02            |-17.01            |
|AveDD      |-0.79             |-2.48             |-1.01             |
|VaR        |-0.48             |-0.64             |-0.6              |
|CVaR       |-0.87             |-1.1              |-1.18             |
    




|           |kansas.corn.WC    |soybeans.WC       |wheat.WC          |
|:----------|:-----------------|:-----------------|:-----------------|
|Period     |Jan2000 - Mar2018 |Jan2000 - Mar2018 |Jan2000 - Mar2018 |
|Cagr       |4.23              |-0.43             |5.34              |
|AveRet     |4.35              |-0.44             |5.5               |
|Sharpe     |0.73              |-0.03             |0.72              |
|Volatility |6.06              |6.8               |7.86              |
|MaxDD      |-16.36            |-25.8             |-17.33            |
|AveDD      |-0.82             |-4.35             |-1.02             |
|VaR        |-0.48             |-0.64             |-0.61             |
|CVaR       |-0.87             |-1.1              |-1.18             |
    


![plot of chunk plot-4](/public/images/2018-03-05-grains-carry/plot-4-2.png)

 As expected, we have roughly 100% per cent on the long and 100% per cent on the short side with net weights noising around zero.
 Since there is a lag between trade decision date and actual trade date, we do not expect to have a perfect boundary on each side.

*(this report was produced on: 2018-03-24)*
