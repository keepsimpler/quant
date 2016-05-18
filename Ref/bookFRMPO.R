#Code for book <Financial Risk Modelling and Portfolio Optimization with R>

#Charpter 3
library(fBasics)
library(evir)
data("siemens")
SieDates <- as.character(format(as.POSIXct(attr(siemens, 'times')), '%Y-%m-%d'))
SieRet <- timeSeries(siemens * 100, charvec = SieDates)
colnames(SieRet) <- 'SieRet'
## Stylised Facts 1
seriesplot