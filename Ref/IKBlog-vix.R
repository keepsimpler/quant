## https://quantstrattrader.wordpress.com/
## How well can you scale your strategy?

require(downloader)
require(PerformanceAnalytics)
require(IKTrading)
require(TTR)

download("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vxvdailyprices.csv", 
         destfile="vxvData.csv")
download("https://dl.dropboxusercontent.com/s/jk6der1s5lxtcfy/XIVlong.TXT",
         destfile="longXIV.txt")
download("https://dl.dropboxusercontent.com/s/950x55x7jtm9x2q/VXXlong.TXT", 
         destfile="longVXX.txt") #requires downloader package
getSymbols('^VIX', from = '1990-01-01')


xiv <- xts(read.zoo("longXIV.txt", format="%Y-%m-%d", sep=",", header=TRUE))
vxx <- xts(read.zoo("longVXX.txt", format="%Y-%m-%d", sep=",", header=TRUE))
vxv <- xts(read.zoo("vxvData.csv", header=TRUE, sep=",", format="%m/%d/%Y", skip=2))
vixVxv <- Cl(VIX)/Cl(vxv)


#xiv <- xts(read.zoo("longXIV.txt", format="%Y-%m-%d", sep=",", header=TRUE))
#vxx <- xts(read.zoo("longVXX.txt", format="%Y-%m-%d", sep=",", header=TRUE))

vxxCloseRets <- Return.calculate(Cl(vxx))
vxxOpenRets <- Return.calculate(Op(vxx))
xivCloseRets <- Return.calculate(Cl(xiv))
xivOpenRets <- Return.calculate(Op(xiv))

vxxSig <- vixVxv > 1
xivSig <- 1-vxxSig

magicThinking <- vxxCloseRets * lag(vxxSig) + xivCloseRets * lag(xivSig)
nextOpen <- vxxOpenRets * lag(vxxSig, 2) + xivOpenRets * lag(xivSig, 2)
nextClose <- vxxCloseRets * lag(vxxSig, 2) + xivCloseRets * lag(xivSig, 2)
tradeWholeDay <- (nextOpen + nextClose)/2

compare <- na.omit(cbind(magicThinking, nextOpen, nextClose, tradeWholeDay))
colnames(compare) <- c("Magic Thinking", "Next Open", 
                       "Next Close", "Execute Through Next Day")
charts.PerformanceSummary(compare)
rbind(table.AnnualizedReturns(compare), 
      maxDrawdown(compare), CalmarRatio(compare))

par(mfrow=c(1,1))
chart.TimeSeries(log(cumprod(1+compare), base = 10), legend.loc='topleft', ylab='log base 10 of additional equity',
                 main = 'VIX vx. VXV different execution times')
