# BackTesting Connors RSI(Relative Strength Index) Strategy

# Compute Connor's RSI, depends on RSI TTR function
connorsRSI <- function(price, nRSI = 3, nStreak = 2,
              nPercentLookBack = 100) {
  priceRSI <- RSI(price, nRSI)
  streakRSI <- RSI(computeStreak(price), nStreak)
  percents <- round(runPercentRank(x = diff(log(price)),
              n = 100, cumulative = F, exact.multiplier = 1) * 100)
  ret <- (priceRSI + streakRSI + percents) / 3
  colnames(ret) <- 'connorsRSI'
  return(ret)
}

# Computes a running streak of positives and
# negatives of price changes
computeStreak <- function(priceSeries) {
  signs <- sign(diff(priceSeries))
  posDiffs <- negDiffs <- rep(0, length(signs))
  posDiffs[signs == 1] <- 1
  negDiffs[signs == -1] <- -1
  
  # Create vector of cumulative sums and cumulative
  # sums not incremented during streaks.
  posCum <- cumsum(posDiffs)
  posNAcum <- posCum
  posNAcum[posDiffs == 1] <- NA
  posNAcum <- na.locf(posNAcum, na.rm = FALSE) 
  posNAcum[is.na(posNAcum)] <- 0
  posStreak <- posCum - posNAcum
  
  # Repeat for negative cumulative sums
  negCum <- cumsum(negDiffs)
  negNAcum <- negCum
  negNAcum[negDiffs == -1] <- NA
  negNAcum <- na.locf(negNAcum, na.rm = FALSE)
  negNAcum[is.na(negNAcum)] <- 0
  negStreak <- negCum - negNAcum
  
  streak <- posStreak + negStreak
  streak <- xts(streak, order.by = index(priceSeries))
  return (streak)
}

sigAND <- function(label, data=mktdata, columns, cross = FALSE) {
  ret_sig = NULL
  colNums <- rep(0, length(columns)) 
  for(i in 1:length(columns)) {
    colNums[i] <- match.names(columns[i], colnames(data)) 
    }
  ret_sig <- data[, colNums[1]] 
  for(i in 2:length(colNums)) { 
    ret_sig <- ret_sig & data[, colNums[i]]
  }
  ret_sig <- ret_sig * 1 
  if (isTRUE(cross))
    ret_sig <- diff(ret_sig) == 1
  colnames(ret_sig) <- label 
  return(ret_sig)
}

cumCRSI <- function(price, nCum = 2, ...) {
  CRSI <- connorsRSI(price, ...) 
  out <- runSum(CRSI, nCum) 
  colnames(out) <- "cumCRSI"
  out 
}

rm(list = ls(.blotter), envir = .blotter) 
initDate = '1990-01-01'
from = "2003-01-01"
to = "2013-12-31"
initEq = 10000
currency('USD')
Sys.setenv(TZ="UTC")
source("src/demoData.R")

strategy.st <- "CRSIcumStrat"
portfolio.st <- "CRSIcumStrat" 
account.st <- "CRSIcumStrat"
rm.strat(portfolio.st) 
rm.strat(strategy.st)
initPortf(portfolio.st, symbols = symbols, initDate = initDate, currency = 'USD')
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = 'USD', initEq = initEq)
initOrders(portfolio.st, initDate = initDate) 
strategy(strategy.st, store = TRUE)

# Parameters 
cumThresh <- 40 
exitThresh <- 75 
nCum <- 2
nRSI <- 3
nStreak <- 2 
nPercentLookBack <- 100 
nSMA <- 200
pctATR <- .02
period <- 10

# Indicators
add.indicator(strategy.st, name = "cumCRSI", arguments = list(price = quote(Cl(mktdata)), nCum = nCum, nRSI = nRSI, nStreak = nStreak, nPercentLookBack = nPercentLookBack), label = "CRSIcum")

add.indicator(strategy.st, name = "connorsRSI", arguments = list(price = quote(Cl(mktdata)), nRSI = nRSI, nStreak = nStreak, nPercentLookBack = nPercentLookBack), label = "CRSI")

add.indicator(strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = nSMA), label = "sma")

add.indicator(strategy.st, name = "lagATR", arguments = list(HLC = quote(HLC(mktdata)), n = period), label = "atrX")

# Signals
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "cumCRSI.CRSIcum", threshold = cumThresh, relationship = "lt", cross = F), label="cumCRSI.lt.thresh")

add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("Close", "SMA.sma"), relationship = "gt"), label = "Cl.gt.SMA")

add.signal(strategy.st, name = "sigAND", arguments = list(columns = c("cumCRSI.lt.thresh", "Cl.gt.SMA"), cross = TRUE), label = "longEntry")

add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "connorsRSI.CRSI", threshold = exitThresh, relationship = "gt", cross = TRUE), label = "longExit")

# Rules
add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "longEntry", sigval = TRUE, ordertype = "market", orderside ="long", replace = FALSE, prefer = "Open", osFUN = osDollarATR, tradeSize = tradeSize, pctATR = pctATR, atrMod = "X"), type = "enter", path.dep = TRUE)
add.rule(strategy.st, name = "ruleSignal", arguments = list(sigcol = "longExit", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open"), type = "exit", path.dep = TRUE)

# Apply Strategy
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
t2 <- Sys.time()
print(t2 - t1)

# Set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)
tStats <- tradeStats(Portfolios = portfolio.st, inclZeroDays = FALSE)  # use = 'trades

aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses)
aggCorrect <- mean(tStats$Percent.Positive)
numTrades <- sum(tStats$Num.Trades)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[ tStats$Avg.WinLoss.Ratio < Inf], na.rm = TRUE)


chart.Posn(portfolio.st, "XLB")
