# BackTesting for Laged ATR strategy

require(quantstrat)
require(PerformanceAnalytics)

initDate = '1990-01-01'
from = '2003-01-01'
to = '2013-01-01'
options(width = 70)

# To rerun the strategy, rerun everything below this line
# demoData.R contains all of the data-related boilerplate
source('src/demoData.R')

# Trade sizing and initial equity settings
tradeSize <- 10000
initEq <- tradeSize * length(symbols)

strategy.st <- 'Clenow_Simple'
portfolio.st <- 'Clenow_Simple'
account.st <- 'Clenow_Simple'
rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols,
          initDate = initDate, currency = 'USD')

initAcct(account.st, portfolios = portfolio.st,
         initDate = initDate, currency = 'USD', initEq = initEq)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store = TRUE)

# the parameters and the function that will be used as our signal-generating indicator.
nLag = 252
pctATR = 0.02
period = 10

namedLag <- function(x, k = 1, na.pad = TRUE, ...) {
  out <- lag(x, k = k, na.pad = na.pad, ...)
  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- 'namedLag'
  return(out)
}
"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...) {
  ATR <- ATR(HLC, n = n, maType = maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- 'atr'
  return(out)
}

# Indicators
add.indicator(strategy.st, name = 'namedLag',
              arguments = list(x = quote(Cl(mktdata)), k = nLag),
              label = 'ind')

add.indicator(strategy.st, name = 'lagATR',
              arguments = list(HLC = quote(HLC(mktdata)), n = period),
              label = 'atrX')

test <- applyIndicators(strategy.st, mktdata = OHLC(XLB))
head(round(test, 2), 253)

#Signals
add.signal(strategy.st, name = 'sigCrossover',
           arguments = list(columns = c('Close', 'namedLag.ind'),
                            relationship = 'gt'),
           label = 'coverOrBuy')

add.signal(strategy.st, name = 'sigCrossover',
           arguments = list(columns = c('Close', 'namedLag.ind'),
                            relationship = 'lt'),
           label = 'sellOrShort')

# Order Sizing function
osDollarATR <- function(orderside, tradeSize, pctATR,
                maxPctATR = pctATR, data, timestamp,
                symbol, prefer = 'Open', portfolio, integerQty = T,
                atrMod = '', rebal = F, ...) {
  if (tradeSize > 0 & orderside == 'short') {
    tradeSize <- tradeSize * -1
  }
  
  pos <- getPosQty(portfolio, symbol, timestamp)
  atrString <- paste0('atr', atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  
  if (length(atrCol) == 0) {
    stop(paste('Term', atrString,
               'not found in mktdata column names.'))
  }
  
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if (is.na(atrTimeStamp) | atrTimeStamp == 0) {
    stop(paste('ATR corresponding to', atrString,
        'is invalid at this point in time. Add a logical
        operator to account for this.'))
  }
  
  dollarATR <- pos * atrTimeStamp
  desiredDollarATR <- pctATR * tradeSize
  remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR
  
  if (orderside == 'long') {
    qty <- min(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  } else {
    qty <- max(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  }
  
  if (integerQty) {
    qty <- trunc(qty)
  }
  if (!rebal) {
    if (orderside == 'long' & qty < 0) {
      qty <- 0
    }
    if (orderside == 'short' & qty > 0) {
      qty <- 0
    }
  }
  if (rebal) {
    if (pos == 0) {
      qty <- 0
    }
  }
  return(qty)
}

# Long rules
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sigcol = 'coverOrBuy',
          sigval = T, ordertype = 'market',
          orderside = 'long', replace = F,
          prefer = 'Open', osFUN = osDollarATR,
          tradeSize = tradeSize, pctATR = pctATR,
          atrMod = 'X'), type = 'enter', path.dep = T)
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sigcol = 'sellOrShort',
          sigval = T, orderqty = 'all',
          ordertype = 'market', orderside = 'long',
          replace = F, prefer = 'Open'),
         type = 'exit', path.dep = T)
# Short rules
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sigcol = 'sellOrShort',
          sigval = T, ordertype = 'market',
          orderside = 'short', replace = F,
          prefer = 'Open', osFUN = osDollarATR,
          tradeSize = - tradeSize, pctATR = pctATR,
          atrMod = 'X'), type = 'enter', path.dep = T)
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sigcol = 'coverOrBuy',
          sigval = T, orderqty = 'all',
          ordertype = 'market', orderside = 'short',
          replace = F, prefer = 'Open'),
         type = 'exit', path.dep = T)

## Run the strategy
# Get begin time
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st,
        portfolios = portfolio.st, mktdata = OHLC(XLY))

# Record end time
t2 <- Sys.time()
print(t2 - t1)


## Evaluating the performance
# upate transactional history needed for the analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

tStats <- tradeStats(Portfolios = portfolio.st, inclZeroDays = FALSE)  # use = 'trades
tStats[, 4:ncol(tStats)] <- round(tStats[, 4:ncol(tStats)], 2)
print(data.frame((t(tStats[, -c(1, 2)]))))
aggPF <- sum(tStats$Gross.Profits) / - sum(tStats$Gross.Losses)
aggCorrect <- mean(tStats$Percent.Positive)
numTrades <- sum(tStats$Num.Trades)
meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm = T)

instRets <- PortfReturns(account.st)
portfRets <- xts(rowMeans(instRets) * ncol(instRets), order.by = index(instRets))
portfRets <- portfRets[!is.na(portfRets)]
cumPortfRets <- cumprod(1 + portfRets)
firstNonZeroDay <- as.character(index(portfRets)[min(which(portfRets != 0))])

# Obtain symbol
getSymbols('SPY', from = firstNonZeroDay, to = to)
SPYrets <- diff(log(Cl(SPY)))[-1]
cumSPYrets <- cumprod(1 + SPYrets)
comparison <- cbind(cumPortfRets, cumSPYrets)
colnames(comparison) <- c('strategy', 'SPY')
chart.TimeSeries(comparison, legend.loc = 'topleft',
      colorset = c('green', 'red'))

# Calculate risk metrics
SharpeRatio.annualized(portfRets)
Return.annualized(portfRets)
maxDrawdown(portfRets)

chart.Posn(portfolio.st, 'XLB')
tmp <- namedLag(Cl(XLB), k = nLag)
add_TA(tmp$namedLag, col = 'blue', on = 1)
