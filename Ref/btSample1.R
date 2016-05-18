## http://www.quantsportal.com/vectorised-backtest-in-r/

# Load/attach packages

require(quantmod)

require(PerformanceAnalytics)



# Step 1: Get the data

getSymbols("^GSPC")



# Step 2: Create your indicators

longSMA <- SMA(Cl(GSPC[‘2008-01-02/2010-09-07’]), 25)

shortSMA <- SMA(Cl(GSPC[‘2008-01-02/2010-09-07’]), 5)

rsi <- RSI(Cl(GSPC), n = 14, wilder = TRUE )



# Step 3: Construct your trading rule

# Remember to lag the indicator to remove look-ahead bias

signals <- Lag(ifelse(shortSMA > longSMA, 1, -1))



##Position Sizes

# Remember to lag the indicator to remove look-ahead bias

positionSize <- Lag(ifelse(rsi < 30, 0.3, ifelse(rsi > 70, 0.3, 1)))



# Step 4: Get the returns

returns <- ROC(Cl(GSPC), type = “discrete”) * signals  * positionSize

returns <- returns[‘2008-02-07/2010-09-07’]



# Step 5: Evaluate strategy performance

charts.PerformanceSummary(returns)

table.Drawdowns(returns, top=5)

table.DownsideRisk(returns)

