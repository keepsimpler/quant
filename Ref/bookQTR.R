## Book : Quantitative Trading with R

library(quantmod)
AAPL <- getSymbols('AAPL', auto.assign = F, src = 'google')
SPY <- getSymbols('SPY', auto.assign = F, src = 'google')
first(AAPL)
last(AAPL)

# Adding some technical indicators on top of the original plot 
chartSeries(AAPL, subset='2010::2010-04', 
            theme = chartTheme('white'), 
            TA = "addVo(); addBBands()")

# Update the original chart without specifying the full arguments
reChart(subset='2009-01-01::2009-03-03')

chartSeries(AAPL, subset='2011::2012', 
            theme = chartTheme('white'),
            TA = "addBBands(); addDEMA()")
addVo()
addDPO()

# Initial chart plot with no indicators
chartSeries(AAPL, theme = chartTheme('white'), TA = NULL)

# Custom function creation
my_indicator <- function(x) {
  return(x + 90) 
}
add_my_indicator <- newTA(FUN = my_indicator, preFUN=Cl, legend.name = "My Fancy Indicator", on = 1)

add_my_indicator()

# Create a matrix with price and volume
df <- AAPL[, c("AAPL.Close", "AAPL.Volume")] 
names(df) <- c("price", "volume")
# Create
df$return <- diff(log(df[, 1]))
df <- df[-1, ]
df$cuts <- cut(abs(df$return),
               breaks = c(0, 0.02, 0.04, 0.25), 
               include.lowest = TRUE)

# Create another column for the mean
df$means <- NA
for(i in 1:3) {
  group <- which(df$cuts == i)
  if(length(group) > 0) {
    df$means[group] <- mean(df$volume[group]) 
    }
  }

# Load ggplot2
library(ggplot2)
ggplot(df) +
  geom_histogram(aes(x=volume)) +
  facet_grid(cuts ~ .) +
  geom_vline(aes(xintercept=means), linetype="dashed", size=1)


## Charpter 6 : spreads, betas and risk

pepsi <- getSymbols('PEP',src = 'google', from = '2013-01-01',
                    to = '2014-01-01', adjust = T, 
                    auto.assign = FALSE)
coke <- getSymbols('COKE',src = 'google', from = '2013-01-01',
                   to = '2014-01-01', adjust = T, 
                   auto.assign = FALSE)
#Sys.setenv(TZ = "UTC")


prices <- cbind(pepsi[, 'PEP.Close'], coke[, 'COKE.Close']) 
price_changes <- apply(prices, 2, diff) 
plot(price_changes[, 1], price_changes[, 2],
     xlab = "Coke price changes", 
     ylab = "Pepsi price changes", 
     main = "Pepsi vs. Coke", 
     cex.main = 0.8, cex.lab = 0.8,
     cex.axis = 0.8) 
grid()

ans <- lm(price_changes[, 1] ~ price_changes[, 2])
beta <- ans$coefficients[2]

ans2 <- lm(price_changes[, 2] ~ price_changes[, 1]) 
beta2 <- ans2$coefficients[2]


# Function to calculate the spread
calculate_spread <- function(x, y, beta) {
  return(y - beta * x) 
  }
# Function to calculate the beta and level
# given start and end dates 
calculate_beta_and_level <- function(x, y, start_date, end_date) {
  require(xts)
  time_range <- paste(start_date, "::", end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  dx <- diff(x[time_range])
  dy <- diff(y[time_range]) 
  r <- prcomp( ~ dx + dy)
  beta <- r$rotation[2, 1] / r$rotation[1, 1] 
  spread <- calculate_spread(x, y, beta) 
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  outL <- list()
  outL$spread <- spread 
  outL$beta <- beta
  outL$level <- level
  return(outL)
}

# Function to calculate buy and sell signals
# with upper and lower threshold 
calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold, upper_threshold) {
  buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)
  # bind these vectors into a matrix 
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")
  return(output)
}

# Implementation
# Pick an in-sample date range
start_date <- "2011-01-01" 
end_date <- "2011-12-31"

SPY <- getSymbols('SPY', adjust = T, auto.assign = FALSE)
AAPL <- getSymbols('AAPL', adjust = T, auto.assign = FALSE)
x <- SPY[, 'SPY.Adjusted']
y <- AAPL[, 'AAPL.Adjusted']
results <- calculate_beta_and_level(x, y, start_date, end_date)
results$beta
results$level

plot(results$spread, ylab = "Spread Value", main = "AAPL - beta * SPY",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)


# Out of sample start and end dates 
start_date_out_sample <- "2012-01-01" 
end_date_out_sample <- "2012-10-22"
range <- paste(start_date_out_sample, "::",
               end_date_out_sample, sep = "")
# Out of sample analysis
spread_out_of_sample <- calculate_spread(x[range], y[range], results$beta)

plot(spread_out_of_sample, main = "AAPL - beta * SPY", 
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = results$level, lwd = 2)


# Rolling window of trading days
window_length <- 10
# Time range
start_date <- "2011-01-01" 
end_date <- "2011-12-31"
range <- paste(start_date, "::", end_date, sep = "")
# Our stock pair
x <- SPY[range, 6]
y <- AAPL[range, 6]
dF <- cbind(x, y) 
names(dF) <- c("x", "y")
# Function that we will use to calculate betas 
run_regression <- function(dF) {
  return(coef(lm(y ~ x - 1, data = as.data.frame(dF)))) 
}

rolling_beta <- function(z, width) {
  rollapply(z, width = width, FUN = run_regression, by.column = FALSE, align = "right")
}

betas <- rolling_beta(diff(dF), 10)
data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

returns <- diff(dF) / dF
return_beta <- rolling_beta(returns, 10)
data$spreadR <- diff(data$y) / data$y -
  return_beta * diff(data$x) / data$x

threshold <- sd(data$spread, na.rm = TRUE)

plot(data$spread, main = "AAPL vs. SPY In-Sample", 
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = threshold, lty = 2) 
abline(h = -threshold, lty = 2)

# Construct the out of sample spread 
# Keep the same 10 day rolling window 
window_length <- 10
# Time range
start_date <- "2012-01-01" 
end_date <- "2012-12-31"
range <- paste(start_date, "::",
               end_date, sep = "")
# Our stock pair
x <- SPY[range, 6]
y <- AAPL[range, 6]
# Bind these together into a matrix 
dF <- cbind(x, y)
names(dF) <- c("x", "y")
# Calculate the out of sample rolling beta
beta_out_of_sample <- rolling_beta(diff(dF), 10)
# Buy and sell threshold
data_out <- merge(beta_out_of_sample, dF) 
data_out$spread <- data_out$y -
  lag(beta_out_of_sample, 1) * data_out$x
# Plot the spread with in-sample bands 
plot(data_out$spread, main = "AAPL vs. SPY out of sample",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(h = threshold, lwd = 2) 
abline(h = -threshold, lwd = 2)

# Generate sell and buy signals (Trading logic)
buys <- ifelse(data_out$spread > threshold, 1, 0) 
sells <- ifelse(data_out$spread < -threshold, -1, 0)
# Merge all signals in one sequence
data_out$signal <- buys + sells

plot(data_out$spread, main = "AAPL vs. SPY out of sample",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
abline(h = threshold, lty = 2) 
abline(h = -threshold, lty = 2)
point_type <- rep(NA, nrow(data_out)) 
buy_index <- which(data_out$signal == 1) 
sell_index <- which(data_out$signal == -1)
point_type[buy_index] <- 21 
point_type[sell_index] <- 24
points(data_out$spread, pch = point_type)

num_of_buy_signals <- sum(buys, na.rm = TRUE)
num_of_sell_signals <- sum(abs(sells), na.rm = TRUE)

# multiple time series in one plot in r
chart_Series(data_out$x, TA="add_TA(data_out$y, on=1)")
chart_Series(data_out$x, TA="add_TA(data_out$y, on=1);add_TA(data_out$spread/10, on=1)")

prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data_out$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_out$beta_out_of_sample)
qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for(i in 1:length(signal)) { 
  if(signal[i] == 1 && position == 0) {
    # buy the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- - prev_x_qty 
    qty_y[i] <- trade_size 
    position <- 1
  }
  if(signal[i] == -1 && position == 0) { 
    # sell the spread initially
    prev_x_qty <- round(beta[i]* trade_size)
    qty_x[i] <- prev_x_qty 
    qty_y[i] <- - trade_size 
    position <- -1
  }
  if(signal[i] == 1 && position == -1) {
     # we are short the spread and need to buy
    qty_x[i] <- -(round(beta[i] * trade_size) + prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- 2 * trade_size
    position <- 1
    }
  if(signal[i] == -1 && position == 1) {
    # we are long the spread and need to sell
    qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- - 2 * trade_size
    position <- - 1
  }
}
qty_x[length(qty_x)] <- -sum(qty_x) 
qty_y[length(qty_y)] <- -sum(qty_y)
data_out$qty_x <- qty_x
data_out$qty_y <- qty_y

chart_Series(data_out$qty_x, TA="add_TA(data_out$qty_y, on=1)")

# function for computing the equity curve 
compute_equity_curve <- function(qty, price) {
  cash_buy <- ifelse(sign(qty) == 1, qty * price, 0)
  cash_sell <- ifelse(sign(qty) == -1, -qty * price, 0)
  position <- cumsum(qty)
  cumulative_buy <- cumsum(cash_buy)
  cumulative_sell <- cumsum(cash_sell)
  equity <- cumulative_sell - cumulative_buy + position * price
  return(equity)
}

# Add the equity curve columns to the data_out table
data_out$equity_curve_x <- compute_equity_curve(
  data_out$qty_x, data_out$x) 
data_out$equity_curve_y <- compute_equity_curve(
  data_out$qty_y, data_out$y)

plot(data_out$equity_curve_x + data_out$equity_curve_y, 
     type = 'l',main = "AAPL / SPY spread", ylab = "P&L", 
     cex.main = 0.8,
     cex.axis = 0.8,
     cex.lab = 0.8)


## Consider risk
# Calculates the Sharpe ratio 
sharpe_ratio <- function(x, rf) {
  sharpe <- (mean(x, na.rm = TRUE) - rf) / sd(x, na.rm = TRUE)
  return(sharpe)
  }
# Calculates the maximum drawdown profile 
drawdown <- function(x) {
  cummax(x) - x 
  }

par(mfrow = c(2, 1))
equity_curve <- data_out$equity_curve_x + data_out$equity_curve_y
plot(equity_curve, main = "Equity Curve", cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
plot(drawdown(equity_curve), main = "Drawdown of equity curve", cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)

equity <- as.numeric(equity_curve[, 1])
equity_curve_returns <- diff(equity) #/ equity[-length(equity)]
# Remove any infinities and NaN
invalid_values <- is.infinite(equity_curve_returns) | is.nan(equity_curve_returns)
sharpe_ratio(equity_curve_returns[!invalid_values], 0.03)


# Find out where the trades occur
trade_dates <- data_out$qty_x[data_out$qty_x != 0]
# The trade_dates object is an xts object whose index 
# contains the necessary time information
duration <- as.numeric(diff(index(trade_dates)))
#How would a simple buy-and-hold strategy have performed over the same time frame?



### Charpter 7: Backtesting with Quantstrat