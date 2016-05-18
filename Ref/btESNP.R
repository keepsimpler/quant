# BackTesting of using ESN as indicator for portfolio

require(quantstrat)
require(PerformanceAnalytics)

from = '2007-01-01'
to <- Sys.Date()

shaIndex50 <- read.csv('sha-index50.txt', sep = ' ', header = F)
colnames(shaIndex50) <- c('stockName', 'symbol')
# remove everything that is not a digits
shaIndex50$symbol <- gsub("[^0-9]", "", shaIndex50$symbol)
# add suffix .SS to be recognized by google/yahoo
shaIndex50$symbol <- paste(shaIndex50$symbol, '.SS', sep = '')

if (! '600000.SS' %in% ls()) {
  #If data is not present, get it from yahoo
  suppressMessages(getSymbols(shaIndex50$symbol, from = from,
                              to = to, src = 'yahoo', adjust = TRUE))
}

# get sha index data
suppressMessages(getSymbols('000001.SS', from = from,
                            to = to, src = 'yahoo', adjust = TRUE))


# get statistics of a xts object
ts1 = `000001.SS`
prices <- ts1$'000001.SS.Adjusted'
meanPrices <- round(mean(prices), digits = 2)
sdPrices <- round(sd(prices), digits = 2)
# Plot the histogram along with a legend
hist(prices, breaks = 100, prob = T, cex.main = 0.9)
abline(v = meanPrices, lwd = 2)
legend("topright", cex = 0.8, bty = "n",
       paste("mean=", meanPrices, "; sd=", sdPrices))


plot_4_ranges <- function(data, start_date, end_date, title)
{
  # Set the plot window to be 2 rows and 2 columns 
  par(mfrow = c(2, 2))
  for(i in 1:4) {
    # Create a string with the appropriate date range
    range <- paste(start_date[i], "::", end_date[i], sep = "")
    # Create the price vector and necessary statistics 
    time_series <- data[range]
    mean_data <- round(mean(time_series, na.rm = TRUE), 3) 
    sd_data <- round(sd(time_series, na.rm = TRUE), 3)
    # Plot the histogram along with a legend 
    hist_title <- paste(title, range)
    hist(time_series, breaks = 100, prob=TRUE,
         xlab = "", main = hist_title, cex.main = 0.8)
    legend("topright", cex = 0.7, bty = 'n', 
           paste("mean=",mean_data, "; sd=", sd_data))
  }
  # Reset the plot window
  par(mfrow = c(1, 1)) 
}

# Define start and end dates of interest 
begin_dates <- c("2007-01-01", "2008-06-06",
                 "2009-10-10", "2011-03-03")
end_dates <- c("2008-06-05", "2009-09-09",
               "2010-12-30", "2013-01-06")
# Create plots
plot_4_ranges(prices, begin_dates,
              end_dates, "SHA Index for:")


# Compute log returns
returns <- diff(log(prices))
# Use the same function as before to plot returns rather than prices
plot_4_ranges(returns, begin_dates, end_dates, "SHA Index for:")

# Determining stationarity with urca 
require(urca)
test <- ur.kpss(as.numeric(prices))
# The output is an S4 object 
class(test)
# Extract the test statistic 
test@teststat
# Look at the critical values
test@cval
# Test on the returns
test_returns <- ur.kpss(as.numeric(returns))
test_returns@teststat
test_returns@cval
test_post_2013 <- ur.kpss(as.numeric(returns['2013::']))
test_post_2013@teststat

#### Assumption of Normality
# Plot histogram and density
mu <- mean(returns, na.rm = TRUE)
sigma <- sd(returns, na.rm = TRUE)
x <- seq(-5 * sigma, 5 * sigma, length = nrow(returns))
hist(returns, breaks = 100,
     main = "Histogram of returns for SHA Index", cex.main = 0.8, prob=TRUE)
lines(x, dnorm(x, mu, sigma), col = "red", lwd = 2)

# Set plotting window 
par(mfrow = c(1, 2))
# SPY data 
qqnorm(as.numeric(returns),
       main = "SHA Index empirical returns qqplot()",
       cex.main = 0.8)
qqline(as.numeric(returns), lwd = 2)
grid()
# Normal random data
normal_data <- rnorm(nrow(returns), mean = mu, sd = sigma)
qqnorm(normal_data, main = "Normal returns", cex.main = 0.8)
qqline(normal_data, lwd = 2)
grid()
answer <- shapiro.test(as.numeric(returns))
answer[[2]]
# [1] 1.388211e-41
# it's unlikely that our data originated from a underlying Normal distribution

# One Outlier CAN deviate heavily from Normal distribution
set.seed(129)
normal_numbers <- rnorm(5000, 0, 1) 
ans <- shapiro.test(normal_numbers)
ans[[2]]
## [1] 0.9963835
# Corrupt a single data point
normal_numbers[50] <- 1000
ans <- shapiro.test(normal_numbers)
ans[[2]]
## [1] 1.775666e-95

### Correlation
pufaBank <- `600000.SS`
gongshangBank <- `601398.SS`
pricesPair <- cbind(Cl(pufaBank),Cl(gongshangBank))
colnames(pricesPair) <- c('pudongBank.Close', 'gongshangBank.Close')
var(pricesPair, na.rm = T)
cor(pricesPair, use = 'complete.obs')
##                       pudongBank.Close gongshangBank.Close
#pudongBank.Close           1.0000000           0.8735991
#gongshangBank.Close        0.8735991           1.0000000
pricesPair[is.na(pricesPair$gongshangBank.Close),] # Check NA values!
# Check Outliers!
plot(as.numeric(pricesPair$pudongBank), as.numeric(pricesPair$gongshangBank))

# Create a linear regression object
reg <- lm(gongshangBank.Close ~ pudongBank.Close, data = pricesPair)
summary(reg)
b <- reg$coefficients[1] 
a <- reg$coefficients[2]

#par(mfrow = c(1, 1))
plot(reg$residuals,
     main = "Residuals through time",
     xlab = "Days", ylab = "Residuals") 
hist(reg$residuals, breaks = 100, 
     main = "Distribution of residuals",xlab = "Residuals")
qqnorm(reg$residuals)
qqline(reg$residuals)
# Strong Autocorrelation among different time lags of residuals
# indicate the linear relation between prices of pudong and gongshang
# is not right!
# there are nonlinear relationship which are thrown in and reflected in residuals!
acf(reg$residuals, main = "Autocorrelation")
#par(mfrow = c(1, 1))
acf(pricesPair$gongshangBank.Close, na.action = na.pass)
ccf(as.numeric(pricesPair$gongshangBank.Close), as.numeric(pricesPair$pudongBank.Close), na.action = na.pass)

returnsPair <- diff(log(pricesPair))
ccf(as.numeric(returnsPair$gongshangBank.Close), as.numeric(returnsPair$pudongBank.Close), na.action = na.pass)
acf(returnsPair$gongshangBank.Close, na.action = na.pass)


### Volatility
# Generate 1000 IID numbers from a normal distribution.
z <- rnorm(1000, 0, 1)
# Autocorrelation of returns and squared returns 
acf(z, main = "returns", cex.main = 0.8,
    cex.lab = 0.8, cex.axis = 0.8)
grid()
acf(z ^ 2, main = "returns squared", cex.lab = 0.8, cex.axis = 0.8)
grid()

acf(abs(returnsPair$gongshangBank.Close), main = "Actual returns squared", na.action = na.pass, cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8) 
grid()

pufaBank <- getSymbols('600000.SS', from = '2006-01-01', to = Sys.Date(),adjust = T, auto.assign = F)
huaxiaBank <- getSymbols('600015.SS', from = '2006-01-01', to = Sys.Date(), src = 'yahoo', adjust = T, auto.assign = F)
minshengBank <- getSymbols('600016.SS', from = '2006-01-01', to = Sys.Date(), adjust = T, auto.assign = F)
zhongxinSecurities <- getSymbols('600030.SS', from = '2006-01-01', to = Sys.Date(), adjust = T, auto.assign = F)
zhaoshangBank <- getSymbols('600036.SS', from = '2006-01-01', to = Sys.Date(), adjust = T, auto.assign = F)
xingyeBank <- getSymbols('601166.SS', from = '2006-01-01', to = Sys.Date(), adjust = T, auto.assign = F)

# trading days that huaxiaBank has and pufaBank hasn't, are removed
huaxiaBank[setdiff(as.character(index(huaxiaBank)),as.character(index(pufaBank)))]
huaxiaBank = huaxiaBank[!(index(huaxiaBank) %in% as.Date(setdiff(as.character(index(huaxiaBank)),as.character(index(pufaBank)))))]
#huaxiaBank[huaxiaBank[index(huaxiaBank) %in% (as.Date(setdiff(as.character(index(huaxiaBank)),as.character(index(pufaBank))))), which.i=TRUE] - 1]
# trading days that pufaBank has and huaxiaBank hasn't, are removed
pufaBank[setdiff(as.character(index(pufaBank)),as.character(index(huaxiaBank)))]
pufaBank = pufaBank[!(index(pufaBank) %in% as.Date(setdiff(as.character(index(pufaBank)),as.character(index(huaxiaBank)))))]

#pricesPufaBank <- Cl(pufaBank)
#pricesHuaxiaBank <- Cl(huaxiaBank)

returnsPufaBank <- diff(as.numeric(Cl(pufaBank)))
returnsHuaxiaBank <- diff(as.numeric(Cl(huaxiaBank)))

plot(returnsPufaBank, returnsHuaxiaBank, main = "Scatter plot of returns. PufaBank vs. HuaxiaBank", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
# Ordinary least squares distance minimization
abline(lm(returnsHuaxiaBank ~ returnsPufaBank)) 
abline(lm(returnsPufaBank ~ returnsHuaxiaBank), lty = 2) 
grid()
# Total least squares regression
r <- prcomp(~ returnsPufaBank + returnsHuaxiaBank)
slope <- r$rotation[2, 1] / r$rotation[1, 1]
intercept <- r$center[2] - slope * r$center[1]
# Show the first principal component on the plot
abline(a = intercept, b = slope, lty = 3)
