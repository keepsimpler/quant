######### Book: Data mining with R - Learning with Case Studies

##### Charpter 3 - Predicting stock market returns

# 3.2.1 - Handling time-dependent Data
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
