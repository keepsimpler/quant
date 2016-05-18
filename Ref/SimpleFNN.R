# https://www.linkedin.com/grp/post/4066593-5836473390680014850

require(quantmod)
require(nnet)
require(caret)

T = seq(0, 20, length = 200)
y = 1 + 3 * cos(4 * T + 2) + .2 * T^2 + rnorm(200)
dat <- data.frame(y, x1 = Lag(y, 1), x2 = Lag(y, 2))
dat[1,2] = dat[2,2] # replace NA with its post
dat[1,3] = dat[2,3] = dat[3,3]
names(dat) <- c('y', 'x1', 'x2')

# Fit model
model <- train(y ~ x1+x2 , 
               dat, 
               method='nnet', 
               linout=TRUE, 
               trace = FALSE)
ps <- predict(model, dat)

# Examine results
plot(T, y, type = 'l', col = 2)
lines(T[-c(1:2)], ps, col = 3)
legend(0, 70, c('y', 'pred'), cex = 1.5, fill = 2:3)
