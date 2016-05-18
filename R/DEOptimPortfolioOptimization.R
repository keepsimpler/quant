# Portfolio Optimization using Differential Evolution
require(DEoptim)

# Drawdown function 
compute_drawdown <- function(x, returns_default = TRUE, geometric = TRUE) {
  # x = Vector of raw pnl(Profit & Lost) or returns
  # If returns_default = f, the geometric
  # argument is ignored and the pnl is used.
  # Output = the maximum drawdown
  
  if (returns_default) {
    # Cumulative return calculation
    if (geometric) {
      cumulative_return <- cumprod(1 + x)
    } else {
      cumulative_return <- 1 + cumsum(x)
    }
    max_cumulative_return <- cummax(c(1, cumulative_return))[-1]
    drawdown <- -(cumulative_return / max_cumulative_return - 1)
  } else {
    # PnL vector is used
    cumulative_pnl <- c(0, cumsum(x))
    drawdown <- cummax(cumulative_pnl) - cumulative_pnl 
    drawdown <- drawdown[-1]
  }
  # Drawdown vector for either pnl or returns
  return(drawdown)
}

# the Objective Function of Optimization
#
obj_max_drawdown <- function(w, r_matrix, small_weight) {
  # w is the weight of every stock
  # r_matrix is the returns matrix of all stocks
  
  # Portfolio return
  portfolio_return <- r_matrix %*% w
  
  # Max drawdown
  drawdown_penalty <- max(compute_drawdown(portfolio_return))
  
  # Create penalty component for sum of weights
  weight_penalty <- 100 * (1 - sum(w)) ^ 2
  
  # Create a penalty component for negative weights
  negative_penalty <- -sum(w[w < 0])
  
  # create penalty component for small weights
  small_weight_penalty <- 100 * sum(w[w < small_weight])
  
  # Objective function to minimize
  obj <- drawdown_penalty + weight_penalty +
    negative_penalty + small_weight_penalty
}

# Calculate a returns matrix for multiple stocks 
symbol_names <- c("AXP", "BA", "CAT", "CVX", "DD", "DIS", "GE", "HD", "IBM", "INTC", "KO", "MMM", "MRK", "PG", "T", "UTX", "VZ")
getSymbols(symbol_names)

# Load these prices into memory
price_matrix <- NULL
for (name in symbol_names) {
  # Extract the adjusted close price vector
  price_matrix <- cbind(price_matrix, get(name)[, 6])
}
colnames(price_matrix) <- symbol_names

# Compute returns
returns_matrix <- apply(price_matrix, 2, function(x) diff(log(x)))

# Specify a small weight below which the allocation should be 0%
small_weight_value <- 0.02

# Specify lower and upper bounds for the weights
lower <- rep(0, ncol(returns_matrix))
upper <- rep(1, ncol(returns_matrix))

optim_result <- DEoptim(obj_max_drawdown, lower, upper,
                control = list(NP = 400, itermax = 300, F = 0.25, CR = 0.75),
                returns_matrix, small_weight_value)

weights <- optim_result$optim$bestmem
sum(weights)

