###############################################################################
#' @title Construct a ESN based on coefficients
#' @param coeffs, a list of
#'        insize outsize,  input and output dimensions, should be equal
#'        ressize,  reservoir dimensions, related to the magnitude of train data
#'        a,  leaking rate ?... not used now
#'        disttype,  the distribution type of weights
#'        distmean, mean of the distribution
#'        distsd, sd of the distribution
#'        stable, stable threshold of the reservoir matrix, this value want to keep the reservoir stay at the critical state between stable and unstable. (1.25)
#'        initlen, the erased length from first observation when training
#'        trainlen, the length of train data
#'        testlen, the length of test data
#'        regulation, the regulation coefficient to prevent Generalization Problem
#'
#' @return a S3 class of initialized ESN
esn <- function(coeffs) {
  with(coeffs, {
    # Input matrix, rows number equal to [ressize], cols number equal to [1 + insize]
    Win <- matrix(random_number(ressize * (1 + insize), disttype, distmean, distsd), nrow = ressize, ncol = 1 + insize)
    # Reservoir matrix, squre matrix
    W <- matrix(random_number(ressize * ressize, disttype, distmean, distsd), nrow = ressize, ncol = ressize)
    # compute spectral radius, which is the largest eigenvalue when mean equal to 0
    lev = abs(eigen(W, only.values=TRUE)$values[1])
    W = W * stable / lev
    esn <- list(coeffs = coeffs, Win = Win, W = W)
    class(esn) <- 'esn'
    return(esn)
  })
}

# create a generic function
train <- function(x, ...) UseMethod("train", x)
train.default <- function(x, ...) "Unknown class"

#' @title  the train method of "esn" class, which is called by the generic function [train]
#' @param  esn, the S3 class of Echo State Network
#' @param  data, the train data, which is a matrix with [trainlen] rows and [insize] cols
#' @param bias, the bias data
#' @param esn, the trained ESN
train.esn <- function(esn, data, bias = NULL) {
  with(esn, {
    if (is.null(bias))
      bias <- matrix(1, nrow = coeffs$trainlen, ncol = 1)

    # allocate memory for the design (collected states) matrix
    X = matrix(0, 1 + coeffs$insize + coeffs$ressize, coeffs$trainlen - coeffs$initlen)
    # set the corresponding target matrix directly
    Yt = matrix(data[(coeffs$initLen + 2):(coeffs$trainLen + 1)], 1)

    # run the reservoir with the data and collect X
    x = rep(0, coeffs$ressize)  # initial state variables
    for (t in 1:coeffs$trainlen) {
      u = data[t, ]
      x = tanh( Win %*% rbind(bias[t], u) + W %*% x )
      if (t > coeffs$initlen)
        X[, t - coeffs$initlen] = rbind(bias[t], u, x)
    }

    # train the output
    reg = coeffs$regulation  # regularization coefficient
    X_T = t(X)
    Wout = Yt %*% X_T %*%
      solve( X %*% X_T + reg * diag(1 + coeffs$insize + coeffs$ressize) )
    #esn <- list(coeffs = coeffs, Win = Win, W = W, Wout = Wout)
    #class(esn) <- "esn"
    esn$Wout = Wout
    return(esn)
  })
}

# create a generic function
test <- function(x, ...) UseMethod("test", x)
test.default <- function(x, ...) "Unknown class"

test.esn <- function(esn, data, mode) {
  with(esn, {

  })
  Y = matrix(0, outSize, testLen)
  u = data[trainLen + 1]
  for (t in 1:testLen){
    x = (1 - a) * x + a * tanh( Win %*% rbind(1, u) + W %*% x )
    y = Wout %*% rbind(1, u, x)
    Y[, t] = y
    # generative mode:
    #u = y
    ## this would be a predictive mode:
    u = data[trainLen+t+1]
  }
}
