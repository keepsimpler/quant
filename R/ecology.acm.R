params_acm <- function(hybrid_graph, coeff, antago.symm = TRUE) {
  competitive_graph = hybrid_graph$competitive_graph
  antago_graph = hybrid_graph$antago_graph
  mutual_graph = hybrid_graph$mutual_graph
  n = dim(antago_graph)[1]  # number of species
  with(as.list(coeff), {
    r <- runif2(n, alpha.mu, alpha.sd)
    s <- runif2(n, beta0.mu, beta0.sd)
    Gamma = matrix(abs(rnorm(n * n, gamma.mu, gamma.sd)),
                   nrow = n, ncol = n)
    M = Gamma * mutual_graph  # mutualism interactions
    # Positive part of antagonism interactions
    antago_graph[antago_graph < 0] = 0
    AP = Gamma * antago_graph
    if (antago.symm == TRUE)
      AN = t(AP)
    else
      AN = Gamma * t(antago_graph)
    C = Gamma * competitive_graph
    H = matrix(runif2(n * n, h.mu, h.sd), nrow = n, ncol = n)
    G = matrix(runif2(n * n, g.mu, g.sd), nrow = n, ncol = n)
    E = matrix(runif2(n * n, e.mu, e.sd), nrow = n, ncol = n)
    list(r = r, s = s, M = M, AP = AP, AN = AN, C = C, H = H, G = G, E = E)
  })
}


# get intrinsic growth rates accroding to model parameters and species densities in  a steady state
get_intrinsic_growth_rates <- function(params, nstar) {
  with(as.list(params), {
    r <- s * nstar -  # intraspecies self-regulation
      ( (E * M) %*% nstar ) / (1 + (H * M) %*% nstar) -  # mutualism
      ( (G * AP) %*% nstar ) / (1 + (H * AP) %*% nstar) +  # positive part of antagonism
      (AN) %*% diag(nstar) %*% (1 / (1 + (H * AP) %*% nstar)) +  # negative part of antagonism t(AP)
      C %*% nstar
    return(r)
  })
}

#' @title get sum of resource species of species j which is consumer of species i
#' @param AN, the Antagonism Negative(species j is consumer of speices i) interactions
#' @param AP, the Antagonism Positive(species j is resource of species i) interactions
#' @param nstar, the densities of species in equilibrium
#' @return a matrix, its i-row j-col element is the sum of resource species of species j which is consumer of species i
GammaAN2 <- function(AN, AP, nstar) {
  n = dim(AP)[1]
  N2 = matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (AN[i, j] > 0) # j is the consumer of i
        N2[i, j] = sum(AP[j, ] * nstar) # sum of the resource species of j
    }
  }
  N2
}

#' @title get Jacobian matrix in equilibrium
#' @param coeff, coefficients
#' @param antago.symm, is it symmetric the antagonism interaction strengths
#' @param nstar, densities of species in equilibrium
get_jacobian <- function(coeff,  antago.symm = FALSE, nstar = NULL) {
  s = unlist(coeff['s'])
  if (is.null(nstar))
    nstar = rep(1, s) # assign species densities in equilibrium to 1
  hybrid_graph <- gen_hybrid_network(s, unlist(coeff['k']), pc = unlist(coeff['pc']), pa = unlist(coeff['pa']), pm = unlist(coeff['pm']))
  params <- params_acm(hybrid_graph, coeff, antago.symm = antago.symm)
  #params$r <- get_intrinsic_growth_rates(params, nstar)

  h = unlist(coeff['h.mu'])
  # diagonal elements of Jacobian matrix equal to ...
  AN2 <- GammaAN2(AN = params$AN, AP = params$AP, nstar = nstar)
  #GammaDh <- rowSums(h * params$AN * t(params$AP) / (1 + h * AN2)^2)
  GammaDh <- diag(nstar) %*% (h * params$AN * t(params$AP) / (1 + h * AN2)^2) %*% nstar
  GammaD <- as.numeric(- params$s * nstar + GammaDh)
  # non-diagonal elements of Jacobian matrix equal to ...
  e = unlist(coeff['e.mu'])
  g = unlist(coeff['g.mu'])
  #GammaM <- diag(1 / ((1  + h * rowSums(params$M))^2)) %*% (e * params$M)
  GammaM <- diag(nstar / ((1  + h * rowSums(params$M %*% diag(nstar)))^2)) %*% (e * params$M)
  #GammaAP <- diag(1 / ((1  + h * rowSums(params$AP))^2)) %*% (g * params$AP)
  GammaAP <- diag(nstar / ((1  + h * rowSums(params$AP %*% diag(nstar)))^2)) %*% (g * params$AP)
  #GammaAN <- - params$AN  / (1 + h * AN2)
  GammaAN <- - diag(nstar) %*% params$AN  / (1 + h * AN2)
  #GammaC <- - params$C
  GammaC <- - diag(nstar) %*% params$C
  Gamma <- GammaM + GammaAP + GammaAN + GammaC + diag(GammaD)
}

myfun3 <- function(coeff, antago.symm = FALSE, nstar = NULL) {
  print(coeff['id'])
  s = unlist(coeff['s'])
  if (is.null(nstar))
    nstar = rep(1, s) # runif(s, min = 0, max = 2)
  Phi <- get_jacobian(coeff = coeff, antago.symm = antago.symm, nstar = nstar)
  Eii <- mean(diag(Phi))  # mean of diagonal elements
  Vii <- var(diag(Phi))  # variance of diagonal elements
  Eij <- mean(Phi[row(Phi) != col(Phi)])  # mean of off-diagonal elements
  Phi2 <- Phi * Phi
  Eij2 <- mean(Phi2[row(Phi2) != col(Phi2)]) # mean of squre of off-diagonal elements
  Phi2 <- Phi * t(Phi)
  Eijji <- mean(Phi2[row(Phi2) != col(Phi2)]) # mean of multiplication of Eij and Eji
  eigenvalues = eigen(Phi)$values
  lev = max(Re(eigenvalues))

#   I = diag(1, s) # identity matrix
#   Vs = 1 / (2 * norm(- solve(kronecker(I, Phi) + kronecker(Phi, I)), type = '2'))
#   Vd = 1 / norm(- solve(Phi), type = '2')
#   R0 = - max(Re(eigen(Phi + t(Phi))$values)) / 2
  c(coeff = coeff, lev = lev, Eii = Eii, Vii = Vii, Eij = Eij, Eij2 = Eij2, Eijji = Eijji)  #  , Vd = Vd, Vs = Vs, R0 = R0
}

# Spectral Norm of matrix (we only consider real matrix here!)
spectral.norm <- function(A) {
  sqrt(max(Re(eigen(t(A) %*% A)$values)))
}

myfun4 <- function(coeff, antago.symm = FALSE) {
  print(coeff['id'])
  I = diag(1, coeff['s']) # identity matrix
  Phi <- get_jacobian(coeff = coeff, antago.symm = antago.symm)
  lev = - max(Re(eigen(Phi)$values))
  Vs = 1 / (2 * norm(- solve(kronecker(I, Phi) + kronecker(Phi, I)), type = '2'))
  Vd = 1 / norm(- solve(Phi), type = '2')
  R0 = - max(Re(eigen(Phi + t(Phi))$values)) / 2
  c(coeff = coeff, lev = lev, Vs = Vs, Vd = Vd, R0 = R0)
}

check_jacobian <- function(coeff, antago.symm = FALSE) {
  s = 100
  k = 4
  nstar = rep(1, s)
  hybrid_graph <- gen_hybrid_network(s, k, pc = coeff['pc'], pa = coeff['pa'], pm = coeff['pm'])
  params <- params_acm(hybrid_graph, coeff, antago.symm = antago.symm)
  params$r <- get_intrinsic_growth_rates(params, nstar)
  Phi = jacobian.full(y = nstar, func = model_acm, params = params) # community matrix, Jacobian matrix at equilibrium
  eigenvalues = eigen(Phi)$values
  lev = max(Re(eigenvalues))

  # check if the Jacobian matrix is expected
  h = unlist(coeff['h.mu'])
  # diagonal elements of Jacobian matrix equal to ...
  AN2 <- GammaAN2(AN = params$AN, AP = params$AP)
  GammaDh <- rowSums(h * params$AN * t(params$AP) / (1 + h * AN2)^2)
  GammaD <- - params$s + GammaDh
  all(round(diag(Phi),5) == round(GammaD,5))

  # non-diagonal elements of Jacobian matrix equal to ...
  e = unlist(coeff['e.mu'])
  g = unlist(coeff['g.mu'])

  GammaM <- diag(1 / ((1  + h * rowSums(params$M))^2)) %*% (e * params$M)
  all(round(Phi *sign(params$M),5) == round(GammaM,5))
  GammaAP <- diag(1 / ((1  + h * rowSums(params$AP))^2)) %*% (g * params$AP)
  all(round(Phi *sign(params$AP),5) == round(GammaAP,5))
  #GammaAN <- - t(params$AP)
  GammaAN <- - params$AN  / (1 + h * AN2)
  all(round(Phi * sign(params$AN),5) == round(GammaAN,5))
  GammaC <- - params$C
  all(round(Phi *sign(params$C),5) == round(- params$C,5))
  Gamma <- GammaM + GammaAP + GammaAN + GammaC + diag(GammaD)
  #stopifnot(all(round(Phi,5) == round(Gamma,5)))
  # if h > 0
}

