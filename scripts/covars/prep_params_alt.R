prep_params_alt <- function(N.countries, beta.0, alpha.1, beta.1, m.1,
                        pi, gamma.0, gammas){
  # Returns a vector representing transformed parameters in a NB-INGARCH(1,1)  
  # for multiple countries with covariates. The user defines how many 
  # countries should be fitted (N.countries), and then for example the
  # parameter beta.0. This parameter will then be transformed and repeated
  # for the N.countries first elements in the returned vector. If the user 
  # define the parameter alpha.1 as well, then the next N.countries elements 
  # in the returned vector will be the transformed alpha parameter. This system 
  # works for all parameters in the input, except gamma.0 and gammas. For gamma.0, 
  # we have that this parameter is repeated N.countries times, but not tranformed.
  # For parameters gammas (representing global effects), we have that simply a vector 
  # can be inputted which is appended to the end of the returned vector. 
  params <- c()
  if (beta.0){
    beta.0s <- rep(beta.0, N.countries)
    beta.0.t <- - log(beta.0s)
    params <- c(params, beta.0.t)
  }
  if (alpha.1){
    alpha.1s <- rep(alpha.1, N.countries)
    alpha.1.t <- - log((1/alpha.1s)-1)
    params <- c(params, alpha.1.t)
  }
  if (beta.1){
    beta.1s <- rep(beta.1, N.countries)
    beta.1.t <- - log(((1-alpha.1s)/(beta.1s))-1)
    params <- c(params, beta.1.t)
  }
  if (m.1){
    m.1s <- rep(m.1, N.countries)
    m.1.t <- - log(m.1s)
    params <- c(params, m.1.t)
  }
  if (pi){
    pis <- rep(pi, N.countries)
    pi.t <- - log((1/pis)-1)
    params <- c(params, pi.t)
  }
  if (gamma.0){
    gamma.0s <- rep(gamma.0, N.countries)
    params <- c(params, gamma.0s)
  }
  if (gammas){
    params <- c(params, gammas)
  }
  return(params)
}

prep_sigmas_alt <- function(N.countries, sig.beta.0, sig.alpha.1,
                            sig.beta.1, sig.m.1, sig.pi,
                            sig.gamma.0, sig.gammas){
  # Returns a vector representing the size of sigma in N(0, sigma^2), 
  # which is used to draw noise from, to the estimated transformed parameters
  # when iteratively fitting in fit_countries(). This function works in the same way as 
  # prep_params_alt(), but does not involve transforming inputs.  
  # This function should be used in combination with prep_params_alt(), such that 
  # there is correspondence with the returned vector in this function and returned 
  # vector from prep_params_alt(). 
  sigmas <- c()
  if (sig.beta.0){
    sigmas <- c(sigmas, rep(sig.beta.0, N.countries))
  }
  if (sig.alpha.1){
    sigmas <- c(sigmas, rep(sig.alpha.1, N.countries))
  }
  if (sig.beta.1){
    sigmas <- c(sigmas, rep(sig.beta.1, N.countries))
  }
  if (sig.m.1){
    sigmas <- c(sigmas, rep(sig.m.1, N.countries))
  }
  if (sig.pi){
    sigmas <- c(sigmas, rep(sig.pi, N.countries))
  }
  if (sig.gamma.0){
    sigmas <- c(sigmas, rep(sig.gamma.0, N.countries))
  }
  if (sig.gammas){
    sigmas <- c(sigmas, sig.gammas)
  }
  return(sigmas)
}
