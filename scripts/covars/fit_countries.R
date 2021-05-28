fit_countries <- function(nll.fn, data, params, N.iter, sigma, hess=FALSE){
  # Returns the best fit, based on the minimized negative log-likelihood, 
  # from iteratively fitting inputted function "nll.fn" with inputted "data".
  # In each iteration, the "nll.fn" is minimzed with nlm(). To start off the 
  # iterations, the inputted "params" is used. At the end of each iteration, 
  # noise is added to the estimated parameters from the nlm-object, according
  # to N(0, "sigma"^2). Here, "sigma" can either be a vector with the length of the 
  # parameters (see prep_sigmas_alt()), or a single number. We recommend the 
  # former choice. "N.iter" controls the number of iterations. If "hess"=TRUE, 
  # then the hessian is computed in nlm() in the last iteration. 
  fits <- vector(mode="list", length=N.iter)
  nlls <- rep(NA, N.iter)
  for (i in 1:N.iter){
    if (i < N.iter){
      nlm.obj <- nlm(f=nll.fn, 
                     p=params,
                     data=data) 
    }
    else {
      nlm.obj <- nlm(f=nll.fn, 
                     p=params,
                     data=data, hessian=hess)
    }
    fits[[i]] <- nlm.obj
    nlls[i] <- nlm.obj$minimum
    params <- nlm.obj$estimate + rnorm(length(params), 0, sigma)
  }
  ind.best <- which.min(nlls)
  print(nlls)
  best.fit <- fits[[ind.best]]
  return(best.fit)
}