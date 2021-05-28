
fit_country <- function(country, nll.fn, transform.fn, inv.transform.fn,
                        params, N.iter, sigma, hess=FALSE){
  # Fits "country" according to "nll.fn" iteratively. The number of iterations are 
  # controlled by "N.iter". "params" are the start-values for "nll.fn" and will be 
  # transformed with "transform.fn" before entering "nll.fn" in the first iteration.
  # For each iteration, noise is added from N(0, "sigma") to every estimated parameter.
  # Then, the estimated parameters with noise are inversely transformed with 
  # "inv.transform.fn", and then transformed again with "transform.fn" when a new 
  # iteration starts. Furthermore, in each iteration the negative log-likelihood is 
  # stored in a vector, and the nlm-object in a list. When the number of iterations are 
  # done, the nlm-object with the lowest negative log-likelihood is returned by the 
  # function. If "hess"=TRUE, then the hessian will be available in the returned nlm-object.
  assign("data", read.csv(paste0("../../data/", country ,".csv")), envir = .GlobalEnv)
  
  fits <- vector(mode="list", length=N.iter)
  nlls <- rep(NA, N.iter)
  for (i in 1:N.iter){
    params.t <- transform.fn(params)
    nlm.obj <- nlm(f=nll.fn, 
                   p=params.t,
                   data=data, hessian=hess)
    fits[[i]] <- nlm.obj
    nlls[i] <- nlm.obj$minimum
    init.params <- inv.transform.fn(nlm.obj$estimate + rnorm(length(params), 0, sigma)) 
  }
  ind.best <- which.min(nlls)
  best.fit <- fits[[ind.best]]
  return(best.fit)
}