# RPS

rps <- function(Y, params.1, param.2, pmf.fn, tol=1e-6){
  N <- length(Y)
  
  S <- 0
  for (i in 1:N){
    param.1.i <- params.1[i]
    Y.i <- Y[i]
    s <- 0 
    k <- 0
    f <- 0
    dummy <- TRUE
    while (dummy){
      f <- f + pmf.fn(k, param.1.i, param.2)
      if (abs(f-1)^2 <= tol){
        f <- 1
      }
      indicator <- ifelse(Y.i<=k, 1, 0)
      s <- s + (f-indicator)^2
      if (f == 1 & indicator == 1){
        dummy <- FALSE
      }
      k = k + 1
    }
    S <- S + s
  }
  return(S/N)
}

# QS

qs <- function(Y, params.1, param.2, pmf.fn, tol=1e-8){
  N <- length(Y)
  
  S <- 0
  for (i in 1:N){
    param.1.i <- params.1[i]
    Y.i <- Y[i]
    s <- 0
    s <- -2 * pmf.fn(Y.i, param.1.i, param.2)
    dummy <- TRUE
    f <- 0 
    k <- 0 
    while(dummy){
      p <- pmf.fn(k, param.1.i, param.2)
      f <- f + p
      p.sq <- p^2
      s <- s + p.sq
      if (f >= 0.99){
        if (p.sq <= tol){
          dummy <- FALSE
        }
      }
      k <- k + 1
    }
    S <- S + s
  }
  return(S/N)
}
