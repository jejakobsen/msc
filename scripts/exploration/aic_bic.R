AIC.normalized <- function(ll, n.params, n.obs, p){
  # Compute and return normalized AIC
  AIC <- -2*(n.obs/(n.obs-p))*ll + 2*n.params
  return(AIC)
}

BIC.normalized <- function(ll, n.params, n.obs, p){
  # Compute and return normalized BIC
  BIC <- -2*(n.obs/(n.obs-p))*ll + n.params*log(n.obs-p)
  return(BIC)
}