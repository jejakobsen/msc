prep_data <- function(countries){
  # Returns a list containing battle deaths and standardized covariates of
  # inputted countries. In this list we have the vector Y - containing 
  # battle deaths from one country, then another country and so on. We also
  # have the matrix X in the returned list, containing covariates of one country
  # after the other - just like with Y. However, this matrix have 4 columns, where 
  # each column represents v2x_polyarchy, gdp_pc, pop_tot and year. This matrix is 
  # standardized globally for each covariate. Lastly, we have inds.start and inds.stop
  # in the returned list - helping us to extract country specific battle deaths and 
  # covariates later on. 
  
  N.countries <- length(countries) # Number of countries
  
  # Get data and store in list
  data_ls <- vector(mode="list", length=N.countries)
  for (i in 1:N.countries){
    data_ls[[i]] <- read.csv(paste0("../../data/", countries[i],".csv"))
  }
  
  N.country <- length(data_ls[[1]]$battle_deaths) # Number of rows in each dataset
  N.params <- 4 # Number of parameters 
  
  # Initialize vector and matrix
  Y <- rep(NA, N.countries*N.country)
  X <- matrix(NA, ncol=N.params, nrow=N.country*N.countries)
  
  # Put data in Y vector and X matrix
  inds.start <- rep(NA, N.countries)
  inds.stop <- rep(NA, N.countries)
  ind.start <- 1
  for (i in 1:N.countries){
    ind.stop <- N.country*i
    Y[ind.start:ind.stop] <- data_ls[[i]]$battle_deaths
    X[ind.start:ind.stop, 1] <- data_ls[[i]]$v2x_polyarchy
    X[ind.start:ind.stop, 2] <- log(data_ls[[i]]$gdp_pc)
    X[ind.start:ind.stop, 3] <- log(data_ls[[i]]$pop_tot)
    X[ind.start:ind.stop, 4] <- data_ls[[i]]$year
    
    inds.start[i] <- ind.start
    inds.stop[i] <- ind.stop
    ind.start <- ind.stop + 1 
  }
  # Standardize columns in X matrix. 
  for (j in 1:N.params){
    X[, j] <- (X[, j] - mean(X[, j]))/sd(X[, j])
  }
  
  new_data_ls <- list(Y, X, inds.start, inds.stop)
  return(new_data_ls)
} 