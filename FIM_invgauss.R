library(numDeriv)
library(statmod)

N <- 10000

mu <- 1
lambda <- 1.5
t0 <- 0.4

parms <- c(mu, lambda, t0)

t <- rinvgauss(N, mu, lambda) + t0

loglik <- function(t, parms){
  
  mu <- parms[1]
  lambda <- parms[2]
  t0 <- parms[3]
  
  sum(log())
  
}

hess <- function(t, parms) hessian(func=function(t, parms) log(dinvgauss(t-parms[3], parms[1], parms[2])), 
                                   x=parms, t=t)
  
res <- lapply(t, hess, parms=parms)
res2 <- matrix(0, nrow=3, ncol=3)

for(i in 1:N){
  
  res2 <- res2 + res[[i]]/N
    
}

FIM <- -res2
FIM
round(solve(FIM), 3)

round(cov2cor(solve(FIM)),3)

FIM_new <- FIM*200
sqrt(diag(round(solve(FIM_new), 3)))

# Verify this

N_data <- 200
N_sim <- 10000
ML_estimates <- matrix(nrow=N_sim, ncol=length(parms))

minus_loglik <- function(t, parms){
  
  mu <- parms[1]
  lambda <- parms[2]
  t0 <- parms[3]
  
  -sum(log(dinvgauss(t-t0, mu, lambda)))
  
}

for(i in 1:N_sim){

  t <- rinvgauss(N_data, mu, lambda) + t0  
  fit <- optim(parms, minus_loglik, t=t,
               control=list(maxit=10000), method="BFGS")
  ML_estimates[i,] <- fit$par
}


apply(ML_estimates, 2, sd)
sqrt(diag(solve(FIM))/N_data) # asymptotic standard error of ML estimator
