library(extraDistr)

set.seed(111)

n <- 100
t0 <- 0.35

data <-  rwald(n, mu=0.5, lambda=0.5) + t0
mean(data)
sd(data)

minus_loglik <- function(t, parms){
  
  z <- parms[1]
  v <- parms[2]
  t0 <- parms[3]
  
  -sum(log(z*(2*pi*(t-t0)^3)^(-0.5)*exp(-(v*(t-t0)-z)^2/2/(t-t0))))
  
  
}

init <- c(0.5, 0.5, 0.3)
fit <- optim(init, minus_loglik, t=data, control=list(maxit=10000, trace=1))
fit$par
fit$convergence
