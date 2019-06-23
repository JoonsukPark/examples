n <- 10
p <- c(0.6, 0.5)

n_data <- 10000
coin <- rbinom(n_data, 1, 0.5)
ps <- coin*0.8 + (1-coin)*0.45
y <- rbinom(n_data, n, ps)

iter <- 0
dif <- 1000
eps <- 1e-10

E <- function(p, weights) -sum(weights*dbinom(y, n, p[1], log=T) + (1-weights)*dbinom(y, n, p[2], log=T))

M <- function(p, weights) optim(p, E, weights=weights, lower=c(1e-10, 1e-10), upper=c(1-(1e-10), 1-(1e-10)),
                                method='L-BFGS-B')$par

while(dif > eps){
  
  weights <- dbinom(y, n, p[1]) / (dbinom(y, n, p[1]) + dbinom(y, n, p[2]))
  temp <- M(p, weights)
  weights_temp <- dbinom(y, n, temp[1]) / (dbinom(y, n, p[1]) + dbinom(y, n, temp[2]))
  dif <- abs(E(p, weights)-E(temp, weights_temp))
  p <- temp
  iter <- iter + 1
}

p
iter
mean(weights)
