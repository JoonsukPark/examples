library(extraDistr)

set.seed(111)

n <- 100
t0 <- 0.35

data <-  rwald(n, mu=0.5, lambda=0.5) + t0
mean(data)
sd(data)

loglik <- function(t, parms){
  
  z <- parms[1]
  v <- parms[2]
  t0 <- parms[3]
  
  sum(log(z*(2*pi*(t-t0)^3)^(-0.5)*exp(-(v*(t-t0)-z)^2/2/(t-t0))))
  
  
}

minus_loglik <- function(t, parms){
  
  z <- parms[1]
  v <- parms[2]
  t0 <- parms[3]
  
  -sum(log(z*(2*pi*(t-t0)^3)^(-0.5)*exp(-(v*(t-t0)-z)^2/2/(t-t0))))
  
  
}

init <- c(.5, 1, 0.4)
fit <- optim(init, minus_loglik, t=data, control=list(maxit=10000, trace=1), hessian=T)
fit$par
fit$convergence

# asymptotic covariance matrix of ML estimates

sqrt(diag(solve(fit$hessian)))

# MCMC sampler using Metropolis algorithm

metropolis <- function(t, parms, proposal_sd){
  
  # propose from normal

  proposal <- parms + rnorm(length(parms), 0, proposal_sd)
  
  # compute ratio
  
  num <- loglik(t, proposal)
  den <- loglik(t, parms)
    
  r <- exp(num-den)
  
  if(is.nan(r)) return(parms)
  
  else {

    if(r > 1) return(proposal)
    else {
      if(rbinom(1, 1, r) == 1) return(proposal)
      else return(parms)
    }
  }
}

# sample from posterior

proposal_sd <- 0.01
n_mcmc <- 100000
accept <- 0

post <- matrix(nrow=n_mcmc, ncol=3)

# use ML estimates as initial values

post[1,] <- fit$par

for(i in 2:(n_mcmc)){
  post[i,] <- metropolis(data, post[i-1,], proposal_sd)
  if(sum(post[i,] == post[i-1,]) < 3) accept <- accept + 1
}

# throw away pre-burn-in samples

post_burnin <- post[(n_mcmc/2+1):n_mcmc,]

# plot posteriors

par(mfrow=c(3,1))
for(i in 1:3){
  plot.ts(post_burnin[,i])
}

# compute posterior means and medians for model parameters

postmean <- colMeans(post_burnin)
postmedian <- apply(post_burnin, 2, median)

# estimates

ests <- cbind(postmean, postmedian, fit$par)
rownames(ests) <- c("z","v","t0")
colnames(ests) <- c("postmean","postmedian","ML")
round(ests, 3)

# posterior standard deviations

apply(post_burnin, 2, sd)

# 95% credible intervals

quantile(post_burnin[,1], c(.025, .975))
quantile(post_burnin[,2], c(.025, .975))
quantile(post_burnin[,3], c(.025, .975))

# acceptance rate

accept/(n_mcmc-1)
