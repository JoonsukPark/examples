set.seed(111)

# settings

n <- 30
mu_true <- 100
sd_true <- 15
prior_mean <- 90
prior_sd <- 30

# ABC rejection sampler

sampler <- function(n, sd_true, prior_mean, prior_sd, mean_sample, eps){
  
  dist <- eps + 1
  while(dist > eps){
    mu_proposal <- rnorm(1, prior_mean, prior_sd)
    mean_sim <- mean(rnorm(n, mu_proposal, sd_true))
    dist <- abs(mean_sim - mean_sample)
  }
  mu_proposal
}

# generate data

data <- rnorm(n, mu_true, sd_true)
mean_sample <- mean(data)

mean_sample

# ABC sampling

eps <- 0.5 # threshold for acceptance
n_mcmc <- 10000
post <- vector(length=n_mcmc)

for(i in 1:n_mcmc){
  post[i] <- sampler(n, sd_true, prior_mean, prior_sd, mean_sample, eps)
}

# Posterior quantities computed from the sample

mean(post)
sd(post)

# Theoretical posterior quantities

post_prec <- 1/prior_sd^2 + n/sd_true^2
post_sd <- sqrt(1/post_prec)
post_mean <- (prior_mean/prior_sd^2 + mean_sample/(sd_true^2/n))/(1/prior_sd^2 + n/sd_true^2)

post_mean
post_sd

# Do they agree?

hist(post, breaks=30, prob=T, main='Posterior distribution of mu', xlab='mu', ylab='density',
     xlim=c(min(post)-2, max(post)+2))

curve(dnorm(x, post_mean, post_sd), lwd=2, lty=2, col='red', add=T)