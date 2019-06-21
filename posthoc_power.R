n <- 30
mu <- 0.5
n_sim <- 10000

# Note: "observed power" = "post hoc power"
# True power is about 0.75

power.t.test(n=n, delta=mu, sd=1, type='one.sample', alt='two.sided')$power

# Simulation

ind <- rep(FALSE, n_sim)
observed_power <- rep(0, n_sim)

for(i in 1:n_sim){
  
  data <- rnorm(n, mu, 1)
  es <- abs(mean(data)/sd(data))
  
  # Record the "observed power"
  
  observed_power[i] <- power.t.test(n=n, delta=es, sd=1, type='one.sample', alt='two.sided')$power

  # Was the null rejected?
  
  if(t.test(data, mu=0)$p.value > .05) ind[i] <- T
  
}

# Overall distribution of observed power

summary(observed_power)
sd(observed_power)

# Post hoc power from non-rejection cases

non_rejection <- observed_power[ind]
summary(non_rejection)
sd(non_rejection)

# Plots
  
par(mfrow=c(1,2))
hist(observed_power, prob=T, xlab='Post_hoc_power', main='Post_hoc_power (overall)')
hist(non_rejection, prob=T, xlab='Post_hoc_power', main='Post_hoc_power (non_rejection)') 
