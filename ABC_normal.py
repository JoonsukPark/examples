from numpy.random import normal
from numpy import mean, std

# settings

n = 30
mu_true = 100.0
sd_true = 15.0
prior_mean = 90.0
prior_sd = 30.0

# ABC rejection sampler

def sampler(n, sd_true, prior_mean, prior_sd, mean_sample, eps):
  
    dist = eps + 1
    
    while dist > eps:
    
        mu_proposal = normal(prior_mean, prior_sd, 1)
        mean_sim = mean(normal(mu_proposal, sd_true, n))
        dist = abs(mean_sim - mean_sample)
    
    return(mu_proposal)

# generate data

data = normal(mu_true, sd_true, n)
mean_sample = mean(data)

print(mean_sample)

# ABC sampling

eps = 0.5 # threshold for acceptance
n_mcmc = 10000
post = []

for i in range(n_mcmc):
    post.extend(sampler(n, sd_true, prior_mean, prior_sd, mean_sample, eps))

# Posterior quantities computed from the sample

print(mean(post))
print(std(post))

# Theoretical posterior quantities

post_prec = 1/prior_sd**2 + n/sd_true**2
post_sd = (1/post_prec)**(0.5)
post_mean = (prior_mean/prior_sd**2 + mean_sample/(sd_true**2/n))/post_prec

print(post_mean)
print(post_sd)
