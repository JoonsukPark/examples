# Python

import numpy as np
from scipy.stats import invgauss, norm, binom
from scipy.optimize import minimize
from numpy import log, exp, apply_along_axis, mean, median, std, quantile
from math import pi, isnan
np.random.seed(111)

n=100
t0=0.35

t = invgauss.rvs(mu=0.5, scale=1, size=n)+t0

def minus_loglik(parms):
    
    z = parms[0]
    v = parms[1]
    t0 = parms[2]
    
    return -sum(log(z*(2*pi*(t-t0)**3)**(-0.5)*exp(-(v*(t-t0)-z)**2/2/(t-t0))))

init = [.5, 1.2, .3]
fit = minimize(minus_loglik, init, method="nelder-mead")
print(fit.x)

def loglik(parms):
    
    z = parms[0]
    v = parms[1]
    t0 = parms[2]
    
    return sum(log(z*(2*pi*(t-t0)**3)**(-0.5)*exp(-(v*(t-t0)-z)**2/2/(t-t0))))

def metropolis(t, parms, proposal_sd):

    # propose from normal

    proposal = parms + norm.rvs(loc=0, scale=proposal_sd, size=len(parms))

    # compute ratio

    num = loglik(proposal)
    den = loglik(parms)

    r = exp(num-den)

    if(isnan(r)):
        return(parms)

    else:
        if(r > 1):
            return(proposal)
        else:
            coin = binom.rvs(1, r, size=1)
            if(coin == 1):
                return(proposal)
            else:
                return(parms)
            
# sample from posterior

proposal_sd = 0.01
n_mcmc = 10000
accept = 0

post = np.zeros([n_mcmc, 3])

# use ML estimates as initial values

post[0,:] = fit.x

for i in range(n_mcmc-1):
    
    post[i+1,:] = metropolis(data, post[i,:], proposal_sd)
    if sum(post[i,:] == post[i-1,:]) < 3:
        accept += 1

# throw away pre-burn-in samples

post_burnin = post[int(n_mcmc/2):,]

import matplotlib.pyplot as plt
plt.plot(range(int(n_mcmc/2)), post_burnin[:,0])
plt.plot(range(int(n_mcmc/2)), post_burnin[:,1])
plt.plot(range(int(n_mcmc/2)), post_burnin[:,2])

# compute posterior means and medians for model parameters

postmean = apply_along_axis(mean, 0, post_burnin)
postmedian = apply_along_axis(median, 0, post_burnin)

print(apply_along_axis(std, 0, post_burnin))

# estimates

ests = [postmean, postmedian, fit.x]
print(np.round(ests, 3))

print(quantile(post_burnin[:,0], [.025, .975]))
print(quantile(post_burnin[:,1], [.025, .975]))
print(quantile(post_burnin[:,2], [.025, .975]))

print(accept/(n_mcmc-1))
