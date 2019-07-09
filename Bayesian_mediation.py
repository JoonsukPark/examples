# Load libraries

import numpy as np
import pystan
from sklearn.datasets import load_iris

# Load data

iris = load_iris()

X = iris['data'][:,0]
M = iris['data'][:,1]
Y = iris['data'][:,2]

data = {'N' : len(X), 'X': X-np.mean(X), 'M': M-np.mean(M), 'Y': Y-np.mean(Y)}

# Stan code

model_code = """

  data {

    int N;
    vector[N] X;
    vector[N] M;
    vector[N] Y;
  }

  parameters {

    real a;
    real b;
    real c;
    real<lower=0> sigma2_M;
    real<lower=0> sigma2_Y;

  }

  transformed parameters {

    real ab;
    real<lower=0> sigma_M;
    real<lower=0> sigma_Y;

    ab = a*b;
    sigma_M = sqrt(sigma2_M);
    sigma_Y = sqrt(sigma2_Y);

  }

  model {
  
    a ~ normal(0, 1000);
    b ~ normal(0, 1000);
    c ~ normal(0, 1000);
    sigma2_M ~ gamma(.001, .001);
    sigma2_Y ~ gamma(.001, .001);

    M ~ normal(a*X, sigma_M);
    Y ~ normal(b*M + c*X, sigma_Y);
  }

"""

# Fit the model

model = pystan.StanModel(model_code=model_code)
fit = model.sampling(data=data, chains=4, iter=500, warmup=250)

# Credible interval for the indirect effect (ab)

ab = fit.extract(permuted=True)['ab']
np.quantile(ab, [.025, .975])

# Plot the results

fit.plot()
