library(rstan)
library(bayesmeta)

data(Rubin1981)

data <- list(N=8, effect=Rubin1981$effect, se=Rubin1981$stderr)

model_8schools_informative <- '

  data {

    int N;
    vector[N] effect;
    vector[N] se;
  }

  parameters {

    real<lower=0> tau2;
    real mu_effect;
    vector[N] effect_true;

  }

  transformed parameters {

    real<lower=0> tau;
    tau = sqrt(tau2);

  }

  model {

    tau2 ~ gamma(1, 0.111111);
  
    effect_true ~ normal(mu_effect, tau);
    effect ~ normal(effect_true, se);
    
  }
'

model_8schools_diffuse <- '

  data {
  
    int N;
    vector[N] effect;
    vector[N] se;
  
  }
  
  parameters {
  
    real<lower=0> tau2;
    real mu_effect;
    vector[N] effect_true;
  
  }

  transformed parameters {

    real<lower=0> tau;
    tau = sqrt(tau2);

  }
  
  model {
  
    tau2 ~ gamma(.00001, .00001);
    effect_true ~ normal(mu_effect, tau);
    effect ~ normal(effect_true, se);
  
  }
'

n_mcmc <- 10000

fit_diffuse <- stan(data=data, model_code=model_8schools_diffuse, iter=n_mcmc)
traceplot(fit_diffuse)

fit_informative <- stan(data=data, model_code=model_8schools_informative, iter=n_mcmc)
traceplot(fit_informative)



