# R
# install the 'rstan' package before running

library(rstan)

# load data

data <- list(N = nrow(iris), 
             X = iris$Sepal.Length - mean(iris$Sepal.Length),
             M = iris$Sepal.Width - mean(iris$Sepal.Width),
             Y = iris$Petal.Length - mean(iris$Petal.Length))

# Specifying the model

model_code <- '

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

'

# fit the model

fit <- stan(model_code=model_code, data=data, chains=4, iter=500, warmup=250)
traceplot(fit)

# 95% credible interval

ab <- extract(fit, "ab")$ab
quantile(ab, c(.025, .975))
