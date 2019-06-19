library(rstan)

# Data and penalty

X <- as.matrix(iris[,1:3])
y <- iris[,4]
lambda <- 0.2

# The usual Ridge estimator

L2_loss <- function(beta, lambda){
  mu <- beta[1] + X %*% beta[2:4]
  sum((y-mu)^2) + lambda*sum(beta[2:4]^2)
}

init <- rep(0, 4)
fit2 <- optim(init, L2_loss, lambda=lambda)
fit2$par # Ridge estimator

# Bayesian Ridge regression using Stan

data <- list(X=X, y=y, lambda=lambda, n=nrow(X))

model <- '

data {

int n;
matrix[n, 3] X;
vector[n] y;
real<lower=0> lambda;

}

parameters {

vector[3] beta;
real intercept;
real<lower=0> sigma2;

}

transformed parameters {

real<lower=0> sigma;
real<lower=0> sq_lambda;
vector[n] mu;

sigma = sqrt(sigma2);
sq_lambda = sqrt(lambda);
mu = intercept + X * beta;

}

model {

sigma2 ~ inv_gamma(1,1);
beta ~ normal(0, sigma/sq_lambda);
y ~ normal(mu, sigma);

}
'

n_iter <- 5000
fit <- stan(model_code = model, data=data, chains=4, iter=n_iter) # fit the Bayesian Ridge model
post <- extract(fit, pars=c("beta","sigma2","intercept"))
c(mean(post$intercept), colMeans(post$beta)) # Bayesian Ridge estimators
mean(post$sigma2) # nuisance parameter

quantile(post$beta[,1], c(.025, .975))
sd(post$beta[,1])

# Visualizing the posterior

par(mfrow=c(2,3))
hist(post$intercept, prob=T, breaks = 50, main="Intercept")
hist(post$beta[,1], prob=T, breaks = 50, main="Sepal.Length.coef")
hist(post$beta[,2], prob=T, breaks = 50, main="Sepal.Width.coef")
hist(post$beta[,3], prob=T, breaks = 50, main="Petal.Length.coef")
hist(post$sigma2, prob=T, breaks = 50, main="Variance")

# Posterior standard deviations

sd(post$intercept)
sd(post$beta[,1])
sd(post$beta[,2])
sd(post$beta[,3])
sd(post$sigma2)