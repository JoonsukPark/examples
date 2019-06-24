library(rstan)

X <- as.matrix(iris[,1:3])
y <- iris[,4]
lambda <- 3 # penalty

data <- list(X=X, y=y, n=nrow(X), lambda=lambda)

model <- '

  data {
    
    int n;
    matrix[n,3] X;
    real y[n];
    real lambda;

  }

  parameters {

    vector[3] beta;
    real intercept;
    real<lower=0> sigma2;

  }

  transformed parameters {

    real<lower=0> sigma;
    vector[n] mu;

    sigma = sqrt(sigma2);
    mu = intercept + X * beta;    
  
  }

  model {

    target += log(1/sigma2);
    target += double_exponential_lpdf(beta | 0, 2*sigma2/lambda);
    target += normal_lpdf(y | mu, sigma);

  }

'

# Bayesian maximum a posteriori (MAP) estimator

m <- stan_model(model_code = model)
f <- optimizing(m, data=data)
res <- f$par
res[c(4,1,2,3)]

# The usual LASSO estimator

Loss_l1 <- function(X, y, beta, lambda)  sum((y - cbind(1, X) %*% as.matrix(beta))^2) + lambda*sum(abs(beta[2:4]))
init <- rep(0, 4)
fit2 <- optim(init, Loss_l1, X=X, y=y, lambda=lambda)
fit2$par

# Fit the Bayesian LASSO model

fit <- stan(model_code = model, data=data, iter=1000, chains=4)
post <- extract(fit, par=c("intercept","beta","sigma2"))

# Posterior means

c(mean(post$intercept), colMeans(post$beta))

# Histograms

par(mfrow=c(2,2))
hist(post$beta[,1], breaks=50, prob=T, main="Sepal.Length.coef", xlab='coef')
hist(post$beta[,2], breaks=50, prob=T, main="Sepal.Width.coef", xlab='coef')
hist(post$beta[,3], breaks=50, prob=T, main="Petal.Length.coef", xlab='coef')
hist(post$sigma2, breaks=50, prob=T, main="Variance", xlab='variance')
