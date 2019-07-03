X <- iris[,1]
M <- iris[,2]
Y <- iris[,3]

fit1 <- lm(M ~ X)
fit2 <- lm(Y ~ X + M)

a_hat <- coef(fit1)[2]
b_hat <- coef(fit2)[3]

a_hat
b_hat

mean <- a_hat*b_hat
mean

sigma2_a_hat <- vcov(fit1)[2,2]
sigma2_b_hat <- vcov(fit2)[3,3]

sigma2_a_hat
sigma2_b_hat

se_ab <- sqrt(a_hat^2*sigma2_b_hat + b_hat^2*sigma2_a_hat)
se_ab

c(mean-1.96*se_ab, mean+1.96*se_ab)

library(multilevel)
fit <- sobel(X, M, Y)
fit$Indirect.Effect
fit$SE
c(fit$Indirect.Effect-1.96*fit$SE, fit$Indirect.Effect+1.96*fit$SE)
