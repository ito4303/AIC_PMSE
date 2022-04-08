library(MASS)
library(AICcmodavg)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

# Number of replications
R <- 100000

# Sample size
N <- 50

set.seed(123)

data <- lapply(1:R, function(i) {
  x <- mvrnorm(N, c(0, 0), matrix(c(1, 0.7, 0.7, 1), 2, 2))
  y <- x[, 1] + rnorm(N, 0, 4)
  data.frame(x1 = x[, 1], x2 = x[, 2], y = y)
})

plot(data[[1]]$x1, data[[1]]$x2)

cl <- makeCluster(getOption("cl.cores", 2))
rlist <- clusterMap(cl, function(d) {
  m1 <- glm(y ~ x1, data = d)
  m2 <- glm(y ~ x1 + x2, data = d)
  
  c(AIC1 =  AIC(m1), AIC2 = AIC(m2),
    AICc1 = AICcmodavg::AICc(m1), AICc2 = AICcmodavg::AICc(m2),
    BIC1 =BIC(m1), BIC2 = BIC(m2),
    coef1 = coef(m1)[-1], coef2 = coef(m2)[-1])
}, data)
stopCluster(cl)

vars <- names(rlist[[1]])
results <- t(matrix(unlist(rlist), nrow = length(vars)))
colnames(results) <- vars

# Summary
apply(results, 2, summary)

# Coefficients
## model 1
m1_x1 <- results[, "coef1.x1"]
hist(m1_x1)
summary(m1_x1)
quantile(m1_x1, probs = c(0.05, 0.5, 0.95))

## model 2
m2_x1 <- results[, "coef2.x1"]
hist(m2_x1)
summary(m2_x1)
quantile(m2_x1, probs = c(0.05, 0.5, 0.95))

m2_x2 <- results[, "coef2.x2"]
hist(m2_x2)
summary(m2_x2)
quantile(m2_x2, probs = c(0.05, 0.5, 0.95))

# AIC
sum(results[, "AIC1"] < results[, "AIC2"]) / R

# AICc
sum(results[, "AICc1"] < results[, "AICc2"]) / R

# BIC
sum(results[, "BIC1"] < results[, "BIC2"]) / R

