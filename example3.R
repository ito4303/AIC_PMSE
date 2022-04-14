library(MASS)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

R <- 100000
N <- 100

set.seed(123)

data <- lapply(1:R, function(i) {
  x <- mvrnorm(N, c(0, 0), matrix(c(1, 2, 2, 9), 2, 2))
  y <- x[, 1] + 0.3 * x[, 2] + rnorm(N, 0, 4)
  data.frame(x1 = x[, 1], x2 = x[, 2], y = y)
})

cl <- makeCluster(getOption("cl.cores", 2))
rlist <- clusterMap(cl, function(d) {
  m1 <- glm(y ~ x1, data = d)
  m2 <- glm(y ~ x1 + x2, data = d)
  confint_m1 <- confint(m1)
  confint_m2 <- confint(m2)
  
  c(AIC1 =  AIC(m1), AIC2 = AIC(m2),
    coef1 = coef(m1)[-1], coef2 = coef(m2)[-1],
    ci_m1_x1 = (confint_m1["x1", 1] < 1 & confint_m1["x1", 2] > 1),
    ci_m2_x1 = (confint_m2["x1", 1] < 1 & confint_m2["x1", 2] > 1))
}, data)
stopCluster(cl)

vars <- names(rlist[[1]])
results <- t(matrix(unlist(rlist), nrow = length(vars)))
colnames(results) <- vars

# Summary
apply(results, 2, summary)

# AIC
sum(results[, "AIC1"] > results[, "AIC2"]) / R

# Coefficients in the "best" model by AIC
## coef of x1
m1_x1 <- results[results[, "AIC1"] < results[, "AIC2"],
                 "coef1.x1"]
m2_x1 <- results[results[, "AIC1"] > results[, "AIC2"],
                 "coef2.x1"]
coef_x1 <- c(m1_x1, m2_x1)
summary(coef_x1)
hist(coef_x1)

## coef of x2
m1_x2 <- rep(0, sum(results[, "AIC1"] < results[, "AIC2"]))
m2_x2 <- results[results[, "AIC1"] > results[, "AIC2"],
                 "coef2.x2"]
coef_x2 <- c(m1_x2, m2_x2)
summary(coef_x2)
hist(coef_x2)

# proportion that 95% confidence interval covers the true value
ci_x1 <- c(results[results[, "AIC1"] < results[, "AIC2"],
                   "ci_m1_x1"],
           results[results[, "AIC1"] > results[, "AIC2"],
                   "ci_m2_x1"])
mean(ci_x1)
