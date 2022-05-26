library(MASS)
library(AICcmodavg)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

R <- 10^5
N <- 100

set.seed(123)

data <- lapply(1:R, function(i) {
  x <- mvrnorm(N, c(0, 0), matrix(c(1, 1, 1, 9), 2, 2))
  y <- x[, 1] + 0.3 * x[, 2] + rnorm(N, 0, 4)
  data.frame(x1 = x[, 1], x2 = x[, 2], y = y)
})

plot(data[[1]]$x1, data[[1]]$x2)

cl <- makeCluster(getOption("cl.cores", 2))
rlist <- clusterMap(cl, function(d) {
  m1 <- lm(y ~ x1, data = d)
  m2 <- lm(y ~ x1 + x2, data = d)

  c(AIC1 =  AIC(m1), AIC2 = AIC(m2),
    AICc1 = AICcmodavg::AICc(m1), AICc2 = AICcmodavg::AICc(m2),
    BIC1 = BIC(m1), BIC2 = BIC(m2),
    coef1 = coef(m1)[-1], coef2 = coef(m2)[-1])
}, data)
stopCluster(cl)

vars <- names(rlist[[1]])
results <- t(matrix(unlist(rlist), nrow = length(vars)))
colnames(results) <- vars

# Summary
apply(results, 2, summary)

# AIC
sum(results[, "AIC1"] > results[, "AIC2"]) / R

# AICc
sum(results[, "AICc1"] > results[, "AICc2"]) / R

# BIC
sum(results[, "BIC1"] > results[, "BIC2"]) / R

# Coefficients in the "best" model by AIC
# in case model1 is selected
m1_x1 <- results[results[, "AIC1"] < results[, "AIC2"],
                 "coef1.x1"]
hist(m1_x1, main = "In case model1 is selected",
     xlab = "Coefficient of x1")
summary(m1_x1)

# in case model2 is selected
m2_x1 <- results[results[, "AIC1"] > results[, "AIC2"],
                 "coef2.x1"]
hist(m2_x1, main = "In case model2 is selected",
     xlab = "Coefficient of x1")
summary(m2_x1)

m2_x2 <- results[results[, "AIC1"] > results[, "AIC2"],
                 "coef2.x2"]
hist(m2_x2, main = "In case model2 is selected",
     xlab = "Coefficient of x2")
summary(m2_x2)

# unconditioned coefficient distribution
uc_x1 <- results[, "coef2.x1"]
hist(uc_x1, main = "Unconditioned distribution in model 2",
     xlab = "Coefficient of x1")
summary(uc_x1)

uc_x2 <- results[, "coef2.x2"]
hist(uc_x2, main = "Unconditioned distribution in model 2",
     xlab = "Coefficient of x2")
summary(uc_x2)
