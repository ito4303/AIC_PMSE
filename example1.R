# cf. Kuchibhotla et al. (2022)
# https://doi.org/10.1146/annurev-statistics-100421-044639

library(MASS)
library(AICcmodavg)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

# Number of replications
R <- 10^4

# Sample size
N <- 1000

set.seed(123)

data <- lapply(1:R, function(i) {
  x <- mvrnorm(N, c(0, 0, 0),
               matrix(c(1, 0, 0,
                        0, 1, 0,
                        0, 0, 1),
                      3, 3))
  y <- rnorm(N, 0, 3)
  data.frame(x1 = x[, 1], x2 = x[, 2], x3 = x[, 3], y = y)
})

cl <- makeCluster(getOption("cl.cores", 2))
rlist <- clusterMap(cl, function(d) {
  m <- list()
  m[[1]] <- lm(y ~ 1, data = d)
  m[[2]] <- lm(y ~ x1, data = d)
  m[[3]] <- lm(y ~ x2, data = d)
  m[[4]] <- lm(y ~ x3, data = d)
  m[[5]] <- lm(y ~ x1 + x2, data = d)
  m[[6]] <- lm(y ~ x1 + x3, data = d)
  m[[7]] <- lm(y ~ x2 + x3, data = d)
  m[[8]] <- lm(y ~ x1 + x2 + x3, data = d)
  
  aic <- sapply(1:8, function(i) AIC(m[[i]]))
  aicc <- sapply(1:8, function(i) AICcmodavg::AICc(m[[i]]))
  bic <- sapply(1:8, function(i) BIC(m[[i]]))
  coef <- sapply(1:8, function(i) coef(m[[i]]))

  # coefficient of x1 and its p-value in the smallest-AIC model
  min_aic <- which.min(aic)
  if (min_aic %in% c(2, 5, 6, 8)) {
    s <- summary(m[[min_aic]])
    coef_x1 <- s$coefficients["x1", "Estimate"]
    pv_x1 <- s$coefficients["x1", "Pr(>|t|)"]
  } else {
    coef_x1 <- pv_x1 <- NA
  }
  list(AIC = aic, AICc = aicc, BIC = bic, coef_x1 = coef_x1, pv_x1 = pv_x1)
}, data)
stopCluster(cl)

# smallest-AIC model
min_aic <- sapply(1:R, function(i) which.min(rlist[[i]]$AIC))
summary(factor(min_aic))

# coefficient of x1 in the smallest-AIC models which contain x1
coef_x1 <- sapply(1:R, function(i) {
         rlist[[i]]$coef_x1
  })
coef_x1 <- coef_x1[!is.na(coef_x1)]
hist(coef_x1, main = "In the \"best\" models which contain x1",
     xlab = "Coefficient of x1")

# p-value for x1 in the smallest-AIC models which contain x1
pv_x1 <- sapply(1:R, function(i) {
  rlist[[i]]$pv_x1
})
pv_x1 <- pv_x1[!is.na(pv_x1)]
hist(pv_x1, main = "In the \"best\" models which contain x1",
     xlab = "p-value of the coefficientnt of x1",
     xlim = c(0, 1), breaks = seq(0, 1, 0.01))
abline(v = 0.05, col = "red", lty = 2)
text(0.6, 800,
     paste("Prop. p < 0.05:", round(sum(pv_x1 < 0.05) / length(pv_x1), 2)),
     cex = 2)

# smallest-AICc model
min_aicc <- sapply(1:R, function(i) which.min(rlist[[i]]$AICc))
summary(factor(min_aicc))

# smallest-BIC model
min_bic <- sapply(1:R, function(i) which.min(rlist[[i]]$BIC))
summary(factor(min_bic))
