# cf. Kuchibhotla et al. (2022)
# https://doi.org/10.1146/annurev-statistics-100421-044639

library(MASS)
library(AICcmodavg)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

# Number of replications
R <- 10000

# Sample size
N <- 50

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
  m[[1]] <- glm(y ~ 1, data = d)
  m[[2]] <- glm(y ~ x1, data = d)
  m[[3]] <- glm(y ~ x2, data = d)
  m[[4]] <- glm(y ~ x3, data = d)
  m[[5]] <- glm(y ~ x1 + x2, data = d)
  m[[6]] <- glm(y ~ x1 + x3, data = d)
  m[[7]] <- glm(y ~ x2 + x3, data = d)
  m[[8]] <- glm(y ~ x1 + x2 + x3, data = d)
  
  aic <- sapply(1:8, function(i) AIC(m[[i]]))
  aicc <- sapply(1:8, function(i) AICcmodavg::AICc(m[[i]]))
  bic <- sapply(1:8, function(i) BIC(m[[i]]))
  coef <- sapply(1:8, function(i) coef(m[[i]]))
  list(AIC = aic, AICc = aicc, BIC = bic, coef = coef)
}, data)
stopCluster(cl)

# smallest-AIC model
min_aic <- sapply(1:R, function(i) which.min(rlist[[i]]$AIC))
summary(factor(min_aic))

# coefficient of x1 in the smallest-AIC models which contain x1
coef_x1 <- sapply(1:R, function(i) {
  ifelse(min_aic[i] %in% c(2, 5, 6, 8),
         rlist[[i]]$coef[[min_aic[i]]]["x1"],
         as.numeric(NA))
  })
coef_x1 <- coef_x1[!is.na(coef_x1)]
hist(coef_x1, main = "In the \"best\" models which contain x1",
     xlab = "Coefficient of x1")

# smallest-AICc model
min_aicc <- sapply(1:R, function(i) which.min(rlist[[i]]$AICc))
summary(factor(min_aicc))

# smallest-BIC model
min_bic <- sapply(1:R, function(i) which.min(rlist[[i]]$BIC))
summary(factor(min_bic))


