# selectiveInference package
# https://qiita.com/saltcooky/items/9b25e7540b7f7d7bc61f

library(MASS)
library(selectiveInference)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

R <- 10^5
N <- 100

set.seed(123)

data <- lapply(1:R, function(i) {
  x1 <- mvrnorm(N, mu = c(0, 0, 0),
                Sigma = matrix(c(1, 1, 1,
                                 1, 2, 2,
                                 1, 2, 9),
                               3, 3))
  x2 <- matrix(rnorm(7 * N, 0, 1), ncol = 7, nrow = N)
  y <- 2 * x1[, 1] + 1 * x1[, 2] + 0.5 * x1[, 3] + rnorm(N, 0, 4)
  list(x = cbind(x1, x2), y = y)
})

cl <- makeCluster(getOption("cl.cores", 2))
rlist <- clusterMap(cl, function(d) {
  fit <- selectiveInference::fs(d$x, d$y)
  out <- selectiveInference::fsInf(fit, type = "aic", alpha = 0.05)

  pv <- out$pv[match(4, out$vars)]
}, data)
stopCluster(cl)

pv <- unlist(rlist)
pv <- pv[!is.na(pv)]

hist(pv)
