# selectiveInference package
# https://github.com/selective-inference
# https://qiita.com/saltcooky/items/9b25e7540b7f7d7bc61f

library(MASS)
library(selectiveInference)
library(parallel)

# Set number of CPU cores
options(cl.cores = 8)

R <- 10^5  # Number of replications
N <- 100   # Sample size

set.seed(123)

data <- lapply(1:R, function(i) {
  x <- matrix(rnorm(N * 10), ncol = 10, nrow = N)
  y <- x[, 1] + x[, 2] + x[, 3] + rnorm(N, 0, 1)
  list(x = x, y = y)
})

cl <- makeCluster(getOption("cl.cores", 2))
rlist <- clusterMap(cl, function(d) {
  # stepAIC
  m <- MASS::stepAIC(lm(y ~ x[, 1] + x[, 2] + x[, 3] + x[, 4] + x[, 5] +
                          x[, 6] + x[, 7] + x[, 8] + x[, 9] + x[, 10],
                        data = d))
  # check if x[, 1] is included in the selected model
  if ("x[, 1]" %in% colnames(m$model)) {
    pv_step1 <- summary(m)$coefficients["x[, 1]", "Pr(>|t|)"]
  } else {
    pv_step1 <- NA
  }
  # check if x[, 4] is included in the selected model
  if ("x[, 4]" %in% colnames(m$model)) {
    pv_step4 <- summary(m)$coefficients["x[, 4]", "Pr(>|t|)"]
  } else {
    pv_step4 <- NA
  }

  # selectiveInference
  fit <- selectiveInference::fs(d$x, d$y)
  out <- selectiveInference::fsInf(fit, type = "aic", alpha = 0.05)
  pv_si1 <- out$pv[match(1, out$vars)]
  pv_si4 <- out$pv[match(4, out$vars)]

  # p-values for the coefficient of x[, 4]
  # in the selected models with x[, 4]
  c(pv_step1, pv_step4, pv_si1, pv_si4)
}, data)
stopCluster(cl)

pv <- matrix(unlist(rlist), ncol = 4, byrow = TRUE)
pv_step1 <- pv[, 1]
pv_step1 <- pv_step1[!is.na(pv_step1)]
pv_step4 <- pv[, 2]
pv_step4 <- pv_step4[!is.na(pv_step4)]
pv_si1 <- pv[, 3]
pv_si1 <- pv_si1[!is.na(pv_si1)]
pv_si4 <- pv[, 4]
pv_si4 <- pv_si4[!is.na(pv_si4)]

# 結果の比較

## stepAIC

### x1を含むモデルが選択された割合
length(pv_step1) / R

### 選択されたモデルでの、x1の係数のp値が0.05未満の割合
sum(pv_step1 < 0.05) / length(pv_step1)

### x4を含むモデルが選択された割合
length(pv_step4) / R

### 選択されたモデルでの、x4の係数のp値が0.05未満の割合
sum(pv_step4 < 0.05) / length(pv_step4)

### x4の係数のp値の頻度分布
hist(pv_step4, breaks = seq(0, 1, 0.025), xlim = c(0, 1))
abline(v = 0.05, col = "red", lty = 2)

## selectiveInference

### x1を含むモデルが選択された割合
length(pv_si1) / R

### 選択されたモデルでの、x1の係数のp値が0.05未満の割合
sum(pv_si1 < 0.05) / length(pv_si1)

### x4を含むモデルが選択された割合
length(pv_si4) / R

### 選択されたモデルでの、x4の係数のp値が0.05未満の割合
sum(pv_si4 < 0.05) / length(pv_si4)

### x4の係数のp値の頻度分布
hist(pv_si4, breaks = seq(0, 1, 0.025), xlim = c(0, 1))
abline(v = 0.05, col = "red", lty = 2)

