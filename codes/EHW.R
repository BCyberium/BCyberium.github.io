remove(list = ls())
library(Matching)
library(car)
data("lalonde")

lalonde.lm <- lm(re78 ~ ., data = lalonde)
lalonde.coef <- summary(lalonde.lm)$coef
lalonde.hc0 <- sqrt(diag(hccm(lalonde.lm, type = "hc0")))
lalonde.hc1 <- sqrt(diag(hccm(lalonde.lm, type = "hc1")))
lalonde.hc2 <- sqrt(diag(hccm(lalonde.lm, type = "hc2")))
lalonde.hc3 <- sqrt(diag(hccm(lalonde.lm, type = "hc3")))
lalonde.hc4 <- sqrt(diag(hccm(lalonde.lm, type = "hc4")))

lalonde.t <- lalonde.coef[, 1] / cbind(lalonde.coef[, 2], lalonde.hc0, lalonde.hc1, lalonde.hc2, lalonde.hc3, lalonde.hc4)
colnames(lalonde.t) <- c("OLS", "hc0", "hc1", "hc2", "hc3", "hc4")
round(lalonde.t, 3)


library(mlbench)
data(BostonHousing)
boston.lm <- lm(medv ~., data = BostonHousing)
summary(boston.lm)


boston.coef <- summary(boston.lm)$coef
boston.hc0 <- sqrt(diag(hccm(boston.lm, type = "hc0")))
boston.hc1 <- sqrt(diag(hccm(boston.lm, type = "hc1")))
boston.hc2 <- sqrt(diag(hccm(boston.lm, type = "hc2")))
boston.hc3 <- sqrt(diag(hccm(boston.lm, type = "hc3")))
boston.hc4 <- sqrt(diag(hccm(boston.lm, type = "hc4")))

boston.t <- boston.coef[, 1] / cbind(
  boston.coef[, 2], 
  boston.hc0,  
  boston.hc1,       
  boston.hc2,
  boston.hc3,
  boston.hc4
)

colnames(boston.t) <- c("OLS", "hc0", "hc1", "hc2", "hc3", "hc4")
round(boston.t, 3)
