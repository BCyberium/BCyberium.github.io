library(HistData)
hist(GaltonFamilies$childHeight)
hist(GaltonFamilies$midparentHeight)
galton.lm <- lm(childHeight ~ midparentHeight, data = GaltonFamilies)
round(summary(galton.lm)$coef, 3)

mph.new <- seq(60, 90, by = 0.5)
data.new <- data.frame(midparentHeight = mph.new)
ci.new <- predict(galton.lm, data.new, interval = "confidence")
pi.new <- predict(galton.lm, data.new, interval = "prediction")

round(head(cbind(ci.new, pi.new)), 3)

?lalonde
# install.packages("Matching")
library(Matching)
data("lalonde")
lalonde.lm <- lm(re78 ~., data = lalonde) # regression on all covariates
summary(lalonde.lm)

library(car)
linearHypothesis(lalonde.lm, c("age=0", "educ=0", "black=0", "hisp=0", "married=0", "nodegr=0", "re74=0", "re75=0", "u74=0", "u75=0"))
