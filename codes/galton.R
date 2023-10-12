install.packages("HistData")
library(HistData)
x <- GaltonFamilies$midparentHeight
y <- GaltonFamilies$childHeight

x.mean <- mean(x)
y.mean <- mean(y)
x.sd <- sd(x)
y.sd <- sd(y)
rho.xy <- cor(x,y)

beta.hat <- rho.xy * y.sd / x.sd
alpha.hat <- y.mean - x.mean * beta.hat

alpha.hat

beta.hat

fit <- lm(y ~ x)
summary(fit)


plot(x, y, xlab = "mid parent height", ylab = "children height", pch = 21, bg = "grey", col = "grey")
abline(fit)
points(x.mean, y.mean, pch = 16)
