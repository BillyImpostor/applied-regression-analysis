x <- c(1:10, 13:22)
y <- numeric(20)
## Create first segment

set.seed(127)
y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
## Create second segment

set.seed(127)
y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
x
y
## Plot it
par(mar=c(4,4,1,1)+0.2)
plot(x,y, ylim=c(0, 50), pch=16)

breaks <- x[which(x >= 1 & x <= 20)]

mse <- numeric(length(breaks))
for(i in 1:length(breaks)){
  piecewise1 <- lm(y ~ x*(x < breaks[i]) + x*(x>=breaks[i]))
  mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse)
b1 <- breaks[which(mse==min(mse))]
b1
piecewise2 <- lm(y ~ x*(x < b1) + x*(x >= b1))
summary(piecewise2)

plot(x,y, ylim=c(5, 20), pch=16)
curve((4.3561 + 15.4849) + (0.5181-1.2598)*x, add=T, from=1, to=15)
curve(4.3561 + 0.5181*x, add=T, from=15, to=max(x))
abline(v=15, lty=3)

library(segmented)
lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=14) # psi=14 bisa diilangin
summary(segmented.mod)

plot(x,y, pch=16, ylim=c(5,20))
plot(segmented.mod, add=T)
