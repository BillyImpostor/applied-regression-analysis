#========Iterative Searching========#

x <- c(1:10, 13:22)
y <- numeric(20)
## Create first segment

set.seed(123)
y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
## Create second segment

set.seed(123)
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
curve((8.61293 + 11.87676) + (0.31118-1.01519  )*x, add=T, from=1, to=8)
curve((8.61293 - 0.94203 ) + 0.31118 *x, add=T, from=8, to=max(x))
abline(v=8, lty=2)


#========= Segmented Package========#
library(segmented)
?segmented

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi=14)
summary(segmented.mod)

plot(x,y, pch=16, ylim=c(5,20))
plot(segmented.mod, add=T)

# Interpretasi:Setiap kenaikan 1 satuan x akan menyebabkan (kenaikan/penurunan) sebanyak (beta) dengan syarat x lebih besar dari (knot) (dengan efek kuadratik/kubik - in case dia kuadrtik dan kubik).
