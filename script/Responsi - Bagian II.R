# BAGIAN II
# Nomor 1
# Membuat data sintetis
x <- c(1:11, 16:21, 24:31)
y <- numeric(length(x))

# Segmen pertama
set.seed(456)
y[1:11] <- seq(10, 60, len = 11) + rnorm(11, 0, 3)

# Segmen kedua
set.seed(456)
y[12:17] <- seq(45, 15, len = length(16:21)) + rnorm(length(16:21), 0, 3)

# Segmen ketiga
set.seed(456)
y[18:25] <- seq(25, 70, len = length(24:31)) + rnorm(length(24:31), 0, 3)

# Validasi
x
y


# Nomor 2
breaks1 <- x[which(x >= 1 & x <= 21)]

# Knot 1
mse1 <- numeric(length(breaks1))
for(i in 1:length(breaks1)){
  piecewise1 <- lm(y ~ x*(x < breaks1[i]) + x*(x >= breaks1[i]))
  mse1[i] <- summary(piecewise1)[6]
}
mse1 <- as.numeric(mse1)
b1 <- breaks1[which(mse1 == min(mse1))]
b1

# Knot 2
breaks2 <- x[which(x > b1)]
mse2 <- numeric(length(breaks2))
for(j in 1:length(breaks2)){
  piecewise2 <- lm(y ~ x*(x < b1) + x*(x >= b1 & x < breaks2[j]) + x*(x >= breaks2[j]))
  mse2[j] <- summary(piecewise2)[6]
}
mse2 <- as.numeric(mse2)
b2 <- breaks2[which(mse2 == min(mse2))]
b2

# Model Regresi
piecewise_final <- lm(y ~ x*(x < b1) + x*(x >= b1 & x < b2) + x*(x >= b2))
summary(piecewise_final)


# Nomor 3
plot(x,y, ylim=c(0, 80), pch=16)
curve((-138.4587 + 142.0703) + (6.7429 - 1.5453)*x, add=T, from=1, to=11)
curve((-138.4587 + 280.0303) + (6.7429 - 12.8373)*x, add=T, from=16, to=21)
curve(-138.4587 + 6.7429*x,, add=T, from=24, to=max(x))
abline(v=16, lty=2)
abline(v=24, lty=2)


# Nomor 4
library(segmented)
lin.mod <- lm(y~x) 
segmented.mod <- segmented(lin.mod, seg.Z = ~x, psi= c(16, 24))
summary(segmented.mod)


# Nomor 15
x <- 15
yhat = 3.612 + 5.198 * x - 11.292 * (x - 12.217)
yhat
