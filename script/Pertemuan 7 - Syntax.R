library(openxlsx)
# Polynomial Regression #
## Contoh Kasus 1
data1 = read.xlsx("D:/KULIAH/SEMESTER 4/ASPRAK ART/Dataset Pertemuan 7 Polynomial & Central Method Regression.xlsx", 1)
data1

# Variabel Dependen
Y = data1$Nilai.Try.Out
# Variabel Independen
X = data1$Lama.Belajar

# Scatter Plot
plot(X, Y, main="Scatterplot Uji Linearitas",
     xlab="Lama Belajar", ylab="Nilai Try Out", pch=19)
abline(lm(Y~X), col="blue")

# Pembentukan Model
poly2 = lm(Y ~ poly(X, 2, raw = TRUE))
summary(poly2)
sqrt(summary(poly2)$r.squared)

# Visualisasi Model
pred2 = predict(poly2, newdata = list(x=X))
plot(X, Y, main="Degree-2 Polynomial",
     xlab="Study Hours", ylab="Exam Scores", pch=19)
lines(X, pred2, lwd = 2, col = "red")  


### --- ###
## Contoh Kasus 2
data2 = read.xlsx("D:/KULIAH/SEMESTER 4/ASPRAK ART/Dataset Pertemuan 7 Polynomial & Central Method Regression.xlsx", 2)
data2

# Variabel Dependen
Y = data2$Tensile.Strength
# Variabel Independen
X = data2$Hardwood.Concentration

# Scatter Plot 
plot(X, Y, main="Scatterplot Uji Linearitas",
     xlab="Hardwood Concentration", ylab="Tensile Strength", pch=19)
abline(lm(Y~X), col="blue")

# Pembentukan Model
## Orde 1
poly1 = lm(Y ~ X)
summary(poly1)
sqrt(summary(poly1)$r.squared)
mean(poly1$residuals^2)

## Orde 2
poly2 = lm(Y ~ poly(X, 2, raw = TRUE))
summary(poly2)
sqrt(summary(poly2)$r.squared) # Nilai R
mean(poly2$residuals^2)

## Orde 3
poly3 = lm(Y ~ poly(X, 3, raw = TRUE))
summary(poly3)
sqrt(summary(poly3)$r.squared) # Nilai R
mean(poly3$residuals^2)

## Orde 4
poly4 = lm(Y ~ poly(X, 4, raw = TRUE))
summary(poly4)
sqrt(summary(poly4)$r.squared) # Nilai R
mean(poly4$residuals^2)

## Orde 5
poly5 = lm(Y ~ poly(X, 5, raw = TRUE))
summary(poly5)
sqrt(summary(poly5)$r.squared) # Nilai R
mean(poly5$residuals^2)

# Gabungkan model ke dalam list
models <- list(
  "Orde 1" = poly1,
  "Orde 2" = poly2,
  "Orde 3" = poly3,
  "Orde 4" = poly4,
  "Orde 5" = poly5
)

# Fungsi untuk mengambil metrik evaluasi
get_metrics <- function(model) {
  s <- summary(model)
  
  r_square <- s$r.squared
  adj_r_square <- s$adj.r.squared
  r_value <- sqrt(r_square)
  rse <- s$sigma
  mse <- mean(model$residuals^2)
  
  return(c(
    R = r_value,
    R_Square = r_square,
    Adj_R_Square = adj_r_square,
    RSE = rse,
    MSE = mse
  ))
}

# Buat tabel perbandingan
model_comparison <- as.data.frame(
  t(sapply(models, get_metrics))
)

# Rapikan hasil
model_comparison$Model <- rownames(model_comparison)
rownames(model_comparison) <- NULL

model_comparison <- model_comparison[, c(
  "Model", "R", "R_Square", "Adj_R_Square", "RSE", "MSE"
)]

View(model_comparison)


# ===================== Model dengan Orde 3 ===================== #
## Model 1
poly3 = lm(Y ~ poly(X, 3, raw = TRUE))
summary(poly3)
sqrt(summary(poly3)$r.squared)
# Konstanta tidak signifikan

## Model 2
poly3.2 = lm(Y ~ poly(X, 3, raw = TRUE)-1) # -1 di sini berfungsi agar konstanta tidak digunakan pada model
summary(poly3.2)
sqrt(summary(poly3.2)$r.squared)
# Koefisien x^2 tidak signifikan

## Model 3
poly3.3 = lm(Y ~ I(X) + I(X^3) - 1) # buat model secara manual agar X^2 tidak ikut menjadi model polynom
summary(poly3.3)
sqrt(summary(poly3.3)$r.squared)

# Overall Test
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

overall_test = data.frame(matrix(ncol = 2, nrow = 3))
colnames(overall_test) = c("Model", "P.Value")
overall_test$Model = c("Model 1", "Model 2", "Model 3")
overall_test$P.Value = c(overall_p(poly3),
                         overall_p(poly3.2),
                         overall_p(poly3.3))
print(overall_test)

# Uji Parsial untuk Konstanta
intercept_p <- function(my_model) {
  p <- summary(my_model)$coefficients[1,4]
  return(p)
}
partial_test = data.frame(matrix(ncol = 2, nrow = 3))
colnames(partial_test) = c("Model", "P.Value")
partial_test$Model = c("Model 1", "Model 2", "Model 3")
partial_test$P.Value = c(intercept_p(poly3)) # hanya model pertama karena konstanta dibuang pada model kedua
print(partial_test)

# Uji Parsial untuk Koefisien
coef_p <- function(my_model) {
  p <- summary(my_model)$coefficients[-1,4]
  return(round(p, 3))
}
coef_p(poly3)
summary(poly3.2)$coefficients[,4]
summary(poly3.3)$coefficients[,4]

# Diagnostic Checking
# Hubungan Variabel Dependen dan Independen
pred = predict(poly3.3, newdata = list(x=X))
plot(X, Y, main="Degree-3 Polynomial",
     xlab="Hardwood Concentration", ylab="Tensile Strength", pch=19)
lines(X, pred, lwd = 2, col = "red")  

# Normalitas residual
poly3.3$residual
## Metode Histogram
hist(poly3.3$residual, main = "Histogram Residual", 
     xlab = "Residual", ylab = "Frekuensi", col = "lightblue")
## Metode QQ-Plot
qqnorm(poly3.3$residual, main = "Normal Q-Q Plot Residual")
qqline(poly3.3$residual)
## Metode Inferensi, menggunakan Shapiro-Wilk Test karena sampel < 50
shapiro.test(poly3.3$residual)

# Homoskedastisitas
## Metode Plot Residuals vs Fitted
plot(poly3.3, 1)

## Metode Inferensi
library(lmtest)
bptest(poly3.3)

# Runs test
#install.packages("devtools")
library(devtools)
#devtools::install_github("vc1492a/runstest-R")
install.packages("runstest")
library(runstest)
runsTest(poly3.3)

# No-Autokorelasi Error
library(car)
durbinWatsonTest(poly3.3)

# No Multkolinearitas
vif(poly3.3)

# Visualisasi Model
pred = predict(poly3.3, newdata = list(x=X))
plot(X, Y, main="Degree-3 Polynomial - model 3",
     xlab="Hardwood Concentration", ylab="Tensile Strength", pch=19)
lines(X, pred, lwd = 2, col = "red")  

# Center Mean Regression #
## Contoh Kasus
# Membentuk variabel X-Xbar
data2$C_hwconc = data2$Hardwood.Concentration - mean(data2$Hardwood.Concentration)

# Variabel Dependen
Y = data2$Tensile.Strength
# Variabel Independen
Xc = data2$C_hwconc

# Pembentukan Model
poly3.3C = lm(Y ~ I(Xc) + I(Xc^3) - 1)
summary(poly3.3C)
sqrt(summary(poly3.3C)$r.squared)

# Cek Multkolinearitas
vif(poly3.3C)
