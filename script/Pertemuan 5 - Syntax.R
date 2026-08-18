# =========== MATERI: DIAGNOSTIC CHECKING FOR MULTIPLE LINEAR REGRESSION ==============
# ===================== CONTOH KASUS 1 ======================
# Input Data
library(openxlsx)
df = read.xlsx("path to file", 1)

# Variabel Dependen
y = df$BCM
# Variabel Independen
X = df[, -1] # Ambil data frame selain BCM

# Asumsi Linearitas
pairs(df, main = "Matriks Scatterplot Overburden Production", pch = 19)
## Metode ggplot2 dan GGally
library(ggplot2) 
library(GGally)
ggpairs(df, title = "Matriks Scatterplot Overburden Production", 
        axisLabels = "show")
cor(df[,-1], method = "pearson")

R2 = c()

# Membuat Dataframe R-Squared
R2 = data.frame(matrix(ncol = ncol(X), nrow = 1))
colnames(R2) = colnames(X)
for (i in 1:ncol(X)){
  R2[i] = summary(lm(y ~ X[,i]))$r.squared
}
print(R2)

# Pembentukan Model
# Model Criterion Function
library(olsrr)

# bentuk model dengan parameter yg lengkap
full_model <- lm(BCM ~ ., data = df)

# membuat function kriteria pemilihan model
model_criterion <- function(data, model){
  rss <- sum(model$residuals^2)
  n <- nrow(data)
  k <- length(coef(model))
  
  rss_full <- sum(residuals(full_model)^2)
  k_full <- length(coef(full_model))
  mse_full <- rss_full/(n - k_full)
  cp <- (rss/mse_full) - (n - 2*k)
  
  aic <- n*log(rss/n) + 2*k
  sbc <- n*log(rss/n) + k*log(n)
  press <- ols_press(model)
  r <- sqrt(summary(model)$r.squared)
  r.square <- summary(model)$r.squared
  adj.r.square <- summary(model)$adj.r.squared
  se <- summary(model)$sigma
  
  result <- data.frame(R=r, R.Square=r.square, "Adj.RSquare"=adj.r.square,"Residual Standard Error"=se ,AIC=aic, SBC=sbc, CP_Mallows=cp, PRESS=press)
  print("Model Criterion for Model:")
  return(result)
}

# model_criterion(data,model)

df_modelling <- df

# Model I
model1 <- lm(BCM ~ ., data = df_modelling)
summary(model1)
model_criterion(df_modelling,model1)

# Model II
# Drop PA Column
library(dplyr)
df_modelling <- select(df_modelling, -PA)
model2 = lm(BCM ~ ., data = df_modelling)
summary(model2)
model_criterion(df_modelling,model2)

# Model III
# Drop USD_BCM Column
df_modelling <- select(df_modelling, -USD_BCM)
model3 = lm(BCM ~ ., data = df_modelling)
summary(model3)
model_criterion(df_modelling,model3)

# Model IV
# Drop Constant
model4 = lm(BCM ~ 0+., data = df_modelling)
summary(model4)
model_criterion(df_modelling,model4)

# Overall Test
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

overall_test = data.frame(matrix(ncol = 2, nrow = 4))
colnames(overall_test) = c("Model", "Overall Test's P-Value")
overall_test$Model = c("Model 1", "Model 2", "Model 3", "Model 4")
overall_test$P.Value = c(overall_p(model1),
                         overall_p(model2),
                         overall_p(model3),
                         overall_p(model4))
print(overall_test[,-2])


# Uji Parsial untuk Konstanta
intercept_p <- function(my_model) {
  p <- summary(my_model)$coefficients[1,4]
  return(p)
}
partial_test = data.frame(matrix(ncol = 2, nrow = 4))
colnames(partial_test) = c("Model", "Intercept's P-Value")
partial_test$Model = c("Model 1", "Model 2", "Model 3", "Model 4")
partial_test$P.Value = c(intercept_p(model1),
                         intercept_p(model2),
                         intercept_p(model3),
                         NA)
print(partial_test[,-2])

# Uji Parsial untuk Koefisien
coef_p <- function(my_model) {
  p <- summary(my_model)$coefficients[-1,4]
  return(round(p, 3))
}
coef_p(model1)
coef_p(model2)
coef_p(model3)
summary(model4)$coefficients[1,4] #Model tanpa konstanta


# Model Regresi Terbentuk
est_coef <- function(my_model) {
  coef <- summary(my_model)$coefficients[,1]
  return(round(coef, 3))
}
est_coef(model1)
est_coef(model2)
est_coef(model3)
est_coef(model4)

# Membandingkan model criterion setiap model
model_criterion(df_modelling,model1)
model_criterion(df_modelling,model2)
model_criterion(df_modelling,model3)
model_criterion(df_modelling,model4)

# ===================== DIAGNOSTIC CHECKING ======================
# Berdasarkan Modul 4, diperoleh model terbaik ada model III berdasarkan kriteria pemilihan model terbaik
# Model III
# Variable Independen : UA
df_diagnostic <- select(df, c("BCM", "UA"))
model3 = lm(BCM ~ ., data = df_diagnostic)
summary(model3)
model_criterion(df_diagnostic, model3)

## Diagnostic Checking
# Asumsi Linearitas
y = df_diagnostic$BCM
X = select(df_diagnostic, -c("BCM"))

# Jika tersisa 1 variabel
plot(as.list(X)[[1]], y, main="Scatterplot Asumsi Linearitas",
     xlab="UA", ylab="BCM", pch=19)
abline(lm(y~as.list(X)[[1]]), col="blue")

R2 = c()
# Membuat Dataframe R-Squared
R2 = data.frame(matrix(ncol = ncol(X), nrow = 1))
colnames(R2) = colnames(X)
for (i in 1:ncol(X)){
  R2[i] = summary(lm(y ~ X[,i]))$r.squared
}
print(R2)

# Normalitas Residual
model3$residual

## Metode Histogram
hist(model3$residual, main = "Histogram Residual", 
     xlab = "Residual", ylab = "Frekuensi", col = "lightblue")

## Metode QQ-Plot
qqnorm(model3$residual, main = "Normal Q-Q Plot Residual")
qqline(model3$residual)

## Metode Inferensi, menggunakan Shapiro-Wilk Test karena sampel < 50
shapiro.test(model3$residual)

# Homoskedastisitas
## Metode Scatter Plot
plot(model3, 1)

## Metode Inferensi
library(lmtest)
bptest(model3)

# No-Autokorelasi Error
library(car)
durbinWatsonTest(model3)

# ===================== CONTOH KASUS 2 ======================

#Import Data
library(openxlsx)
df <- read.xlsx("path to file", 2)

# Variabel Dedependen
y = df$Whole.weight
# Variabel Independen
X = df[, -5] # Ambil data frame selain whole weight

# Asumsi Linearitas
pairs(df[,-1], main = "Matriks Scatterplot Data Numerik Abalone", pch = 19)
## Metode ggplot2 dan GGally
library(ggplot2) 
library(GGally)
ggpairs(df[,-1], title = "Matriks Scatterplot Data Numerik Abalone", 
        axisLabels = "show")
cor(df[,-1], method = "pearson")

R2 = c()

# Membuat Dataframe R-Squared
X_num = X[,-1]
R2 = data.frame(matrix(ncol = ncol(X_num), nrow = 1))
colnames(R2) = colnames(X_num)
for (i in 1:ncol(X_num)){
  R2[i] = summary(lm(y ~ X_num[,i]))$r.squared
}
print(R2)

# Dummy Variable
unique(df$Sex)
df$dFemale <- ifelse(df$Sex == "Female", 1, 0)
df$dInfant <- ifelse(df$Sex == "Infant", 1, 0)

# bentuk model dengan parameter yg lengkap
full_model <- lm(Whole.weight ~ ., data = df)

# membuat function kriteria pemilihan model
model_criterion <- function(data, model){
  rss <- sum(model$residuals^2)
  n <- nrow(data)
  k <- length(coef(model))
  
  rss_full <- sum(residuals(full_model)^2)
  k_full <- length(coef(full_model))
  mse_full <- rss_full/(n - k_full)
  cp <- (rss/mse_full) - (n - 2*k)
  
  aic <- n*log(rss/n) + 2*k
  sbc <- n*log(rss/n) + k*log(n)
  press <- ols_press(model)
  r <- sqrt(summary(model)$r.squared)
  r.square <- summary(model)$r.squared
  adj.r.square <- summary(model)$adj.r.squared
  se <- summary(model)$sigma
  
  result <- data.frame(R=r, R.Square=r.square, "Adj.RSquare"=adj.r.square,"Residual Standard Error"=se ,AIC=aic, SBC=sbc, CP_Mallows=cp, PRESS=press)
  print("Model Criterion for Model:")
  return(result)
}

# Pembantukan Model
df_modelling <- df[,-1]

# Model I
model1 = lm(Whole.weight ~ ., data = df_modelling)
summary(model1)
model_criterion(df_modelling,model1)

# Model II
# Drop Rings Column
library(dplyr)
df_modelling <- select(df_modelling, -Rings)
model2 = lm(Whole.weight ~ ., data = df_modelling)
summary(model2)
model_criterion(df_modelling,model2)

# Model III
# Drop Length Column
df_modelling <- select(df_modelling, -Length)
model3 = lm(Whole.weight ~ ., data = df_modelling)
summary(model3)
model_criterion(df_modelling,model3)

# Model IV
# Drop Height Column
df_modelling <- select(df_modelling, -Height)
model4 = lm(Whole.weight ~ ., data = df_modelling)
summary(model4)
model_criterion(df_modelling,model4)

# Model V
# Drop Sex Column
df_modelling <- select(df_modelling, -c(dFemale, dInfant))
model5 = lm(Whole.weight ~ ., data = df_modelling)
summary(model5)
model_criterion(df_modelling,model5)

# Model VI
# Drop Diameter Column
df_modelling <- select(df_modelling, -Diameter)
model6 = lm(Whole.weight ~ ., data = df_modelling)
summary(model6)
model_criterion(df_modelling,model6)

# Overall Test
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

overall_test = data.frame(matrix(ncol = 2, nrow = 6))
colnames(overall_test) = c("Model", "Overall Test's P-Value")
overall_test$Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
overall_test$P.Value = c(overall_p(model1),
                         overall_p(model2),
                         overall_p(model3),
                         overall_p(model4),
                         overall_p(model5),
                         overall_p(model6))
print(overall_test[,-2])

# Uji Parsial untuk Konstanta
intercept_p <- function(my_model) {
  p <- summary(my_model)$coefficients[1,4]
  return(p)
}
partial_test = data.frame(matrix(ncol = 2, nrow = 6))
colnames(partial_test) = c("Model", "Intercept's P-Value")
partial_test$Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
partial_test$P.Value = c(intercept_p(model1),
                         intercept_p(model2),
                         intercept_p(model3),
                         intercept_p(model4),
                         intercept_p(model5),
                         intercept_p(model6))
print(partial_test[,-2])

# Uji Parsial untuk Koefisien
coef_p <- function(my_model) {
  p <- summary(my_model)$coefficients[-1,4]
  return(round(p, 3))
}
coef_p(model1)
coef_p(model2)
coef_p(model3)
coef_p(model4)
coef_p(model5)
coef_p(model6)

# Model Regresi Terbentuk
est_coef <- function(my_model) {
  coef <- summary(my_model)$coefficients[,1]
  return(round(coef, 3))
}
est_coef(model1)
est_coef(model2)
est_coef(model3)
est_coef(model4)
est_coef(model5)
est_coef(model6)

# Membandingkan model criterion setiap model
model_criterion(df_modelling,model1)
model_criterion(df_modelling,model2)
model_criterion(df_modelling,model3)
model_criterion(df_modelling,model4)
model_criterion(df_modelling,model5)
model_criterion(df_modelling,model6)

# ===================== DIAGNOSTIC CHECKING ======================
# Berdasarkan Modul 4, diperoleh model terbaik ada model VI berdasarkan kriteria pemilihan model terbaik
# Model VI
# Variable Independen : shucked weight, viscera weight, shell weight
df_diagnostic <- select(df, c("Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight"))
model6 = lm(Whole.weight ~ ., data = df_diagnostic)
summary(model6)
model_criterion(df_diagnostic, model6)

## Diagnostic Checking
# Asumsi Linearitas
y = df_diagnostic$Whole.weight
X = select(df_diagnostic, -c("Whole.weight"))

# Jika terdapat lebih dari 1 variabel
pairs(df_diagnostic, main = "Matriks Scatterplot Overburden Production", pch = 19)
## Metode ggplot2 dan GGally
library(ggplot2) 
library(GGally)
ggpairs(df_diagnostic, title = "Matriks Scatterplot Overburden Production", 
        axisLabels = "show")
cor(df_diagnostic[,-1], method = "pearson")

R2 = c()
# Membuat Dataframe R-Squared
R2 = data.frame(matrix(ncol = ncol(X), nrow = 1))
colnames(R2) = colnames(X)
for (i in 1:ncol(X)){
  R2[i] = summary(lm(y ~ X[,i]))$r.squared
}
print(R2)

# Normalitas Residual
model6$residual

## Metode Histogram
hist(model6$residual, main = "Histogram Residual", 
     xlab = "Residual", ylab = "Frekuensi", col = "lightblue")

## Metode QQ-Plot
qqnorm(model6$residual, main = "Normal Q-Q Plot Residual")
qqline(model6$residual)

## Metode Inferensi, menggunakan Kolmogorov-Smirnov Test karena sampel > 50
ks.test(model6$residual,'pnorm',mean=mean(model6$residual),sd=sd(model6$residual))

# Homoskedastisitas
## Metode Scatter Plot
plot(model6, 1)

## Metode Inferensi
library(lmtest)
bptest(model6)

# No-Autokorelasi Error
library(car)
durbinWatsonTest(model6)

# No Multikolinearitas
ols_vif_tol(model6)