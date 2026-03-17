# Input Data
library(openxlsx)
data1 = read.xlsx("path to file")

# Variabel Independen
RataSuhu = data1$Suhu_Ruangan
# Variabel Dependen
Y = data1$Cacat_Produksi

#Asumsi Linearitas 
plot(RataSuhu, Y, main="Scatterplot Uji Linearitas",
     xlab="Rata-rata Suhu Ruangan", ylab="Jumlah Cacat Produksi", pch=19)
abline(lm(Y~RataSuhu), col="blue")

model1 = lm(formula = Y ~ RataSuhu, data = data1)
summary(model1)$r.squared

# Pembentukan Model
model1 = lm(formula = Y ~ RataSuhu, data = data1)

# Nilai Korelasi (R)
cor(RataSuhu,Y)

# Summary Model
summary(model1)

# Diagnostic checking linearitas
plot(RataSuhu, Y, main="Scatterplot Uji Linearitas",
     xlab="Rata-rata Suhu Ruangan", ylab="Jumlah Cacat Produksi", pch=19)
abline(lm(Y~RataSuhu), col="blue")


# Normalitas residual
model1$residual
## Metode Histogram
hist(model1$residual, main = "Histogram Residual", 
     xlab = "Residual", ylab = "Frekuensi", col = "lightblue")
## Metode QQ-Plot
qqnorm(model1$residual, main = "Normal Q-Q Plot Residual")
qqline(model1$residual)
## Metode Inferensi, menggunakan Shapiro-Wilk Test karena sampel < 50
shapiro.test(model1$residual)

# Homoskedastisitas
## Metode Plot Residuals vs Fitted
plot(model1, 1)

## Metode Inferensi
library(lmtest)
bptest(model1)

# Runs test
library(devtools)
devtools::install_github("vc1492a/runstest-R")
library(runstest)
runsTest(model1)

# No-Autokorelasi Error
library(car)
durbinWatsonTest(model1)