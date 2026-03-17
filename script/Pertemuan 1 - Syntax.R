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

