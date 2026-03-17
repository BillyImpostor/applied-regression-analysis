# ====================== MATERI: DUMMY VARIABLE ======================

#Import Data
library(openxlsx)
data <- read.xlsx("path to file")

# Melihat data asli
cat("Original dataset: \n")
head(data, 20)

#Lihat kategori pada variabel yang akan dibuat dummy
unique(data$Sex)

# Create dummy variable
data$dFemale <- ifelse(data$Sex == "Female", 1, 0)
data$dInfant <- ifelse(data$Sex == "Infant", 1, 0)

# Print
cat("After creating dummy variable: /n")
head(data, 20)

#=== Cara lain ===#
# Install the required package
install.packages("fastDummies")

# Load the library
library(fastDummies)

# Create dummy variable
data <- dummy_cols(data, select_columns = "Sex", remove_first_dummy=TRUE)

help("dummy_cols")
# Print
print(data)

# ====================== CONTOH KASUS ======================
# Input Data
library(openxlsx)
df = read.xlsx('Path to File')

# Variabel Independen
y = df$Whole.weight
# Variabel Dependen
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

# Pembantukan Model
df_modelling <- df[,-1]


# Model I
model1 = lm(Whole.weight ~ ., data = df_modelling)
summary(model1)
sqrt(summary(model1)$r.squared)

# Model II
# Drop Rings Column
library(dplyr)
df_modelling <- select(df_modelling, -Rings)
model2 = lm(Whole.weight ~ ., data = df_modelling)
summary(model2)
sqrt(summary(model2)$r.squared)

# Model III
# Drop Length Column
df_modelling <- select(df_modelling, -Length)
model3 = lm(Whole.weight ~ ., data = df_modelling)
summary(model3)
sqrt(summary(model3)$r.squared)

# Model IV
# Drop Height Column
df_modelling <- select(df_modelling, -Height)
model4 = lm(Whole.weight ~ ., data = df_modelling)
summary(model4)
sqrt(summary(model4)$r.squared)

# Model V
# Drop Sex Column
df_modelling <- select(df_modelling, -c(dFemale, dInfant))
model5 = lm(Whole.weight ~ ., data = df_modelling)
summary(model5)
sqrt(summary(model5)$r.squared)

# Model VI
# Drop Diameter Column
df_modelling <- select(df_modelling, -Diameter)
model6 = lm(Whole.weight ~ ., data = df_modelling)
summary(model6)
sqrt(summary(model6)$r.squared)

summary(model6)$p.value

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