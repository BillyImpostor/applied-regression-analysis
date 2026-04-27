# =========== MATERI: SELEKSI VARIABEL SECARA SEKUENSIAL ==============
# ===================== CONTOH KASUS 1 ======================
# Input Data
library(openxlsx)
df = read.xlsx("Path to file", 1)
df

#Membuat model regresi
baseModel = lm(BCM ~ 1, df)
model <- lm(BCM ~ ., data = df)
baseModel
model

#=========Tanpa Library========#
#Forward stepwise selection
step(baseModel,scope=list(upper=model,lower=~1),direction="forward")

#Backward stepwise elimination
step(model, direction="backward")


#========Menggunakan Library========#
library(olsrr)

#All Possible Regression
ols_step_all_possible(model)

#Best Subset Regression
ols_step_best_subset(model)

#Stepwise Forward Selection berdasarkan p-value
ols_step_forward_p(model)

#Metode lain
#Berdasarkan adjusted r2
ols_step_forward_adj_r2(model)

#Berdasarkan AIC
ols_step_forward_aic(model)

#Berdasarkan r2
ols_step_forward_r2(model)

#Berdasarkan SBC
ols_step_forward_sbc(model)

#Berdasarkan SBIC
ols_step_forward_sbic(model)


#=====Stepwise Backward Elimination
#Stepwise Backward Selection berdasarkan p-value
ols_step_backward_p(model)

#Metode lain
#Berdasarkan adjusted r2
ols_step_backward_adj_r2(model)

#Berdasarkan AIC
ols_step_backward_aic(model)

#Berdasarkan r2
ols_step_backward_r2(model)

#Berdasarkan SBC
ols_step_backward_sbc(model)

#Berdasarkan SBIC
ols_step_backward_sbic(model)

#======Stepwise
#Stepwise Regression berdasarkan p-value
ols_step_both_p(model)

#Metode lain
#Berdasarkan adjusted r2
ols_step_both_adj_r2(model)

#Berdasarkan AIC
ols_step_both_aic(model)

#Berdasarkan r2
ols_step_both_r2(model)

#Berdasarkan SBC
ols_step_both_sbc(model)

#Berdasarkan SBIC
ols_step_both_sbic(model)
