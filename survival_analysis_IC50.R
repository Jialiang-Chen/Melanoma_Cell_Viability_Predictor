
setwd('/Users/stan/Desktop/spring_semester/AI_for_personalised_medicine/final_assignment/Melanoma_Cell_Viability_Predictor')
getwd()

# import package
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# read file
drug_Dacarbazine = read.csv('IC50_survival_status_Dacarbazine.csv')
drug_Temozolomide = read.csv('IC50_survival_status_Temozolomide.csv')
drug_Paclitaxel = read.csv('IC50_survival_status_Paclitaxel.csv')
drug_Cisplatin = read.csv('IC50_survival_status_Cisplatin.csv')
drug_Dabrafenib = read.csv('IC50_survival_status_Dabrafenib.csv')
drug_Trametinib = read.csv('IC50_survival_status_Trametinib.csv')
drug_Nilotinib = read.csv('IC50_survival_status_Nilotinib.csv')
drug_Dasatinib = read.csv('IC50_survival_status_Dasatinib.csv')

# drug_list = list('Dacarbazine','Temozolomide','Paclitaxel','Cisplatin','Dabrafenib','Trametinib','Nilotinib','Dasatinib')
# change the name of df to plot survival plots for each drug
median_ic50 <- median(drug_Dabrafenib$Pred_IC50, na.rm = TRUE)
max(drug_Dabrafenib$Pred_IC50) 
min(drug_Dabrafenib$Pred_IC50) 
drug_Dabrafenib$IC50_group <- ifelse(drug_Dabrafenib$Pred_IC50 > median_ic50, "High", "Low")
# 创建Surv对象
surv_object <- Surv(time = drug_Dabrafenib$OS_MONTHS, 
                    event = drug_Dabrafenib$OS_STATUS)

# 拟合Kaplan-Meier模型
km_fit <- survfit(surv_object ~ IC50_group, data = drug_Dabrafenib)

# 绘制生存曲线
ggsurvplot(km_fit, data = drug_Dabrafenib, 
           pval = TRUE, 
           conf.int = TRUE,
           xlab = "Time (months)", 
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curve by IC50 Group")
