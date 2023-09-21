# Importing packages

library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(tableone)

setwd("/your_working_directory/")
pnq_pandemic <- fread("PNQ_Pandemic_Deidentified.csv")

# 1.	Demographics – All Cohorts (Cohort 1 – Pitt, Cohort 2 – Pitt, Cohort 3 – Columbia, Cohort 4 – Columbia, Cohort 5 – Yale, Cohort 6 – Buffalo, Cohort 7 – Penn, Cohort 8 – GEMS)

listVars1 <- c("age", "sex", "race_eth", "age_firstsx", "dx_duration", "education", "employment", "income", "married", "live_alone", "occupation", "network_size", "fdr", "msrs_total", "promis_t_score")
table1 <- suppressWarnings(CreateTableOne(vars = listVars1, strata = c("ms", "cohort"), data = pnq_pandemic, test = TRUE, includeNA = FALSE))
table1_print <- print(table1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# 2.	Demographics – MS vs Control; grouping all sites together 

table2 <- suppressWarnings(CreateTableOne(vars = listVars1, strata = c("ms"), data = pnq_pandemic, test = TRUE, includeNA = FALSE))
table2_print <- print(table2, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# CROSS SECTIONAL ANALYSIS

pnq_ms <- pnq_pandemic %>% filter(pnq_pandemic$ms == "MS")
pnq_ms$education <- factor(pnq_ms$education, labels = c("High school grad", "Some college", "Associate degree", "Bachelor's degree", "Graduate degree"), levels = c("High school grad", "Some college", "Associate degree", "Bachelor's degree", "Graduate degree"))
pnq_ms$employment <- factor(pnq_ms$employment, labels = c("Employed for wages", "Self-employed", "Out of work and looking for work", "Out of work but not currently looking for work", "Homemaker", "Student", "Military", "Retired", "Unable to work"), levels = c("Employed for wages", "Self-employed", "Out of work and looking for work", "Out of work but not currently looking for work", "Homemaker", "Student", "Military", "Retired", "Unable to work"))
pnq_ms$occupation <- factor(pnq_ms$occupation, labels = c("Professional", "Executive, manager", "Sales or clerical worker", "Mechanic, electrician, skilled worker",  "Machine operator, inspector, bus/cab driver", "Service worker", "Business owner", "Laborer, unskilled worker", "Farming", "Military", "Other"), levels = c("Professional", "Executive, manager", "Sales or clerical worker", "Mechanic, electrician, skilled worker",  "Machine operator, inspector, bus/cab driver", "Service worker", "Business owner", "Laborer, unskilled worker", "Farming", "Military", "Other"))
pnq_ms$income <- factor(pnq_ms$income, labels = c("0 to $19,999", "$20,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $79,999", "$80,000 to $94,999", "$95,000 to $109,999", "$110,000 to $124,999", "$125,000 or higher"), levels = c("0 to $19,999", "$20,000 to $34,999", "$35,000 to $49,999", "$50,000 to $64,999", "$65,000 to $79,999", "$80,000 to $94,999", "$95,000 to $109,999", "$110,000 to $124,999", "$125,000 or higher"))
pnq_ms$live_alone <- factor(pnq_ms$live_alone, labels = c("Yes", "No"), levels = c("Yes", "No"))

pnq_control <- pnq_pandemic %>% filter(pnq_pandemic$ms == "Control")

# Identifying covariates of interest
# Checking which variables are correlated with neurological outcomes

model1_1 <- lm(fdr ~ age, data=pnq_ms)
model2_1 <- lm(fdr ~ sex, data=pnq_ms)
model3_1 <- lm(fdr ~ race_eth, data=pnq_ms)
model4_1 <- lm(fdr ~ education, data=pnq_ms)
model5_1 <- lm(fdr ~ employment, data=pnq_ms)
model6_1 <- lm(fdr ~ occupation, data=pnq_ms)
model7_1 <- lm(fdr ~ income, data=pnq_ms)
model8_1 <- lm(fdr ~ married, data=pnq_ms)
model9_1 <- lm(fdr ~ live_alone, data=pnq_ms)
model10_1 <- lm(fdr ~ dx_duration, data=pnq_ms)

model1_2 <- lm(msrs_total ~ age, data=pnq_ms)
model2_2 <- lm(msrs_total ~ sex, data=pnq_ms)
model3_2 <- lm(msrs_total ~ race_eth, data=pnq_ms)
model4_2 <- lm(msrs_total ~ education, data=pnq_ms)
model5_2 <- lm(msrs_total ~ employment, data=pnq_ms)
model6_2 <- lm(msrs_total ~ occupation, data=pnq_ms)
model7_2 <- lm(msrs_total ~ income, data=pnq_ms)
model8_2 <- lm(msrs_total ~ married, data=pnq_ms)
model9_2 <- lm(msrs_total ~ live_alone, data=pnq_ms)
model10_2 <- lm(msrs_total ~ dx_duration, data=pnq_ms)

model1_3 <- lm(promis_t_score ~ age, data=pnq_ms)
model2_3 <- lm(promis_t_score ~ sex, data=pnq_ms)
model3_3 <- lm(promis_t_score ~ race_eth, data=pnq_ms)
model4_3 <- lm(promis_t_score ~ education, data=pnq_ms)
model5_3 <- lm(promis_t_score ~ employment, data=pnq_ms)
model6_3 <- lm(promis_t_score ~ occupation, data=pnq_ms)
model7_3 <- lm(promis_t_score ~ income, data=pnq_ms)
model8_3 <- lm(promis_t_score ~ married, data=pnq_ms)
model9_3 <- lm(promis_t_score ~ live_alone, data=pnq_ms)
model10_3 <- lm(promis_t_score ~ dx_duration, data=pnq_ms)

table_row_0 <- c("Age", "Sex", "Race/Ethnicity", "Education", "Employment", "Occupation", "Income", "Married", "Live Alone", "Disease Duration")
table_col_0 <- c("Metric","PDDS", "MSRSR", "PROMIS")

table_pdds_corr <- c(summary(model1_1)$r.squared, summary(model2_1)$r.squared, summary(model3_1)$r.squared, summary(model4_1)$r.squared, summary(model5_1)$r.squared, summary(model6_1)$r.squared, summary(model7_1)$r.squared, summary(model8_1)$r.squared, summary(model9_1)$r.squared, summary(model10_1)$r.squared)
table_msrsr_corr <- c(summary(model1_2)$r.squared, summary(model2_2)$r.squared, summary(model3_2)$r.squared, summary(model4_2)$r.squared, summary(model5_2)$r.squared, summary(model6_2)$r.squared, summary(model7_2)$r.squared, summary(model8_2)$r.squared, summary(model9_2)$r.squared, summary(model10_2)$r.squared)
table_promis_corr <- c(summary(model1_3)$r.squared, summary(model2_3)$r.squared, summary(model3_3)$r.squared, summary(model4_3)$r.squared, summary(model5_3)$r.squared, summary(model6_3)$r.squared, summary(model7_3)$r.squared, summary(model8_3)$r.squared, summary(model9_3)$r.squared, summary(model10_3)$r.squared)

table_0 <- data.table(table_row_0, table_pdds_corr, table_msrsr_corr, table_promis_corr)
rownames(table_0) <- table_row_0
colnames(table_0) <- table_col_0

table_0_mat <- as.matrix(table_0[,-c(1)])
rownames(table_0_mat) <- table_row_0

table_pdds_pval <- c(summary(model1_1)$coef[2,4], summary(model2_1)$coef[2,4], summary(model3_1)$coef[2,4], summary(model4_1)$coef[2,4], summary(model5_1)$coef[2,4], summary(model6_1)$coef[2,4], summary(model7_1)$coef[2,4], summary(model8_1)$coef[2,4], summary(model9_1)$coef[2,4], summary(model10_1)$coef[2,4])
table_msrsr_pval <- c(summary(model1_2)$coef[2,4], summary(model2_2)$coef[2,4], summary(model3_2)$coef[2,4], summary(model4_2)$coef[2,4], summary(model5_2)$coef[2,4], summary(model6_2)$coef[2,4], summary(model7_2)$coef[2,4], summary(model8_2)$coef[2,4], summary(model9_2)$coef[2,4], summary(model10_2)$coef[2,4])
table_promis_pval <- c(summary(model1_3)$coef[2,4], summary(model2_3)$coef[2,4], summary(model3_3)$coef[2,4], summary(model4_3)$coef[2,4], summary(model5_3)$coef[2,4], summary(model6_3)$coef[2,4], summary(model7_3)$coef[2,4], summary(model8_3)$coef[2,4], summary(model9_3)$coef[2,4], summary(model10_3)$coef[2,4])

table_pdds_n <- c(nobs(model1_1), nobs(model2_1), nobs(model3_1), nobs(model4_1), nobs(model5_1), nobs(model6_1), nobs(model7_1), nobs(model8_1), nobs(model9_1), nobs(model10_1))
table_msrsr_n <- c(nobs(model1_2), nobs(model2_2), nobs(model3_2), nobs(model4_2), nobs(model5_2), nobs(model6_2), nobs(model7_2), nobs(model8_2), nobs(model9_2), nobs(model10_2))
table_promis_n <- c(nobs(model1_3), nobs(model2_3), nobs(model3_3), nobs(model4_3), nobs(model5_3), nobs(model6_3), nobs(model7_3), nobs(model8_3), nobs(model9_3), nobs(model10_3))

table_01 <- data.table(table_row_0, table_pdds_n, table_pdds_corr, table_pdds_pval, table_msrsr_n, table_msrsr_corr, table_msrsr_pval, table_promis_n, table_promis_corr, table_promis_pval)
rownames(table_01) <- table_row_0
colnames(table_01) <- c("Metric","PDDS N", "PDDS R^2", "PDDS p val", "MSRSR N", "MSRSR R^2", "MSRSR p val", "PROMIS N", "PROMIS R^2", "PROMIS p val")

library(pheatmap)
tiff(filename = "Covariate Heatmap.tiff", width = 6, height = 5, units="in", compression = "lzw", res=300)
col.pal <- c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#20A387FF")
pheatmap(table_0_mat, cluster_cols = F, cluster_rows = F, cexRow = 1, cexCol = 1, angle_col = 0, display_numbers = TRUE, color = col.pal)
dev.off()

# 3.	Linear regression of patient reported outcomes adjusting for age, disease duration, employment and income

#PDDS
#Network Size
model1.1 <- lm(fdr ~ network_size + age + dx_duration + employment + income, data=pnq_ms)
#Density
model2.1 <- lm(fdr ~ density + age + dx_duration + employment + income, data=pnq_ms)
#Constraint
model3.1 <- lm(fdr ~ constraint + age + dx_duration + employment + income, data=pnq_ms)
#Effective Size
model4.1 <- lm(fdr ~ effsize + age + dx_duration + employment + income, data=pnq_ms)
#Maximum Degree
model5.1 <- lm(fdr ~ max_degree + age + dx_duration + employment + income, data=pnq_ms)
#Mean Degree
model6.1 <- lm(fdr ~ mean_degree + age + dx_duration + employment + income, data=pnq_ms)
#Percent Kin
pnq_ms[is.infinite(pnq_ms$kin_prop),]$kin_prop <- NA
model7.1 <- lm(fdr ~ kin_prop + age + dx_duration + employment + income, data=pnq_ms)
#Standard deviation of age
model8.1 <- lm(fdr ~ age_sd + age + dx_duration + employment + income, data=pnq_ms)
#Diversity of Sex
model9.1 <- lm(fdr ~ IQVsex + age + dx_duration + employment + income, data=pnq_ms)
#Diversity of Race
model10.1 <- lm(fdr ~ IQVrace + age + dx_duration + employment + income, data=pnq_ms)
#Percent contacted weekly or less
pnq_ms[is.infinite(pnq_ms$weak_freq_prop),]$weak_freq_prop <- NA
pnq_ms$weak_freq_prop <- as.numeric(pnq_ms$weak_freq_prop)
model11.1 <- lm(fdr ~ weak_freq_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent known for less than 6 years
pnq_ms[is.infinite(pnq_ms$weak_dur_prop),]$weak_dur_prop <- NA
model12.1 <- lm(fdr ~ weak_dur_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who live over 15 miles away
pnq_ms[is.infinite(pnq_ms$far_dist_prop),]$far_dist_prop <- NA
model13.1 <- lm(fdr ~ far_dist_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who drink
pnq_ms[is.infinite(pnq_ms$drinking_prop),]$drinking_prop <- NA
model14.1 <- lm(fdr ~ drinking_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who smoke
pnq_ms[is.infinite(pnq_ms$smoking_prop),]$smoking_prop <- NA
model15.1 <- lm(fdr ~ smoking_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent non exercisers
pnq_ms[is.infinite(pnq_ms$no_exercise_prop),]$no_exercise_prop <- NA
model16.1 <- lm(fdr ~ no_exercise_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent bad diet
pnq_ms[is.infinite(pnq_ms$bad_diet_prop),]$bad_diet_prop <- NA
model17.1 <- lm(fdr ~ bad_diet_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who have a negative health influence
pnq_ms[is.infinite(pnq_ms$health_prob_prop),]$health_prob_prop <- NA
model18.1 <- lm(fdr ~ health_prob_prop + age + dx_duration + employment + income, data=pnq_ms)

#MSRSR
#Network Size
model19.1 <- lm(msrs_total ~ network_size + age + dx_duration + employment + income, data=pnq_ms)
#Density
model20.1 <- lm(msrs_total ~ density + age + dx_duration + employment + income, data=pnq_ms)
#Constraint
model21.1 <- lm(msrs_total ~ constraint + age + dx_duration + employment + income, data=pnq_ms)
#Effective Size
model22.1 <- lm(msrs_total ~ effsize + age + dx_duration + employment + income, data=pnq_ms)
#Maximum Degree
model23.1 <- lm(msrs_total ~ max_degree + age + dx_duration + employment + income, data=pnq_ms)
#Mean Degree
model24.1 <- lm(msrs_total ~ mean_degree + age + dx_duration + employment + income, data=pnq_ms)
#Percent Kin
model25.1 <- lm(msrs_total ~ kin_prop + age + dx_duration + employment + income, data=pnq_ms)
#Standard deviation of age
model26.1 <- lm(msrs_total ~ age_sd + age + dx_duration + employment + income, data=pnq_ms)
#Diversity of Sex
model27.1 <- lm(msrs_total ~ IQVsex + age + dx_duration + employment + income, data=pnq_ms)
#Diversity of Race
model28.1 <- lm(msrs_total ~ IQVrace + age + dx_duration + employment + income, data=pnq_ms)
#Percent contacted weekly or less
model29.1 <- lm(msrs_total ~ weak_freq_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent known for less than 6 years
model30.1 <- lm(msrs_total ~ weak_dur_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who live over 15 miles away
model31.1 <- lm(msrs_total ~ far_dist_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who drink
model32.1 <- lm(msrs_total ~ drinking_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who smoke
model33.1 <- lm(msrs_total ~ smoking_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent non exercisers
model34.1 <- lm(msrs_total ~ no_exercise_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent bad diet
model35.1 <- lm(msrs_total ~ bad_diet_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who have a negative health influence
model36.1 <- lm(msrs_total ~ health_prob_prop + age + dx_duration + employment + income, data=pnq_ms)

#PROMIS
#Network Size
model37.1 <- lm(promis_t_score ~ network_size + age + dx_duration + employment + income, data=pnq_ms)
#Density
model38.1 <- lm(promis_t_score ~ density + age + dx_duration + employment + income, data=pnq_ms)
#Constraint
model39.1 <- lm(promis_t_score ~ constraint + age + dx_duration + employment + income, data=pnq_ms)
#Effective Size
model40.1 <- lm(promis_t_score ~ effsize + age + dx_duration + employment + income, data=pnq_ms)
#Maximum Degree
model41.1 <- lm(promis_t_score ~ max_degree + age + dx_duration + employment + income, data=pnq_ms)
#Mean Degree
model42.1 <- lm(promis_t_score ~ mean_degree + age + dx_duration + employment + income, data=pnq_ms)
#Percent Kin
model43.1 <- lm(promis_t_score ~ kin_prop + age + dx_duration + employment + income, data=pnq_ms)
#Standard deviation of age
model44.1 <- lm(promis_t_score ~ age_sd + age + dx_duration + employment + income, data=pnq_ms)
#Diversity of Sex
model45.1 <- lm(promis_t_score ~ IQVsex + age + dx_duration + employment + income, data=pnq_ms)
#Diversity of Race
model46.1 <- lm(promis_t_score ~ IQVrace + age + dx_duration + employment + income, data=pnq_ms)
#Percent contacted weekly or less
model47.1 <- lm(promis_t_score ~ weak_freq_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent known for less than 6 years
model48.1 <- lm(promis_t_score ~ weak_dur_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who live over 15 miles away
model49.1 <- lm(promis_t_score ~ far_dist_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who drink
model50.1 <- lm(promis_t_score ~ drinking_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who smoke
model51.1 <- lm(promis_t_score ~ smoking_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent non exercisers
model52.1 <- lm(promis_t_score ~ no_exercise_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent bad diet
model53.1 <- lm(promis_t_score ~ bad_diet_prop + age + dx_duration + employment + income, data=pnq_ms)
#Percent who have a negative health influence
model54.1 <- lm(promis_t_score ~ health_prob_prop + age + dx_duration + employment + income, data=pnq_ms)

table_col_1 <- c("PDDS Beta MS", "PDDS 95% CI MS (lower)", "PDDS 95% CI MS (upper)", "PDDS p value MS", "MSRSR Beta MS", "MSRSR 95% CI MS (lower)", "MSRSR 95% CI MS (upper)", "MSRSR p value MS", "PROMIS Beta MS", "PROMIS 95% CI MS (lower)", "PROMIS 95% CI MS (upper)", "PROMIS p value MS")
table_row_1 <- c("Size", "Density", "Constraint", "Effective Size", "Maximum Degree", "Mean Degree", "Percent Kin", "SD Age", "Diversity of Sex", "Diversity of Race", "Percent contacted weekly or less", "Percent known for less than 6 years", "Percent who live over 15 miles away", "Percent who drink", "Percent who smoke", "Percent non exercisers", "Percent bad diet", "Percent who have a negative health influence")

table_pdds_beta_ms_1 <- c(summary(model1.1)$coef[2,1], summary(model2.1)$coef[2,1], summary(model3.1)$coef[2,1], summary(model4.1)$coef[2,1], summary(model5.1)$coef[2,1], summary(model6.1)$coef[2,1], summary(model7.1)$coef[2,1], summary(model8.1)$coef[2,1], summary(model9.1)$coef[2,1], summary(model10.1)$coef[2,1], summary(model11.1)$coef[2,1], summary(model12.1)$coef[2,1], summary(model13.1)$coef[2,1], summary(model14.1)$coef[2,1], summary(model15.1)$coef[2,1], summary(model16.1)$coef[2,1], summary(model17.1)$coef[2,1], summary(model18.1)$coef[2,1])
table_msrsr_beta_ms_1 <- c(summary(model19.1)$coef[2,1], summary(model20.1)$coef[2,1], summary(model21.1)$coef[2,1], summary(model22.1)$coef[2,1],summary(model23.1)$coef[2,1], summary(model24.1)$coef[2,1], summary(model25.1)$coef[2,1], summary(model26.1)$coef[2,1], summary(model27.1)$coef[2,1], summary(model28.1)$coef[2,1], summary(model29.1)$coef[2,1], summary(model30.1)$coef[2,1], summary(model31.1)$coef[2,1], summary(model32.1)$coef[2,1], summary(model33.1)$coef[2,1], summary(model34.1)$coef[2,1], summary(model35.1)$coef[2,1], summary(model36.1)$coef[2,1])
table_promis_beta_ms_1 <- c(summary(model37.1)$coef[2,1],summary(model38.1)$coef[2,1],summary(model39.1)$coef[2,1],summary(model40.1)$coef[2,1], summary(model41.1)$coef[2,1],summary(model42.1)$coef[2,1],summary(model43.1)$coef[2,1],summary(model44.1)$coef[2,1], summary(model45.1)$coef[2,1],summary(model46.1)$coef[2,1],summary(model47.1)$coef[2,1],summary(model48.1)$coef[2,1], summary(model49.1)$coef[2,1],summary(model50.1)$coef[2,1],summary(model51.1)$coef[2,1],summary(model52.1)$coef[2,1], summary(model53.1)$coef[2,1],summary(model54.1)$coef[2,1])

table_pdds_95_ms_1 <- c(confint(model1.1, "network_size")[1], confint(model2.1, "density")[1], confint(model3.1, "constraint")[1], confint(model4.1, "effsize")[1], confint(model5.1, "max_degree")[1], confint(model6.1, "mean_degree")[1], confint(model7.1, "kin_prop")[1], confint(model8.1, "age_sd")[1], confint(model9.1, "IQVsex")[1], confint(model10.1, "IQVrace")[1], confint(model11.1, "weak_freq_prop")[1], confint(model12.1, "weak_dur_prop")[1], confint(model13.1, "far_dist_prop")[1], confint(model14.1, "drinking_prop")[1], confint(model15.1, "smoking_prop")[1], confint(model16.1, "no_exercise_prop")[1], confint(model17.1, "bad_diet_prop")[1], confint(model18.1, "health_prob_prop")[1])
table_pdds_95_ms_2 <- c(confint(model1.1, "network_size")[2], confint(model2.1, "density")[2], confint(model3.1, "constraint")[2], confint(model4.1, "effsize")[2], confint(model5.1, "max_degree")[2], confint(model6.1, "mean_degree")[2], confint(model7.1, "kin_prop")[2], confint(model8.1, "age_sd")[2], confint(model9.1, "IQVsex")[2], confint(model10.1, "IQVrace")[2], confint(model11.1, "weak_freq_prop")[2], confint(model12.1, "weak_dur_prop")[2], confint(model13.1, "far_dist_prop")[2], confint(model14.1, "drinking_prop")[2], confint(model15.1, "smoking_prop")[2], confint(model16.1, "no_exercise_prop")[2], confint(model17.1, "bad_diet_prop")[2], confint(model18.1, "health_prob_prop")[2])

table_msrsr_95_ms_1 <- c(confint(model19.1, "network_size")[1], confint(model20.1, "density")[1], confint(model21.1, "constraint")[1], confint(model22.1, "effsize")[1], confint(model23.1, "max_degree")[1], confint(model24.1, "mean_degree")[1], confint(model25.1, "kin_prop")[1], confint(model26.1, "age_sd")[1], confint(model27.1, "IQVsex")[1], confint(model28.1, "IQVrace")[1], confint(model29.1, "weak_freq_prop")[1], confint(model30.1, "weak_dur_prop")[1], confint(model31.1, "far_dist_prop")[1], confint(model32.1, "drinking_prop")[1], confint(model33.1, "smoking_prop")[1], confint(model34.1, "no_exercise_prop")[1], confint(model35.1, "bad_diet_prop")[1], confint(model36.1, "health_prob_prop")[1])
table_msrsr_95_ms_2 <- c(confint(model19.1, "network_size")[2], confint(model20.1, "density")[2], confint(model21.1, "constraint")[2], confint(model22.1, "effsize")[2], confint(model23.1, "max_degree")[2], confint(model24.1, "mean_degree")[2], confint(model25.1, "kin_prop")[2], confint(model26.1, "age_sd")[2], confint(model27.1, "IQVsex")[2], confint(model28.1, "IQVrace")[2], confint(model29.1, "weak_freq_prop")[2], confint(model30.1, "weak_dur_prop")[2], confint(model31.1, "far_dist_prop")[2], confint(model32.1, "drinking_prop")[2], confint(model33.1, "smoking_prop")[2], confint(model34.1, "no_exercise_prop")[2], confint(model35.1, "bad_diet_prop")[2], confint(model36.1, "health_prob_prop")[2])

table_promis_95_ms_1 <- c(confint(model37.1, "network_size")[1], confint(model38.1, "density")[1], confint(model39.1, "constraint")[1], confint(model40.1, "effsize")[1], confint(model41.1, "max_degree")[1], confint(model42.1, "mean_degree")[1], confint(model43.1, "kin_prop")[1], confint(model44.1, "age_sd")[1], confint(model45.1, "IQVsex")[1], confint(model46.1, "IQVrace")[1], confint(model47.1, "weak_freq_prop")[1], confint(model48.1, "weak_dur_prop")[1], confint(model49.1, "far_dist_prop")[1], confint(model50.1, "drinking_prop")[1], confint(model51.1, "smoking_prop")[1], confint(model52.1, "no_exercise_prop")[1], confint(model53.1, "bad_diet_prop")[1], confint(model54.1, "health_prob_prop")[1])
table_promis_95_ms_2 <- c(confint(model37.1, "network_size")[2], confint(model38.1, "density")[2], confint(model39.1, "constraint")[2], confint(model40.1, "effsize")[2], confint(model41.1, "max_degree")[2], confint(model42.1, "mean_degree")[2], confint(model43.1, "kin_prop")[2], confint(model44.1, "age_sd")[2], confint(model45.1, "IQVsex")[2], confint(model46.1, "IQVrace")[2], confint(model47.1, "weak_freq_prop")[2], confint(model48.1, "weak_dur_prop")[2], confint(model49.1, "far_dist_prop")[2], confint(model50.1, "drinking_prop")[2], confint(model51.1, "smoking_prop")[2], confint(model52.1, "no_exercise_prop")[2], confint(model53.1, "bad_diet_prop")[2], confint(model54.1, "health_prob_prop")[2])

table_pdds_pval_ms_1 <- c(summary(model1.1)$coef[2,4], summary(model2.1)$coef[2,4], summary(model3.1)$coef[2,4], summary(model4.1)$coef[2,4], summary(model5.1)$coef[2,4], summary(model6.1)$coef[2,4], summary(model7.1)$coef[2,4], summary(model8.1)$coef[2,4], summary(model9.1)$coef[2,4], summary(model10.1)$coef[2,4], summary(model11.1)$coef[2,4], summary(model12.1)$coef[2,4], summary(model13.1)$coef[2,4], summary(model14.1)$coef[2,4], summary(model15.1)$coef[2,4], summary(model16.1)$coef[2,4], summary(model17.1)$coef[2,4], summary(model18.1)$coef[2,4])
table_msrsr_pval_ms_1 <- c(summary(model19.1)$coef[2,4], summary(model20.1)$coef[2,4], summary(model21.1)$coef[2,4], summary(model22.1)$coef[2,4],summary(model23.1)$coef[2,4], summary(model24.1)$coef[2,4], summary(model25.1)$coef[2,4], summary(model26.1)$coef[2,4], summary(model27.1)$coef[2,4], summary(model28.1)$coef[2,4], summary(model29.1)$coef[2,4], summary(model30.1)$coef[2,4], summary(model31.1)$coef[2,4], summary(model32.1)$coef[2,4], summary(model33.1)$coef[2,4], summary(model34.1)$coef[2,4], summary(model35.1)$coef[2,4], summary(model36.1)$coef[2,4])
table_promis_pval_ms_1 <- c(summary(model37.1)$coef[2,4],summary(model38.1)$coef[2,4],summary(model39.1)$coef[2,4],summary(model40.1)$coef[2,4], summary(model41.1)$coef[2,4],summary(model42.1)$coef[2,4],summary(model43.1)$coef[2,4],summary(model44.1)$coef[2,4], summary(model45.1)$coef[2,4],summary(model46.1)$coef[2,4],summary(model47.1)$coef[2,4],summary(model48.1)$coef[2,4], summary(model49.1)$coef[2,4],summary(model50.1)$coef[2,4],summary(model51.1)$coef[2,4],summary(model52.1)$coef[2,4], summary(model53.1)$coef[2,4],summary(model54.1)$coef[2,4])

table_coefs_1 <- data.table(table_pdds_beta_ms_1, table_pdds_95_ms_1, table_pdds_95_ms_2, table_pdds_pval_ms_1, table_msrsr_beta_ms_1, table_msrsr_95_ms_1, table_msrsr_95_ms_2, table_msrsr_pval_ms_1, table_promis_beta_ms_1, table_promis_95_ms_1, table_promis_95_ms_2, table_promis_pval_ms_1)
colnames(table_coefs_1) <- table_col_1
rownames(table_coefs_1) <- table_row_1

table_coefs_1$`Network Feature` <- table_row_1
table_coefs_1 <- table_coefs_1[,c(13, 1:12)]

# 4.	Cross Sectional Analysis of Network and Compositional Variables – Control
# Linear regression of PROMIS physical function adjusting for age, employment and income

#PROMIS
#Network Size
model37_c <- lm(promis_t_score ~ network_size + age + employment + income, data=pnq_control)
#Density
model38_c <- lm(promis_t_score ~ density + age + employment + income, data=pnq_control)
#Constraint
model39_c <- lm(promis_t_score ~ constraint + age + employment + income, data=pnq_control)
#Effective Size
model40_c <- lm(promis_t_score ~ effsize + age + employment + income, data=pnq_control)
#Maximum Degree
model41_c <- lm(promis_t_score ~ max_degree + age + employment + income, data=pnq_control)
#Mean Degree
model42_c <- lm(promis_t_score ~ mean_degree + age + employment + income, data=pnq_control)
#Percent Kin
pnq_control[is.infinite(pnq_control$kin_prop),]$kin_prop <- NA
model43_c <- lm(promis_t_score ~ kin_prop + age + employment + income, data=pnq_control)
#Standard deviation of age
model44_c <- lm(promis_t_score ~ age_sd + age + employment + income, data=pnq_control)
#Diversity of Sex
model45_c <- lm(promis_t_score ~ IQVsex + age + employment + income, data=pnq_control)
#Diversity of Race
model46_c <- lm(promis_t_score ~ IQVrace + age + employment + income, data=pnq_control)
#Percent contacted weekly or less
pnq_control[is.infinite(pnq_control$weak_freq_prop),]$weak_freq_prop <- NA
model47_c <- lm(promis_t_score ~ weak_freq_prop + age + employment + income, data=pnq_control)
#Percent known for less than 6 years
model48_c <- lm(promis_t_score ~ weak_dur_prop + age + employment + income, data=pnq_control)
#Percent who live over 15 miles away
pnq_control[is.infinite(pnq_control$far_dist_prop),]$far_dist_prop <- NA
model49_c <- lm(promis_t_score ~ far_dist_prop + age + employment + income, data=pnq_control)
#Percent who drink
pnq_control[is.infinite(pnq_control$drinking_prop),]$drinking_prop <- NA
model50_c <- lm(promis_t_score ~ drinking_prop + age + employment + income, data=pnq_control)
#Percent who smoke
model51_c <- lm(promis_t_score ~ smoking_prop + age + employment + income, data=pnq_control)
#Percent non exercisers
pnq_control[is.infinite(pnq_control$no_exercise_prop),]$no_exercise_prop <- NA
model52_c <- lm(promis_t_score ~ no_exercise_prop + age + employment + income, data=pnq_control)
#Percent bad diet
pnq_control[is.infinite(pnq_control$bad_diet_prop),]$bad_diet_prop <- NA
model53_c <- lm(promis_t_score ~ bad_diet_prop + age + employment + income, data=pnq_control)
#Percent who have a negative health influence
pnq_control[is.infinite(pnq_control$health_prob_prop),]$health_prob_prop <- NA
model54_c <- lm(promis_t_score ~ health_prob_prop + age + employment + income, data=pnq_control)

table_col_2 <- c("PROMIS Beta Control", "PROMIS 95% CI Control (lower)", "PROMIS 95% CI Control (upper)", "PROMIS p value Control")

table_promis_beta_control_1 <- c(summary(model37_c)$coef[2,1],summary(model38_c)$coef[2,1],summary(model39_c)$coef[2,1],summary(model40_c)$coef[2,1], summary(model41_c)$coef[2,1],summary(model42_c)$coef[2,1],summary(model43_c)$coef[2,1],summary(model44_c)$coef[2,1], summary(model45_c)$coef[2,1],summary(model46_c)$coef[2,1],summary(model47_c)$coef[2,1],summary(model48_c)$coef[2,1], summary(model49_c)$coef[2,1],summary(model50_c)$coef[2,1],summary(model51_c)$coef[2,1],summary(model52_c)$coef[2,1], summary(model53_c)$coef[2,1],summary(model54_c)$coef[2,1])
table_promis_95_ms_c1 <- c(confint(model37_c, "network_size")[1], confint(model38_c, "density")[1], confint(model39_c, "constraint")[1], confint(model40_c, "effsize")[1], confint(model41_c, "max_degree")[1], confint(model42_c, "mean_degree")[1], confint(model43_c, "kin_prop")[1], confint(model44_c, "age_sd")[1], confint(model45_c, "IQVsex")[1], confint(model46_c, "IQVrace")[1], confint(model47_c, "weak_freq_prop")[1], confint(model48_c, "weak_dur_prop")[1], confint(model49_c, "far_dist_prop")[1], confint(model50_c, "drinking_prop")[1], confint(model51_c, "smoking_prop")[1], confint(model52_c, "no_exercise_prop")[1], confint(model53_c, "bad_diet_prop")[1], confint(model54_c, "health_prob_prop")[1])
table_promis_95_ms_c2 <- c(confint(model37_c, "network_size")[2], confint(model38_c, "density")[2], confint(model39_c, "constraint")[2], confint(model40_c, "effsize")[2], confint(model41_c, "max_degree")[2], confint(model42_c, "mean_degree")[2], confint(model43_c, "kin_prop")[2], confint(model44_c, "age_sd")[2], confint(model45_c, "IQVsex")[2], confint(model46_c, "IQVrace")[2], confint(model47_c, "weak_freq_prop")[2], confint(model48_c, "weak_dur_prop")[2], confint(model49_c, "far_dist_prop")[2], confint(model50_c, "drinking_prop")[2], confint(model51_c, "smoking_prop")[2], confint(model52_c, "no_exercise_prop")[2], confint(model53_c, "bad_diet_prop")[2], confint(model54_c, "health_prob_prop")[2])
table_promis_pval_control_1 <- c(summary(model37_c)$coef[2,4],summary(model38_c)$coef[2,4],summary(model39_c)$coef[2,4],summary(model40_c)$coef[2,4], summary(model41_c)$coef[2,4],summary(model42_c)$coef[2,4],summary(model43_c)$coef[2,4],summary(model44_c)$coef[2,4], summary(model45_c)$coef[2,4],summary(model46_c)$coef[2,4],summary(model47_c)$coef[2,4],summary(model48_c)$coef[2,4], summary(model49_c)$coef[2,4],summary(model50_c)$coef[2,4],summary(model51_c)$coef[2,4],summary(model52_c)$coef[2,4], summary(model53_c)$coef[2,4],summary(model54_c)$coef[2,4])

table_coefs_3 <- data.table(table_promis_beta_control_1, table_promis_95_ms_c1, table_promis_95_ms_c2, table_promis_pval_control_1)
colnames(table_coefs_3) <- table_col_2
rownames(table_coefs_3) <- table_row_1

table_coefs_3$`Network Feature` <- table_row_1
table_coefs_3 <- table_coefs_3[,c(5, 1:4)]

# 5.	Cross Sectional Analysis of Network and Compositional Variables – MS vs Control
# Logistic regression of disease status (MS/Control) adjusting for age, sex, race, education, employment, income

pnq_pandemic$ms <- factor(pnq_pandemic$ms, levels = c("MS", "Control"), labels = c(1,0))

#MS vs Control
#Network Size
model1.3 <- glm(ms ~ network_size + age + employment + income, data=pnq_pandemic, family = binomial)
#Density
model2.3 <- glm(ms ~ density + age + employment + income, data=pnq_pandemic, family = binomial)
#Constraint
model3.3 <- glm(ms ~ constraint + age + employment + income, data=pnq_pandemic, family = binomial)
#Effective Size
model4.3 <- glm(ms ~ effsize + age + employment + income, data=pnq_pandemic, family = binomial)
#Maximum Degree
model5.3 <- glm(ms ~ max_degree + age + employment + income, data=pnq_pandemic, family = binomial)
#Mean Degree
model6.3 <- glm(ms ~ mean_degree + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent Kin
pnq_pandemic[is.infinite(pnq_pandemic$kin_prop),]$kin_prop <- NA
model7.3 <- glm(ms ~ kin_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Standard deviation of age
model8.3 <- glm(ms ~ age_sd + age + employment + income, data=pnq_pandemic, family = binomial)
#Diversity of Sex
model9.3 <- glm(ms ~ IQVsex + age + employment + income, data=pnq_pandemic, family = binomial)
#Diversity of race
model10.3 <- glm(ms ~ IQVrace + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent contacted weekly or less
pnq_pandemic[is.infinite(pnq_pandemic$weak_freq_prop),]$weak_freq_prop <- NA
pnq_pandemic$weak_freq_prop <- as.numeric(pnq_pandemic$weak_freq_prop)
model11.3 <- glm(ms ~ weak_freq_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent known for less than 6 years
pnq_pandemic[is.infinite(pnq_pandemic$weak_dur_prop),]$weak_dur_prop <- NA
model12.3 <- glm(ms ~ weak_dur_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent who live over 15 miles away
pnq_pandemic[is.infinite(pnq_pandemic$far_dist_prop),]$far_dist_prop <- NA
model13.3 <- glm(ms ~ far_dist_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent who drink
pnq_pandemic[is.infinite(pnq_pandemic$drinking_prop),]$drinking_prop <- NA
model14.3 <- glm(ms ~ drinking_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent who smoke
pnq_pandemic[is.infinite(pnq_pandemic$smoking_prop),]$smoking_prop <- NA
model15.3 <- glm(ms ~ smoking_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent non exercisers
pnq_pandemic[is.infinite(pnq_pandemic$no_exercise_prop),]$no_exercise_prop <- NA
model16.3 <- glm(ms ~ no_exercise_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent bad diet
pnq_pandemic[is.infinite(pnq_pandemic$bad_diet_prop),]$bad_diet_prop <- NA
model17.3 <- glm(ms ~ bad_diet_prop + age + employment + income, data=pnq_pandemic, family = binomial)
#Percent who have a negative health influence
pnq_pandemic[is.infinite(pnq_pandemic$health_prob_prop),]$health_prob_prop <- NA
model18.3 <- glm(ms ~ health_prob_prop + age + employment + income, data=pnq_pandemic, family = binomial)

table_col_3 <- c("Beta", "95% CI (lower)", "95% CI (upper)", "p value")

table_glm_beta <- c(summary(model1.3)$coef[2,1], summary(model2.3)$coef[2,1], summary(model3.3)$coef[2,1], summary(model4.3)$coef[2,1], summary(model5.3)$coef[2,1], summary(model6.3)$coef[2,1], summary(model7.3)$coef[2,1], summary(model8.3)$coef[2,1], summary(model9.3)$coef[2,1], summary(model10.3)$coef[2,1], summary(model11.3)$coef[2,1], summary(model12.3)$coef[2,1], summary(model13.3)$coef[2,1], summary(model14.3)$coef[2,1], summary(model15.3)$coef[2,1], summary(model16.3)$coef[2,1], summary(model17.3)$coef[2,1], summary(model18.3)$coef[2,1])
table_glm_95_1 <- suppressWarnings(c(confint(model1.3, "network_size")[1], confint(model2.3, "density")[1], confint(model3.3, "constraint")[1], confint(model4.3, "effsize")[1], confint(model5.3, "max_degree")[1], confint(model6.3, "mean_degree")[1], confint(model7.3, "kin_prop")[1], confint(model8.3, "age_sd")[1], confint(model9.3, "IQVsex")[1], confint(model10.3, "IQVrace")[1], confint(model11.3, "weak_freq_prop")[1], confint(model12.3, "weak_dur_prop")[1], confint(model13.3, "far_dist_prop")[1], confint(model14.3, "drinking_prop")[1], confint(model15.3, "smoking_prop")[1], confint(model16.3, "no_exercise_prop")[1], confint(model17.3, "bad_diet_prop")[1], confint(model18.3, "health_prob_prop")[1]))
table_glm_95_2 <- suppressWarnings(c(confint(model1.3, "network_size")[2], confint(model2.3, "density")[2], confint(model3.3, "constraint")[2], confint(model4.3, "effsize")[2], confint(model5.3, "max_degree")[2], confint(model6.3, "mean_degree")[2], confint(model7.3, "kin_prop")[2], confint(model8.3, "age_sd")[2], confint(model9.3, "IQVsex")[2], confint(model10.3, "IQVrace")[2], confint(model11.3, "weak_freq_prop")[2], confint(model12.3, "weak_dur_prop")[2], confint(model13.3, "far_dist_prop")[2], confint(model14.3, "drinking_prop")[2], confint(model15.3, "smoking_prop")[2], confint(model16.3, "no_exercise_prop")[2], confint(model17.3, "bad_diet_prop")[2], confint(model18.3, "health_prob_prop")[2]))
table_glm_pval <- c(summary(model1.3)$coef[2,4], summary(model2.3)$coef[2,4], summary(model3.3)$coef[2,4], summary(model4.3)$coef[2,4], summary(model5.3)$coef[2,4], summary(model6.3)$coef[2,4], summary(model7.3)$coef[2,4], summary(model8.3)$coef[2,4], summary(model9.3)$coef[2,4], summary(model10.3)$coef[2,4], summary(model11.3)$coef[2,4], summary(model12.3)$coef[2,4], summary(model13.3)$coef[2,4], summary(model14.3)$coef[2,4], summary(model15.3)$coef[2,4], summary(model16.3)$coef[2,4], summary(model17.3)$coef[2,4], summary(model18.3)$coef[2,4])

table_coefs_4 <- data.table(table_glm_beta, table_glm_95_1, table_glm_95_2, table_glm_pval)
colnames(table_coefs_4) <- table_col_3
rownames(table_coefs_4) <- table_row_1

table_coefs_4$`Network Feature` <- table_row_1
table_coefs_4 <- table_coefs_4[,c(5, 1:4)]

# 6.	Comparison of Network and Compositional Variables – MS vs Control

listVars2 <- c("network_size", "density", "constraint", "effsize", "max_degree", "mean_degree", "kin_prop", "age_sd", "IQVsex", "IQVrace", "weak_freq_prop", "weak_dur_prop", "far_dist_prop", "drinking_prop", "smoking_prop", "no_exercise_prop", "bad_diet_prop", "health_prob_prop")
table3 <- suppressWarnings(CreateTableOne(vars = listVars2, strata = c("ms"), data = pnq_pandemic, test = TRUE, includeNA = FALSE))
table3_print <- print(table3, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# Comparison of PROMIS as a function of disease status (MS vs Control)
#Network Size
model1.2 <- glm(promis_t_score ~ network_size + ms + age + employment + income, data=pnq_pandemic)
#Density
model2.2 <- glm(promis_t_score ~ density + ms + age + employment + income, data=pnq_pandemic)
#Constraint
model3.2 <- glm(promis_t_score ~ constraint + ms + age + employment + income, data=pnq_pandemic)
#Effective Size
model4.2 <- glm(promis_t_score ~ effsize + ms + age + employment + income, data=pnq_pandemic)
#Maximum Degree
model5.2 <- glm(promis_t_score ~ max_degree + ms + age + employment + income, data=pnq_pandemic)
#Mean Degree
model6.2 <- glm(promis_t_score ~ mean_degree + ms + age + employment + income, data=pnq_pandemic)
#Percent Kin
model7.2 <- glm(promis_t_score ~ kin_prop + ms + age + employment + income, data=pnq_pandemic)
#Standard deviation of age
model8.2 <- glm(promis_t_score ~ age_sd + ms + age + employment + income, data=pnq_pandemic)
#Diversity of Sex
model9.2 <- glm(promis_t_score ~ IQVsex + ms + age + employment + income, data=pnq_pandemic)
#Diversity of race
model10.2 <- glm(promis_t_score ~ IQVrace + ms + age + employment + income, data=pnq_pandemic)
#Percent contacted weekly or less
model11.2 <- glm(promis_t_score ~ weak_freq_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent known for less than 6 years
model12.2 <- glm(promis_t_score ~ weak_dur_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent who live over 15 miles away
model13.2 <- glm(promis_t_score ~ far_dist_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent who drink
model14.2 <- glm(promis_t_score ~ drinking_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent who smoke
model15.2 <- glm(promis_t_score ~ smoking_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent non exercisers
model16.2 <- glm(promis_t_score ~ no_exercise_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent bad diet
model17.2 <- glm(promis_t_score ~ bad_diet_prop + ms + age + employment + income, data=pnq_pandemic)
#Percent who have a negative health influence
model18.2 <- glm(promis_t_score ~ health_prob_prop + ms + age + employment + income, data=pnq_pandemic)

table_glm_beta_promis <- c(summary(model1.2)$coef[2,1], summary(model2.2)$coef[2,1], summary(model3.2)$coef[2,1], summary(model4.2)$coef[2,1], summary(model5.2)$coef[2,1], summary(model6.2)$coef[2,1], summary(model7.2)$coef[2,1], summary(model8.2)$coef[2,1], summary(model9.2)$coef[2,1], summary(model10.2)$coef[2,1], summary(model11.2)$coef[2,1], summary(model12.2)$coef[2,1], summary(model13.2)$coef[2,1], summary(model14.2)$coef[2,1], summary(model15.2)$coef[2,1], summary(model16.2)$coef[2,1], summary(model17.2)$coef[2,1], summary(model18.2)$coef[2,1])
table_glm_95_1_promis <- suppressWarnings(c(confint(model1.2, "network_size")[1], confint(model2.2, "density")[1], confint(model3.2, "constraint")[1], confint(model4.2, "effsize")[1], confint(model5.2, "max_degree")[1], confint(model6.2, "mean_degree")[1], confint(model7.2, "kin_prop")[1], confint(model8.2, "age_sd")[1], confint(model9.2, "IQVsex")[1], confint(model10.2, "IQVrace")[1], confint(model11.2, "weak_freq_prop")[1], confint(model12.2, "weak_dur_prop")[1], confint(model13.2, "far_dist_prop")[1], confint(model14.2, "drinking_prop")[1], confint(model15.2, "smoking_prop")[1], confint(model16.2, "no_exercise_prop")[1], confint(model17.2, "bad_diet_prop")[1], confint(model18.2, "health_prob_prop")[1]))
table_glm_95_2_promis <- suppressWarnings(c(confint(model1.2, "network_size")[2], confint(model2.2, "density")[2], confint(model3.2, "constraint")[2], confint(model4.2, "effsize")[2], confint(model5.2, "max_degree")[2], confint(model6.2, "mean_degree")[2], confint(model7.2, "kin_prop")[2], confint(model8.2, "age_sd")[2], confint(model9.2, "IQVsex")[2], confint(model10.2, "IQVrace")[2], confint(model11.2, "weak_freq_prop")[2], confint(model12.2, "weak_dur_prop")[2], confint(model13.2, "far_dist_prop")[2], confint(model14.2, "drinking_prop")[2], confint(model15.2, "smoking_prop")[2], confint(model16.2, "no_exercise_prop")[2], confint(model17.2, "bad_diet_prop")[2], confint(model18.2, "health_prob_prop")[2]))
table_glm_pval_promis <- c(summary(model1.2)$coef[2,4], summary(model2.2)$coef[2,4], summary(model3.2)$coef[2,4], summary(model4.2)$coef[2,4], summary(model5.2)$coef[2,4], summary(model6.2)$coef[2,4], summary(model7.2)$coef[2,4], summary(model8.2)$coef[2,4], summary(model9.2)$coef[2,4], summary(model10.2)$coef[2,4], summary(model11.2)$coef[2,4], summary(model12.2)$coef[2,4], summary(model13.2)$coef[2,4], summary(model14.2)$coef[2,4], summary(model15.2)$coef[2,4], summary(model16.2)$coef[2,4], summary(model17.2)$coef[2,4], summary(model18.2)$coef[2,4])

table_coefs_2 <- data.table(table_glm_beta_promis, table_glm_95_1_promis, table_glm_95_2_promis, table_glm_pval_promis)
colnames(table_coefs_2) <- table_col_3
rownames(table_coefs_2) <- table_row_1

table_coefs_2$`Network Feature` <- table_row_1
table_coefs_2 <- table_coefs_2[,c(5, 1:4)]

# Does MS status mediate the relationship between structural network/compositional variables and PROs?

library(lavaan)
library(semPlot)

model1.9<-'ms~a*network_size
promis_t_score~b*ms
promis_t_score~c*network_size
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit1.9<-sem(model1.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit1.9,standardized=TRUE)

model2.9<-'ms~a*density
promis_t_score~b*ms
promis_t_score~c*density
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit2.9<-sem(model2.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit2.9,standardized=TRUE)

model3.9<-'ms~a*constraint
promis_t_score~b*ms
promis_t_score~c*constraint
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit3.9<-sem(model3.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit3.9,standardized=TRUE)

model4.9<-'ms~a*effsize
promis_t_score~b*ms
promis_t_score~c*effsize
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit4.9<-sem(model4.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit4.9,standardized=TRUE)

model5.9<-'ms~a*max_degree
promis_t_score~b*ms
promis_t_score~c*max_degree
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit5.9<-sem(model5.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit5.9,standardized=TRUE)

model6.9<-'ms~a*mean_degree
promis_t_score~b*ms
promis_t_score~c*mean_degree
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit6.9<-sem(model6.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit6.9,standardized=TRUE)

model7.9<-'ms~a*kin_prop
promis_t_score~b*ms
promis_t_score~c*kin_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit7.9<-sem(model7.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit7.9,standardized=TRUE)

model8.9<-'ms~a*age_sd
promis_t_score~b*ms
promis_t_score~c*age_sd
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit8.9<-sem(model8.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit8.9,standardized=TRUE)

model9.9<-'ms~a*IQVsex
promis_t_score~b*ms
promis_t_score~c*IQVsex
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit9.9<-sem(model9.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit9.9,standardized=TRUE)

model10.9<-'ms~a*IQVrace
promis_t_score~b*ms
promis_t_score~c*IQVrace
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit10.9<-sem(model10.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit10.9,standardized=TRUE)

model11.9<-'ms~a*weak_freq_prop
promis_t_score~b*ms
promis_t_score~c*weak_freq_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit11.9<-sem(model11.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit11.9,standardized=TRUE)

model12.9<-'ms~a*weak_dur_prop
promis_t_score~b*ms
promis_t_score~c*weak_dur_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit12.9<-sem(model12.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit12.9,standardized=TRUE)

model13.9<-'ms~a*far_dist_prop
promis_t_score~b*ms
promis_t_score~c*far_dist_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit13.9<-sem(model13.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit13.9,standardized=TRUE)

model14.9<-'ms~a*drinking_prop
promis_t_score~b*ms
promis_t_score~c*drinking_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit14.9<-sem(model14.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit14.9,standardized=TRUE)

model15.9<-'ms~a*drinking_prop
promis_t_score~b*ms
promis_t_score~c*drinking_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit15.9<-sem(model15.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit15.9,standardized=TRUE)

model16.9<-'ms~a*smoking_prop
promis_t_score~b*ms
promis_t_score~c*smoking_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit16.9<-sem(model16.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit16.9,standardized=TRUE)

model17.9<-'ms~a*no_exercise_prop
promis_t_score~b*ms
promis_t_score~c*no_exercise_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit17.9<-sem(model17.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit17.9,standardized=TRUE)

model18.9<-'ms~a*bad_diet_prop
promis_t_score~b*ms
promis_t_score~c*bad_diet_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit18.9<-sem(model18.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit18.9,standardized=TRUE)

model19.9<-'ms~a*health_prob_prop
promis_t_score~b*ms
promis_t_score~c*health_prob_prop
#indirect effect
ab:=a*b
#total effect
total:=c+(a*b)'
fit19.9<-sem(model19.9,data=pnq_pandemic,ordered=c("ms"))
parameterEstimates(fit19.9,standardized=TRUE)

# LONGITUDINAL ANALYSIS

# 7.	Longitudinal Analysis – Individual Paired T Tests for Network and Compositional variables pre-pandemic and during pandemic – MS vs Control

pnq_all_two_sample <- fread("Longitudinal PNQ Two Sample.csv")

#Create table of network metrics stratified by MS diagnosis and pandemic status
pnq_all_two_sample_ms <- pnq_all_two_sample %>% filter(pnq_all_two_sample$ms == "MS")
pnq_all_two_sample_control <- pnq_all_two_sample %>% filter(pnq_all_two_sample$ms == "Control")

table4 <- CreateTableOne(vars = listVars2, strata = c("pandemic"), data = pnq_all_two_sample_ms, test = TRUE, includeNA = FALSE)
table4_print <- print(table4, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

table5 <- CreateTableOne(vars = listVars2, strata = c("pandemic"), data = pnq_all_two_sample_control, test = TRUE, includeNA = FALSE)
table5_print <- print(table5, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

# 8.	Longitudinal Analysis – Change in Network and Compositional variables from pre-pandemic to during pandemic vs Neurological Outcomes (most recent)

pnq_all_time <- fread("PNQ Change.csv")
pnq_all_time_ms <- pnq_all_time %>% filter(pnq_all_time$ms == "MS")
pnq_all_time_control <- pnq_all_time %>% filter(pnq_all_time$ms == "Control")

#All covariates

#PDDS
#Network Size
model1.4 <- lm(fdr ~ network_size_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Density
model2.4 <- lm(fdr ~ density_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Constraint
model3.4 <- lm(fdr ~ constraint_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Effective Size
model4.4 <- lm(fdr ~ effsize_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Maximum Degree
model5.4 <- lm(fdr ~ max_degree_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Mean Degree
model6.4 <- lm(fdr ~ mean_degree_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent Kin
pnq_all_time_ms[is.infinite(pnq_all_time_ms$kin_prop_change),]$kin_prop_change <- NA
model7.4 <- lm(fdr ~ kin_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Standard deviation of age
model8.4 <- lm(fdr ~ age_sd_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model9.4 <- lm(fdr ~ IQVsex_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Race
model10.4 <- lm(fdr ~ IQVrace_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent contacted weekly or less
pnq_all_time_ms[is.infinite(pnq_all_time_ms$weak_freq_prop_change),]$weak_freq_prop_change <- NA
pnq_all_time_ms$weak_freq_prop_change <- as.numeric(pnq_all_time_ms$weak_freq_prop_change)
model11.4 <- lm(fdr ~ weak_freq_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent known for less than 6 years
pnq_all_time_ms[is.infinite(pnq_all_time_ms$weak_dur_prop_change),]$weak_dur_prop_change <- NA
model12.4 <- lm(fdr ~ weak_dur_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who live over 15 miles away
pnq_all_time_ms[is.infinite(pnq_all_time_ms$far_dist_prop_change),]$far_dist_prop_change <- NA
model13.4 <- lm(fdr ~ far_dist_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who drink
pnq_all_time_ms[is.infinite(pnq_all_time_ms$drinking_prop_change),]$drinking_prop_change <- NA
model14.4 <- lm(fdr ~ drinking_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who smoke
pnq_all_time_ms[is.infinite(pnq_all_time_ms$smoking_prop_change),]$smoking_prop_change <- NA
model15.4 <- lm(fdr ~ smoking_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent non exercisers
pnq_all_time_ms[is.infinite(pnq_all_time_ms$no_exercise_prop_change),]$no_exercise_prop_change <- NA
model16.4 <- lm(fdr ~ no_exercise_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent bad diet
pnq_all_time_ms[is.infinite(pnq_all_time_ms$bad_diet_prop_change),]$bad_diet_prop_change <- NA
model17.4 <- lm(fdr ~ bad_diet_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who have a negative health influence
pnq_all_time_ms[is.infinite(pnq_all_time_ms$health_prob_prop_change),]$health_prob_prop_change <- NA
model18.4 <- lm(fdr ~ health_prob_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)

#MSRSR
#Network Size
model19.4 <- lm(msrs_total ~ network_size_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Density
model20.4 <- lm(msrs_total ~ density_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Constraint
model21.4 <- lm(msrs_total ~ constraint_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Effective Size
model22.4 <- lm(msrs_total ~ effsize_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Maximum Degree
model23.4 <- lm(msrs_total ~ max_degree_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Mean Degree
model24.4 <- lm(msrs_total ~ mean_degree_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent Kin
model25.4 <- lm(msrs_total ~ kin_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Standard deviation of age
model26.4 <- lm(msrs_total ~ age_sd_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model27.4 <- lm(msrs_total ~ IQVsex_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Race
model28.4 <- lm(msrs_total ~ IQVrace_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent contacted weekly or less
model29.4 <- lm(msrs_total ~ weak_freq_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent known for less than 6 years
model30.4 <- lm(msrs_total ~ weak_dur_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who live over 15 miles away
model31.4 <- lm(msrs_total ~ far_dist_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who drink
model32.4 <- lm(msrs_total ~ drinking_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who smoke
model33.4 <- lm(msrs_total ~ smoking_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent non exercisers
model34.4 <- lm(msrs_total ~ no_exercise_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent bad diet
model35.4 <- lm(msrs_total ~ bad_diet_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who have a negative health influence
model36.4 <- lm(msrs_total ~ health_prob_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)

#PROMIS
#Network Size
model37.4 <- lm(promis_t_score ~ network_size_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Density
model38.4 <- lm(promis_t_score ~ density_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Constraint
model39.4 <- lm(promis_t_score ~ constraint_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Effective Size
model40.4 <- lm(promis_t_score ~ effsize_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Maximum Degree
model41.4 <- lm(promis_t_score ~ max_degree_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Mean Degree
model42.4 <- lm(promis_t_score ~ mean_degree_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent Kin
model43.4 <- lm(promis_t_score ~ kin_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Standard deviation of age
model44.4 <- lm(promis_t_score ~ age_sd_change + age + sex + education + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model45.4 <- lm(promis_t_score ~ IQVsex_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Race
model46.4 <- lm(promis_t_score ~ IQVrace_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent contacted weekly or less
model47.4 <- lm(promis_t_score ~ weak_freq_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent known for less than 6 years
model48.4 <- lm(promis_t_score ~ weak_dur_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who live over 15 miles away
model49.4 <- lm(promis_t_score ~ far_dist_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who drink
model50.4 <- lm(promis_t_score ~ drinking_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who smoke
model51.4 <- lm(promis_t_score ~ smoking_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent non exercisers
model52.4 <- lm(promis_t_score ~ no_exercise_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent bad diet
model53.4 <- lm(promis_t_score ~ bad_diet_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Percent who have a negative health influence
model54.4 <- lm(promis_t_score ~ health_prob_prop_change + age + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)

table_pdds_beta_ms_time <- c(summary(model1.4)$coef[2,1], summary(model2.4)$coef[2,1], summary(model3.4)$coef[2,1], summary(model4.4)$coef[2,1], summary(model5.4)$coef[2,1], summary(model6.4)$coef[2,1], summary(model7.4)$coef[2,1], summary(model8.4)$coef[2,1], summary(model9.4)$coef[2,1], summary(model10.4)$coef[2,1], summary(model11.4)$coef[2,1], summary(model12.4)$coef[2,1], summary(model13.4)$coef[2,1], summary(model14.4)$coef[2,1], summary(model15.4)$coef[2,1], summary(model16.4)$coef[2,1], summary(model17.4)$coef[2,1], summary(model18.4)$coef[2,1])
table_msrsr_beta_ms_time <- c(summary(model19.4)$coef[2,1], summary(model20.4)$coef[2,1], summary(model21.4)$coef[2,1], summary(model22.4)$coef[2,1],summary(model23.4)$coef[2,1], summary(model24.4)$coef[2,1], summary(model25.4)$coef[2,1], summary(model26.4)$coef[2,1], summary(model27.4)$coef[2,1], summary(model28.4)$coef[2,1], summary(model29.4)$coef[2,1], summary(model30.4)$coef[2,1], summary(model31.4)$coef[2,1], summary(model32.4)$coef[2,1], summary(model33.4)$coef[2,1], summary(model34.4)$coef[2,1], summary(model35.4)$coef[2,1], summary(model36.4)$coef[2,1])
table_promis_beta_ms_time <- c(summary(model37.4)$coef[2,1],summary(model38.4)$coef[2,1],summary(model39.4)$coef[2,1],summary(model40.4)$coef[2,1], summary(model41.4)$coef[2,1],summary(model42.4)$coef[2,1],summary(model43.4)$coef[2,1],summary(model44.4)$coef[2,1], summary(model45.4)$coef[2,1],summary(model46.4)$coef[2,1],summary(model47.4)$coef[2,1],summary(model48.4)$coef[2,1], summary(model49.4)$coef[2,1],summary(model50.4)$coef[2,1],summary(model51.4)$coef[2,1],summary(model52.4)$coef[2,1], summary(model53.4)$coef[2,1],summary(model54.4)$coef[2,1])

table_pdds_ci_ms_time_1 <- c(confint(model1.4, "network_size_change")[1], confint(model2.4, "density_change")[1], confint(model3.4, "constraint_change")[1], confint(model4.4, "effsize_change")[1], confint(model5.4, "max_degree_change")[1], confint(model6.4, "mean_degree_change")[1], confint(model7.4, "kin_prop_change")[1], confint(model8.4, "age_sd_change")[1], confint(model9.4, "IQVsex_change")[1], confint(model10.4, "IQVrace_change")[1], confint(model11.4, "weak_freq_prop_change")[1], confint(model12.4, "weak_dur_prop_change")[1], confint(model13.4, "far_dist_prop_change")[1], confint(model14.4, "drinking_prop_change")[1], confint(model15.4, "smoking_prop_change")[1], confint(model16.4, "no_exercise_prop_change")[1], confint(model17.4, "bad_diet_prop_change")[1], confint(model18.4, "health_prob_prop_change")[1])
table_pdds_ci_ms_time_2 <- c(confint(model1.4, "network_size_change")[2], confint(model2.4, "density_change")[2], confint(model3.4, "constraint_change")[2], confint(model4.4, "effsize_change")[2], confint(model5.4, "max_degree_change")[2], confint(model6.4, "mean_degree_change")[2], confint(model7.4, "kin_prop_change")[2], confint(model8.4, "age_sd_change")[2], confint(model9.4, "IQVsex_change")[2], confint(model10.4, "IQVrace_change")[2], confint(model11.4, "weak_freq_prop_change")[2], confint(model12.4, "weak_dur_prop_change")[2], confint(model13.4, "far_dist_prop_change")[2], confint(model14.4, "drinking_prop_change")[2], confint(model15.4, "smoking_prop_change")[2], confint(model16.4, "no_exercise_prop_change")[2], confint(model17.4, "bad_diet_prop_change")[2], confint(model18.4, "health_prob_prop_change")[2])
  
table_msrsr_ci_ms_time_1 <- c(confint(model19.4, "network_size_change")[1], confint(model20.4, "density_change")[1], confint(model21.4, "constraint_change")[1], confint(model22.4, "effsize_change")[1], confint(model23.4, "max_degree_change")[1], confint(model24.4, "mean_degree_change")[1], confint(model25.4, "kin_prop_change")[1], confint(model26.4, "age_sd_change")[1], confint(model27.4, "IQVsex_change")[1], confint(model28.4, "IQVrace_change")[1], confint(model29.4, "weak_freq_prop_change")[1], confint(model30.4, "weak_dur_prop_change")[1], confint(model31.4, "far_dist_prop_change")[1], confint(model32.4, "drinking_prop_change")[1], confint(model33.4, "smoking_prop_change")[1], confint(model34.4, "no_exercise_prop_change")[1], confint(model35.4, "bad_diet_prop_change")[1], confint(model36.4, "health_prob_prop_change")[1])
table_msrsr_ci_ms_time_2 <- c(confint(model19.4, "network_size_change")[2], confint(model20.4, "density_change")[2], confint(model21.4, "constraint_change")[2], confint(model22.4, "effsize_change")[2], confint(model23.4, "max_degree_change")[2], confint(model24.4, "mean_degree_change")[2], confint(model25.4, "kin_prop_change")[2], confint(model26.4, "age_sd_change")[2], confint(model27.4, "IQVsex_change")[2], confint(model28.4, "IQVrace_change")[2], confint(model29.4, "weak_freq_prop_change")[2], confint(model30.4, "weak_dur_prop_change")[2], confint(model31.4, "far_dist_prop_change")[2], confint(model32.4, "drinking_prop_change")[2], confint(model33.4, "smoking_prop_change")[2], confint(model34.4, "no_exercise_prop_change")[2], confint(model35.4, "bad_diet_prop_change")[2], confint(model36.4, "health_prob_prop_change")[2])

table_promis_ci_ms_time_1 <- c(confint(model37.4, "network_size_change")[1], confint(model38.4, "density_change")[1], confint(model39.4, "constraint_change")[1], confint(model40.4, "effsize_change")[1], confint(model41.4, "max_degree_change")[1], confint(model42.4, "mean_degree_change")[1], confint(model43.4, "kin_prop_change")[1], confint(model44.4, "age_sd_change")[1], confint(model45.4, "IQVsex_change")[1], confint(model46.4, "IQVrace_change")[1], confint(model47.4, "weak_freq_prop_change")[1], confint(model48.4, "weak_dur_prop_change")[1], confint(model49.4, "far_dist_prop_change")[1], confint(model50.4, "drinking_prop_change")[1], confint(model51.4, "smoking_prop_change")[1], confint(model52.4, "no_exercise_prop_change")[1], confint(model53.4, "bad_diet_prop_change")[1], confint(model54.4, "health_prob_prop_change")[1])
table_promis_ci_ms_time_2 <- c(confint(model37.4, "network_size_change")[2], confint(model38.4, "density_change")[2], confint(model39.4, "constraint_change")[2], confint(model40.4, "effsize_change")[2], confint(model41.4, "max_degree_change")[2], confint(model42.4, "mean_degree_change")[2], confint(model43.4, "kin_prop_change")[2], confint(model44.4, "age_sd_change")[2], confint(model45.4, "IQVsex_change")[2], confint(model46.4, "IQVrace_change")[2], confint(model47.4, "weak_freq_prop_change")[2], confint(model48.4, "weak_dur_prop_change")[2], confint(model49.4, "far_dist_prop_change")[2], confint(model50.4, "drinking_prop_change")[2], confint(model51.4, "smoking_prop_change")[2], confint(model52.4, "no_exercise_prop_change")[2], confint(model53.4, "bad_diet_prop_change")[2], confint(model54.4, "health_prob_prop_change")[2])

table_pdds_pval_ms_time <- c(summary(model1.4)$coef[2,4], summary(model2.4)$coef[2,4], summary(model3.4)$coef[2,4], summary(model4.4)$coef[2,4], summary(model5.4)$coef[2,4], summary(model6.4)$coef[2,4], summary(model7.4)$coef[2,4], summary(model8.4)$coef[2,4], summary(model9.4)$coef[2,4], summary(model10.4)$coef[2,4], summary(model11.4)$coef[2,4], summary(model12.4)$coef[2,4], summary(model13.4)$coef[2,4], summary(model14.4)$coef[2,4], summary(model15.4)$coef[2,4], summary(model16.4)$coef[2,4], summary(model17.4)$coef[2,4], summary(model18.4)$coef[2,4])
table_msrsr_pval_ms_time <- c(summary(model19.4)$coef[2,4], summary(model20.4)$coef[2,4], summary(model21.4)$coef[2,4], summary(model22.4)$coef[2,4],summary(model23.4)$coef[2,4], summary(model24.4)$coef[2,4], summary(model25.4)$coef[2,4], summary(model26.4)$coef[2,4], summary(model27.4)$coef[2,4], summary(model28.4)$coef[2,4], summary(model29.4)$coef[2,4], summary(model30.4)$coef[2,4], summary(model31.4)$coef[2,4], summary(model32.4)$coef[2,4], summary(model33.4)$coef[2,4], summary(model34.4)$coef[2,4], summary(model35.4)$coef[2,4], summary(model36.4)$coef[2,4])
table_promis_pval_ms_time <- c(summary(model37.4)$coef[2,4],summary(model38.4)$coef[2,4],summary(model39.4)$coef[2,4],summary(model40.4)$coef[2,4], summary(model41.4)$coef[2,4],summary(model42.4)$coef[2,4],summary(model43.4)$coef[2,4],summary(model44.4)$coef[2,4], summary(model45.4)$coef[2,4],summary(model46.4)$coef[2,4],summary(model47.4)$coef[2,4],summary(model48.4)$coef[2,4], summary(model49.4)$coef[2,4],summary(model50.4)$coef[2,4],summary(model51.4)$coef[2,4],summary(model52.4)$coef[2,4], summary(model53.4)$coef[2,4],summary(model54.4)$coef[2,4])

table_coefs_5 <- data.table(table_pdds_beta_ms_time, table_pdds_ci_ms_time_1, table_pdds_ci_ms_time_2, table_pdds_pval_ms_time, table_msrsr_beta_ms_time, table_msrsr_ci_ms_time_1, table_msrsr_ci_ms_time_2, table_msrsr_pval_ms_time, table_promis_beta_ms_time, table_promis_ci_ms_time_1, table_promis_ci_ms_time_2, table_promis_pval_ms_time)
colnames(table_coefs_5) <- table_col_1
rownames(table_coefs_5) <- table_row_1

table_coefs_5$`Network Feature` <- table_row_1
table_coefs_5 <- table_coefs_5[,c(13, 1:12)]

#Control

#PROMIS
#Network Size
model37_c1 <- lm(promis_t_score ~ network_size_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Density
model38_c1 <- lm(promis_t_score ~ density_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Constraint
model39_c1 <- lm(promis_t_score ~ constraint_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Effective Size
model40_c1 <- lm(promis_t_score ~ effsize_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Maximum Degree
model41_c1 <- lm(promis_t_score ~ max_degree_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Mean Degree
model42_c1 <- lm(promis_t_score ~ mean_degree_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent Kin
model43_c1 <- lm(promis_t_score ~ kin_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Standard deviation of age
model44_c1 <- lm(promis_t_score ~ age_sd_change + age + sex + education + employment + income + dx_duration + cohort + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model45_c1 <- lm(promis_t_score ~ IQVsex_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Diversity of Race
model46_c1 <- lm(promis_t_score ~ IQVrace_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent contacted weekly or less
model47_c1 <- lm(promis_t_score ~ weak_freq_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent known for less than 6 years
model48_c1 <- lm(promis_t_score ~ weak_dur_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent who live over 15 miles away
model49_c1 <- lm(promis_t_score ~ far_dist_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent who drink
model50_c1 <- lm(promis_t_score ~ drinking_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent who smoke
model51_c1 <- lm(promis_t_score ~ smoking_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent non exercisers
model52_c1 <- lm(promis_t_score ~ no_exercise_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent bad diet
model53_c1 <- lm(promis_t_score ~ bad_diet_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)
#Percent who have a negative health influence
model54_c1 <- lm(promis_t_score ~ health_prob_prop_change + age + employment + income + cohort + year_change, data=pnq_all_time_control)

table_promis_beta_c1 <- c(summary(model37_c1)$coef[2,1],summary(model38_c1)$coef[2,1],summary(model39_c1)$coef[2,1],summary(model40_c1)$coef[2,1], summary(model41_c1)$coef[2,1],summary(model42_c1)$coef[2,1],summary(model43_c1)$coef[2,1],summary(model44_c1)$coef[2,1], summary(model45_c1)$coef[2,1],summary(model46_c1)$coef[2,1],summary(model47_c1)$coef[2,1],summary(model48_c1)$coef[2,1], summary(model49_c1)$coef[2,1],summary(model50_c1)$coef[2,1],summary(model51_c1)$coef[2,1],summary(model52_c1)$coef[2,1], summary(model53_c1)$coef[2,1],summary(model54_c1)$coef[2,1])

table_promis_ci_c1 <- c(confint(model37_c1, "network_size_change")[1], confint(model38_c1, "density_change")[1], confint(model39_c1, "constraint_change")[1], confint(model40_c1, "effsize_change")[1], confint(model41_c1, "max_degree_change")[1], confint(model42_c1, "mean_degree_change")[1], confint(model43_c1, "kin_prop_change")[1], confint(model44_c1, "age_sd_change")[1], confint(model45_c1, "IQVsex_change")[1], confint(model46_c1, "IQVrace_change")[1], confint(model47_c1, "weak_freq_prop_change")[1], confint(model48_c1, "weak_dur_prop_change")[1], confint(model49_c1, "far_dist_prop_change")[1], confint(model50_c1, "drinking_prop_change")[1], confint(model51_c1, "smoking_prop_change")[1], confint(model52_c1, "no_exercise_prop_change")[1], confint(model53_c1, "bad_diet_prop_change")[1], confint(model54_c1, "health_prob_prop_change")[1])
table_promis_ci_c2 <- c(confint(model37_c1, "network_size_change")[2], confint(model38_c1, "density_change")[2], confint(model39_c1, "constraint_change")[2], confint(model40_c1, "effsize_change")[2], confint(model41_c1, "max_degree_change")[2], confint(model42_c1, "mean_degree_change")[2], confint(model43_c1, "kin_prop_change")[2], confint(model44_c1, "age_sd_change")[2], confint(model45_c1, "IQVsex_change")[2], confint(model46_c1, "IQVrace_change")[2], confint(model47_c1, "weak_freq_prop_change")[2], confint(model48_c1, "weak_dur_prop_change")[2], confint(model49_c1, "far_dist_prop_change")[2], confint(model50_c1, "drinking_prop_change")[2], confint(model51_c1, "smoking_prop_change")[2], confint(model52_c1, "no_exercise_prop_change")[2], confint(model53_c1, "bad_diet_prop_change")[2], confint(model54_c1, "health_prob_prop_change")[2])

table_promis_pval_c1 <- c(summary(model37_c1)$coef[2,4],summary(model38_c1)$coef[2,4],summary(model39_c1)$coef[2,4],summary(model40_c1)$coef[2,4], summary(model41_c1)$coef[2,4],summary(model42_c1)$coef[2,4],summary(model43_c1)$coef[2,4],summary(model44_c1)$coef[2,4], summary(model45_c1)$coef[2,4],summary(model46_c1)$coef[2,4],summary(model47_c1)$coef[2,4],summary(model48_c1)$coef[2,4], summary(model49_c1)$coef[2,4],summary(model50_c1)$coef[2,4],summary(model51_c1)$coef[2,4],summary(model52_c1)$coef[2,4], summary(model53_c1)$coef[2,4],summary(model54_c1)$coef[2,4])

table_coefs_6 <- data.table(table_promis_beta_c1, table_promis_ci_c1, table_promis_ci_c2, table_promis_pval_c1)
colnames(table_coefs_6) <- c("Beta", "95% CI (lower)", "95% CI (upper)", "P value")
rownames(table_coefs_6) <- table_row_1

table_coefs_6$`Network Feature` <- table_row_1
table_coefs_6 <- table_coefs_6[,c(5, 1:4)]

#Subset of covariates

#PDDS
#Network Size
model1.5 <- lm(fdr ~ network_size_change + age + employment + year_change, data=pnq_all_time_ms)
#Density
model2.5 <- lm(fdr ~ density_change + age + employment + year_change, data=pnq_all_time_ms)
#Constraint
model3.5 <- lm(fdr ~ constraint_change + age + employment + year_change, data=pnq_all_time_ms)
#Effective Size
model4.5 <- lm(fdr ~ effsize_change + age + employment + year_change, data=pnq_all_time_ms)
#Maximum Degree
model5.5 <- lm(fdr ~ max_degree_change + age + employment + year_change, data=pnq_all_time_ms)
#Mean Degree
model6.5 <- lm(fdr ~ mean_degree_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent Kin
pnq_all_time_ms[is.infinite(pnq_all_time_ms$kin_prop_change),]$kin_prop_change <- NA
model7.5 <- lm(fdr ~ kin_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Standard deviation of age
model8.5 <- lm(fdr ~ age_sd_change + age + employment + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model9.5 <- lm(fdr ~ IQVsex_change + age + employment + year_change, data=pnq_all_time_ms)
#Diversity of Race
model10.5 <- lm(fdr ~ IQVrace_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent contacted weekly or less
pnq_all_time_ms[is.infinite(pnq_all_time_ms$weak_freq_prop_change),]$weak_freq_prop_change <- NA
pnq_all_time_ms$weak_freq_prop_change <- as.numeric(pnq_all_time_ms$weak_freq_prop_change)
model11.5 <- lm(fdr ~ weak_freq_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent known for less than 6 years
pnq_all_time_ms[is.infinite(pnq_all_time_ms$weak_dur_prop_change),]$weak_dur_prop_change <- NA
model12.5 <- lm(fdr ~ weak_dur_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who live over 15 miles away
pnq_all_time_ms[is.infinite(pnq_all_time_ms$far_dist_prop_change),]$far_dist_prop_change <- NA
model13.5 <- lm(fdr ~ far_dist_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who drink
pnq_all_time_ms[is.infinite(pnq_all_time_ms$drinking_prop_change),]$drinking_prop_change <- NA
model14.5 <- lm(fdr ~ drinking_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who smoke
pnq_all_time_ms[is.infinite(pnq_all_time_ms$smoking_prop_change),]$smoking_prop_change <- NA
model15.5 <- lm(fdr ~ smoking_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent non exercisers
pnq_all_time_ms[is.infinite(pnq_all_time_ms$no_exercise_prop_change),]$no_exercise_prop_change <- NA
model16.5 <- lm(fdr ~ no_exercise_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent bad diet
pnq_all_time_ms[is.infinite(pnq_all_time_ms$bad_diet_prop_change),]$bad_diet_prop_change <- NA
model17.5 <- lm(fdr ~ bad_diet_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who have a negative health influence
pnq_all_time_ms[is.infinite(pnq_all_time_ms$health_prob_prop_change),]$health_prob_prop_change <- NA
model18.5 <- lm(fdr ~ health_prob_prop_change + age + employment + year_change, data=pnq_all_time_ms)

#MSRSR
#Network Size
model19.5 <- lm(msrs_total ~ network_size_change + age + employment + year_change, data=pnq_all_time_ms)
#Density
model20.5 <- lm(msrs_total ~ density_change + age + employment + year_change, data=pnq_all_time_ms)
#Constraint
model21.5 <- lm(msrs_total ~ constraint_change + age + employment + year_change, data=pnq_all_time_ms)
#Effective Size
model22.5 <- lm(msrs_total ~ effsize_change + age + employment + year_change, data=pnq_all_time_ms)
#Maximum Degree
model23.5 <- lm(msrs_total ~ max_degree_change + age + employment + year_change, data=pnq_all_time_ms)
#Mean Degree
model24.5 <- lm(msrs_total ~ mean_degree_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent Kin
model25.5 <- lm(msrs_total ~ kin_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Standard deviation of age
model26.5 <- lm(msrs_total ~ age_sd_change + age + employment + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model27.5 <- lm(msrs_total ~ IQVsex_change + age + employment + year_change, data=pnq_all_time_ms)
#Diversity of Race
model28.5 <- lm(msrs_total ~ IQVrace_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent contacted weekly or less
model29.5 <- lm(msrs_total ~ weak_freq_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent known for less than 6 years
model30.5 <- lm(msrs_total ~ weak_dur_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who live over 15 miles away
model31.5 <- lm(msrs_total ~ far_dist_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who drink
model32.5 <- lm(msrs_total ~ drinking_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who smoke
model33.5 <- lm(msrs_total ~ smoking_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent non exercisers
model34.5 <- lm(msrs_total ~ no_exercise_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent bad diet
model35.5 <- lm(msrs_total ~ bad_diet_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who have a negative health influence
model36.5 <- lm(msrs_total ~ health_prob_prop_change + age + employment + year_change, data=pnq_all_time_ms)

#PROMIS
#Network Size
model37.5 <- lm(promis_t_score ~ network_size_change + age + employment + year_change, data=pnq_all_time_ms)
#Density
model38.5 <- lm(promis_t_score ~ density_change + age + employment + year_change, data=pnq_all_time_ms)
#Constraint
model39.5 <- lm(promis_t_score ~ constraint_change + age + employment + year_change, data=pnq_all_time_ms)
#Effective Size
model40.5 <- lm(promis_t_score ~ effsize_change + age + employment + year_change, data=pnq_all_time_ms)
#Maximum Degree
model41.5 <- lm(promis_t_score ~ max_degree_change + age + employment + year_change, data=pnq_all_time_ms)
#Mean Degree
model42.5 <- lm(promis_t_score ~ mean_degree_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent Kin
model43.5 <- lm(promis_t_score ~ kin_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Standard deviation of age
model44.5 <- lm(promis_t_score ~ age_sd_change + age + sex + year_change, data=pnq_all_time_ms)
#Diversity of Sex
model45.5 <- lm(promis_t_score ~ IQVsex_change + age + employment + year_change, data=pnq_all_time_ms)
#Diversity of Race
model46.5 <- lm(promis_t_score ~ IQVrace_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent contacted weekly or less
model47.5 <- lm(promis_t_score ~ weak_freq_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent known for less than 6 years
model48.5 <- lm(promis_t_score ~ weak_dur_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who live over 15 miles away
model49.5 <- lm(promis_t_score ~ far_dist_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who drink
model50.5 <- lm(promis_t_score ~ drinking_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who smoke
model51.5 <- lm(promis_t_score ~ smoking_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent non exercisers
model52.5 <- lm(promis_t_score ~ no_exercise_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent bad diet
model53.5 <- lm(promis_t_score ~ bad_diet_prop_change + age + employment + year_change, data=pnq_all_time_ms)
#Percent who have a negative health influence
model54.5 <- lm(promis_t_score ~ health_prob_prop_change + age + employment + year_change, data=pnq_all_time_ms)

table_pdds_beta_ms_time_1 <- c(summary(model1.5)$coef[2,1], summary(model2.5)$coef[2,1], summary(model3.5)$coef[2,1], summary(model4.5)$coef[2,1], summary(model5.5)$coef[2,1], summary(model6.5)$coef[2,1], summary(model7.5)$coef[2,1], summary(model8.5)$coef[2,1], summary(model9.5)$coef[2,1], summary(model10.5)$coef[2,1], summary(model11.5)$coef[2,1], summary(model12.5)$coef[2,1], summary(model13.5)$coef[2,1], summary(model14.5)$coef[2,1], summary(model15.5)$coef[2,1], summary(model16.5)$coef[2,1], summary(model17.5)$coef[2,1], summary(model18.5)$coef[2,1])
table_msrsr_beta_ms_time_1 <- c(summary(model19.5)$coef[2,1], summary(model20.5)$coef[2,1], summary(model21.5)$coef[2,1], summary(model22.5)$coef[2,1],summary(model23.5)$coef[2,1], summary(model24.5)$coef[2,1], summary(model25.5)$coef[2,1], summary(model26.5)$coef[2,1], summary(model27.5)$coef[2,1], summary(model28.5)$coef[2,1], summary(model29.5)$coef[2,1], summary(model30.5)$coef[2,1], summary(model31.5)$coef[2,1], summary(model32.5)$coef[2,1], summary(model33.5)$coef[2,1], summary(model34.5)$coef[2,1], summary(model35.5)$coef[2,1], summary(model36.5)$coef[2,1])
table_promis_beta_ms_time_1 <- c(summary(model37.5)$coef[2,1],summary(model38.5)$coef[2,1],summary(model39.5)$coef[2,1],summary(model40.5)$coef[2,1], summary(model41.5)$coef[2,1],summary(model42.5)$coef[2,1],summary(model43.5)$coef[2,1],summary(model44.5)$coef[2,1], summary(model45.5)$coef[2,1],summary(model46.5)$coef[2,1],summary(model47.5)$coef[2,1],summary(model48.5)$coef[2,1], summary(model49.5)$coef[2,1],summary(model50.5)$coef[2,1],summary(model51.5)$coef[2,1],summary(model52.5)$coef[2,1], summary(model53.5)$coef[2,1],summary(model54.5)$coef[2,1])

table_pdds_ci_ms_time_3 <- c(confint(model1.5, "network_size_change")[1], confint(model2.5, "density_change")[1], confint(model3.5, "constraint_change")[1], confint(model4.5, "effsize_change")[1], confint(model5.5, "max_degree_change")[1], confint(model6.5, "mean_degree_change")[1], confint(model7.5, "kin_prop_change")[1], confint(model8.5, "age_sd_change")[1], confint(model9.5, "IQVsex_change")[1], confint(model10.5, "IQVrace_change")[1], confint(model11.5, "weak_freq_prop_change")[1], confint(model12.5, "weak_dur_prop_change")[1], confint(model13.5, "far_dist_prop_change")[1], confint(model14.5, "drinking_prop_change")[1], confint(model15.5, "smoking_prop_change")[1], confint(model16.5, "no_exercise_prop_change")[1], confint(model17.5, "bad_diet_prop_change")[1], confint(model18.5, "health_prob_prop_change")[1])
table_pdds_ci_ms_time_4 <- c(confint(model1.5, "network_size_change")[2], confint(model2.5, "density_change")[2], confint(model3.5, "constraint_change")[2], confint(model4.5, "effsize_change")[2], confint(model5.5, "max_degree_change")[2], confint(model6.5, "mean_degree_change")[2], confint(model7.5, "kin_prop_change")[2], confint(model8.5, "age_sd_change")[2], confint(model9.5, "IQVsex_change")[2], confint(model10.5, "IQVrace_change")[2], confint(model11.5, "weak_freq_prop_change")[2], confint(model12.5, "weak_dur_prop_change")[2], confint(model13.5, "far_dist_prop_change")[2], confint(model14.5, "drinking_prop_change")[2], confint(model15.5, "smoking_prop_change")[2], confint(model16.5, "no_exercise_prop_change")[2], confint(model17.5, "bad_diet_prop_change")[2], confint(model18.5, "health_prob_prop_change")[2])

table_msrsr_ci_ms_time_3 <- c(confint(model19.5, "network_size_change")[1], confint(model20.5, "density_change")[1], confint(model21.5, "constraint_change")[1], confint(model22.5, "effsize_change")[1], confint(model23.5, "max_degree_change")[1], confint(model24.5, "mean_degree_change")[1], confint(model25.5, "kin_prop_change")[1], confint(model26.5, "age_sd_change")[1], confint(model27.5, "IQVsex_change")[1], confint(model28.5, "IQVrace_change")[1], confint(model29.5, "weak_freq_prop_change")[1], confint(model30.5, "weak_dur_prop_change")[1], confint(model31.5, "far_dist_prop_change")[1], confint(model32.5, "drinking_prop_change")[1], confint(model33.5, "smoking_prop_change")[1], confint(model34.5, "no_exercise_prop_change")[1], confint(model35.5, "bad_diet_prop_change")[1], confint(model36.5, "health_prob_prop_change")[1])
table_msrsr_ci_ms_time_4 <- c(confint(model19.5, "network_size_change")[2], confint(model20.5, "density_change")[2], confint(model21.5, "constraint_change")[2], confint(model22.5, "effsize_change")[2], confint(model23.5, "max_degree_change")[2], confint(model24.5, "mean_degree_change")[2], confint(model25.5, "kin_prop_change")[2], confint(model26.5, "age_sd_change")[2], confint(model27.5, "IQVsex_change")[2], confint(model28.5, "IQVrace_change")[2], confint(model29.5, "weak_freq_prop_change")[2], confint(model30.5, "weak_dur_prop_change")[2], confint(model31.5, "far_dist_prop_change")[2], confint(model32.5, "drinking_prop_change")[2], confint(model33.5, "smoking_prop_change")[2], confint(model34.5, "no_exercise_prop_change")[2], confint(model35.5, "bad_diet_prop_change")[2], confint(model36.5, "health_prob_prop_change")[2])

table_promis_ci_ms_time_3 <- c(confint(model37.5, "network_size_change")[1], confint(model38.5, "density_change")[1], confint(model39.5, "constraint_change")[1], confint(model40.5, "effsize_change")[1], confint(model41.5, "max_degree_change")[1], confint(model42.5, "mean_degree_change")[1], confint(model43.5, "kin_prop_change")[1], confint(model44.5, "age_sd_change")[1], confint(model45.5, "IQVsex_change")[1], confint(model46.5, "IQVrace_change")[1], confint(model47.5, "weak_freq_prop_change")[1], confint(model48.5, "weak_dur_prop_change")[1], confint(model49.5, "far_dist_prop_change")[1], confint(model50.5, "drinking_prop_change")[1], confint(model51.5, "smoking_prop_change")[1], confint(model52.5, "no_exercise_prop_change")[1], confint(model53.5, "bad_diet_prop_change")[1], confint(model54.5, "health_prob_prop_change")[1])
table_promis_ci_ms_time_4 <- c(confint(model37.5, "network_size_change")[2], confint(model38.5, "density_change")[2], confint(model39.5, "constraint_change")[2], confint(model40.5, "effsize_change")[2], confint(model41.5, "max_degree_change")[2], confint(model42.5, "mean_degree_change")[2], confint(model43.5, "kin_prop_change")[2], confint(model44.5, "age_sd_change")[2], confint(model45.5, "IQVsex_change")[2], confint(model46.5, "IQVrace_change")[2], confint(model47.5, "weak_freq_prop_change")[2], confint(model48.5, "weak_dur_prop_change")[2], confint(model49.5, "far_dist_prop_change")[2], confint(model50.5, "drinking_prop_change")[2], confint(model51.5, "smoking_prop_change")[2], confint(model52.5, "no_exercise_prop_change")[2], confint(model53.5, "bad_diet_prop_change")[2], confint(model54.5, "health_prob_prop_change")[2])

table_pdds_pval_ms_time_1 <- c(summary(model1.5)$coef[2,4], summary(model2.5)$coef[2,4], summary(model3.5)$coef[2,4], summary(model4.5)$coef[2,4], summary(model5.5)$coef[2,4], summary(model6.5)$coef[2,4], summary(model7.5)$coef[2,4], summary(model8.5)$coef[2,4], summary(model9.5)$coef[2,4], summary(model10.5)$coef[2,4], summary(model11.5)$coef[2,4], summary(model12.5)$coef[2,4], summary(model13.5)$coef[2,4], summary(model14.5)$coef[2,4], summary(model15.5)$coef[2,4], summary(model16.5)$coef[2,4], summary(model17.5)$coef[2,4], summary(model18.5)$coef[2,4])
table_msrsr_pval_ms_time_1 <- c(summary(model19.5)$coef[2,4], summary(model20.5)$coef[2,4], summary(model21.5)$coef[2,4], summary(model22.5)$coef[2,4],summary(model23.5)$coef[2,4], summary(model24.5)$coef[2,4], summary(model25.5)$coef[2,4], summary(model26.5)$coef[2,4], summary(model27.5)$coef[2,4], summary(model28.5)$coef[2,4], summary(model29.5)$coef[2,4], summary(model30.5)$coef[2,4], summary(model31.5)$coef[2,4], summary(model32.5)$coef[2,4], summary(model33.5)$coef[2,4], summary(model34.5)$coef[2,4], summary(model35.5)$coef[2,4], summary(model36.5)$coef[2,4])
table_promis_pval_ms_time_1 <- c(summary(model37.5)$coef[2,4],summary(model38.5)$coef[2,4],summary(model39.5)$coef[2,4],summary(model40.5)$coef[2,4], summary(model41.5)$coef[2,4],summary(model42.5)$coef[2,4],summary(model43.5)$coef[2,4],summary(model44.5)$coef[2,4], summary(model45.5)$coef[2,4],summary(model46.5)$coef[2,4],summary(model47.5)$coef[2,4],summary(model48.5)$coef[2,4], summary(model49.5)$coef[2,4],summary(model50.5)$coef[2,4],summary(model51.5)$coef[2,4],summary(model52.5)$coef[2,4], summary(model53.5)$coef[2,4],summary(model54.5)$coef[2,4])

table_coefs_7 <- data.table(table_pdds_beta_ms_time_1, table_pdds_ci_ms_time_3, table_pdds_ci_ms_time_4, table_pdds_pval_ms_time_1, table_msrsr_beta_ms_time_1, table_msrsr_ci_ms_time_3, table_msrsr_ci_ms_time_4, table_msrsr_pval_ms_time_1, table_promis_beta_ms_time_1, table_promis_ci_ms_time_3, table_promis_ci_ms_time_4, table_promis_pval_ms_time_1)
colnames(table_coefs_7) <- table_col_1
rownames(table_coefs_7) <- table_row_1

table_coefs_7$`Network Feature` <- table_row_1
table_coefs_7 <- table_coefs_7[,c(13, 1:12)]

# 9. Longitudinal Analysis – Association Between Pre-pandemic Network and Compositional Variables and During Pandemic Outcomes - MS vs Control

#PROMIS
#Network Size
model37.10 <- lm(promis_t_score ~ network_size_change + age + employment + income + ms, data=pnq_all_time)
#Density
model38.10 <- lm(promis_t_score ~ density_change + age + employment + income + ms, data=pnq_all_time)
#Constraint
model39.10 <- lm(promis_t_score ~ constraint_change + age + employment + income + ms, data=pnq_all_time)
#Effective Size
model40.10 <- lm(promis_t_score ~ effsize_change + age + employment + income + ms, data=pnq_all_time)
#Maximum Degree
model41.10 <- lm(promis_t_score ~ max_degree_change + age + employment + income + ms, data=pnq_all_time)
#Mean Degree
model42.10 <- lm(promis_t_score ~ mean_degree_change + age + employment + income + ms, data=pnq_all_time)
#Percent Kin
model43.10 <- lm(promis_t_score ~ kin_prop_change + age + employment + income + ms, data=pnq_all_time)
#Standard deviation of age
model44.10 <- lm(promis_t_score ~ age_sd_change + age + sex + education + employment + income + ms, data=pnq_all_time)
#Diversity of Sex
model45.10 <- lm(promis_t_score ~ IQVsex_change + age + employment + income + ms, data=pnq_all_time)
#Diversity of Race
model46.10 <- lm(promis_t_score ~ IQVrace_change + age + employment + income + ms, data=pnq_all_time)
#Percent contacted weekly or less
model47.10 <- lm(promis_t_score ~ weak_freq_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent known for less than 6 years
model48.10 <- lm(promis_t_score ~ weak_dur_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent who live over 15 miles away
model49.10 <- lm(promis_t_score ~ far_dist_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent who drink
model50.10 <- lm(promis_t_score ~ drinking_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent who smoke
model51.10 <- lm(promis_t_score ~ smoking_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent non exercisers
model52.10 <- lm(promis_t_score ~ no_exercise_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent bad diet
model53.10 <- lm(promis_t_score ~ bad_diet_prop_change + age + employment + income + ms, data=pnq_all_time)
#Percent who have a negative health influence
model54.10 <- lm(promis_t_score ~ health_prob_prop_change + age + employment + income + ms, data=pnq_all_time)

table_promis_beta.10 <- c(summary(model37.10)$coef[2,1],summary(model38.10)$coef[2,1],summary(model39.10)$coef[2,1],summary(model40.10)$coef[2,1], summary(model41.10)$coef[2,1],summary(model42.10)$coef[2,1],summary(model43.10)$coef[2,1],summary(model44.10)$coef[2,1], summary(model45.10)$coef[2,1],summary(model46.10)$coef[2,1],summary(model47.10)$coef[2,1],summary(model48.10)$coef[2,1], summary(model49.10)$coef[2,1],summary(model50.10)$coef[2,1],summary(model51.10)$coef[2,1],summary(model52.10)$coef[2,1], summary(model53.10)$coef[2,1],summary(model54.10)$coef[2,1])

table_promis_ci.10 <- c(confint(model37.10, "network_size_change")[1], confint(model38.10, "density_change")[1], confint(model39.10, "constraint_change")[1], confint(model40.10, "effsize_change")[1], confint(model41.10, "max_degree_change")[1], confint(model42.10, "mean_degree_change")[1], confint(model43.10, "kin_prop_change")[1], confint(model44.10, "age_sd_change")[1], confint(model45.10, "IQVsex_change")[1], confint(model46.10, "IQVrace_change")[1], confint(model47.10, "weak_freq_prop_change")[1], confint(model48.10, "weak_dur_prop_change")[1], confint(model49.10, "far_dist_prop_change")[1], confint(model50.10, "drinking_prop_change")[1], confint(model51.10, "smoking_prop_change")[1], confint(model52.10, "no_exercise_prop_change")[1], confint(model53.10, "bad_diet_prop_change")[1], confint(model54.10, "health_prob_prop_change")[1])
table_promis_ci.101 <- c(confint(model37.10, "network_size_change")[2], confint(model38.10, "density_change")[2], confint(model39.10, "constraint_change")[2], confint(model40.10, "effsize_change")[2], confint(model41.10, "max_degree_change")[2], confint(model42.10, "mean_degree_change")[2], confint(model43.10, "kin_prop_change")[2], confint(model44.10, "age_sd_change")[2], confint(model45.10, "IQVsex_change")[2], confint(model46.10, "IQVrace_change")[2], confint(model47.10, "weak_freq_prop_change")[2], confint(model48.10, "weak_dur_prop_change")[2], confint(model49.10, "far_dist_prop_change")[2], confint(model50.10, "drinking_prop_change")[2], confint(model51.10, "smoking_prop_change")[2], confint(model52.10, "no_exercise_prop_change")[2], confint(model53.10, "bad_diet_prop_change")[2], confint(model54.10, "health_prob_prop_change")[2])

table_promis_pval.10 <- c(summary(model37.10)$coef[2,4],summary(model38.10)$coef[2,4],summary(model39.10)$coef[2,4],summary(model40.10)$coef[2,4], summary(model41.10)$coef[2,4],summary(model42.10)$coef[2,4],summary(model43.10)$coef[2,4],summary(model44.10)$coef[2,4], summary(model45.10)$coef[2,4],summary(model46.10)$coef[2,4],summary(model47.10)$coef[2,4],summary(model48.10)$coef[2,4], summary(model49.10)$coef[2,4],summary(model50.10)$coef[2,4],summary(model51.10)$coef[2,4],summary(model52.10)$coef[2,4], summary(model53.10)$coef[2,4],summary(model54.10)$coef[2,4])

table_coefs_8 <- data.table(table_promis_beta.10, table_promis_ci.10, table_promis_ci.101, table_promis_pval.10)
colnames(table_coefs_8) <- c("Beta", "95% CI (lower)", "95% CI (upper)", "P value")
rownames(table_coefs_8) <- table_row_1

table_coefs_8$`Network Feature` <- table_row_1
table_coefs_8 <- table_coefs_8[,c(5, 1:4)]


# 10. Q-Q plot

library(lattice)

#Credit to Matthew Flickinger on https://genome.sph.umich.edu/wiki/Code_Sample:_Generating_QQ_Plots_in_R for function

qqunif.plot<-function(pvalues, 
                      should.thin=T, thin.obs.places=2, thin.exp.places=2, 
                      xlab=expression(paste("Expected (",-log[10], " p-value)")),
                      ylab=expression(paste("Observed (",-log[10], " p-value)")), 
                      draw.conf=TRUE, conf.points=1000, conf.col="lightgray", conf.alpha=.05,
                      already.transformed=FALSE, pch=20, aspect="iso", prepanel=prepanel.qqunif,
                      par.settings=list(superpose.symbol=list(pch=pch)), ...) 
{
  
  
  #error checking
  if (length(pvalues)==0) stop("pvalue vector is empty, can't draw plot")
  if(!(class(pvalues)=="numeric" || 
       (class(pvalues)=="list" && all(sapply(pvalues, class)=="numeric"))))
    stop("pvalue vector is not numeric, can't draw plot")
  if (any(is.na(unlist(pvalues)))) stop("pvalue vector contains NA values, can't draw plot")
  if (already.transformed==FALSE) {
    if (any(unlist(pvalues)==0)) stop("pvalue vector contains zeros, can't draw plot")
  } else {
    if (any(unlist(pvalues)<0)) stop("-log10 pvalue vector contains negative values, can't draw plot")
  }
  
  
  grp<-NULL
  n<-1
  exp.x<-c()
  if(is.list(pvalues)) {
    nn<-sapply(pvalues, length)
    rs<-cumsum(nn)
    re<-rs-nn+1
    n<-min(nn)
    if (!is.null(names(pvalues))) {
      grp=factor(rep(names(pvalues), nn), levels=names(pvalues))
      names(pvalues)<-NULL
    } else {
      grp=factor(rep(1:length(pvalues), nn))
    }
    pvo<-pvalues
    pvalues<-numeric(sum(nn))
    exp.x<-numeric(sum(nn))
    for(i in 1:length(pvo)) {
      if (!already.transformed) {
        pvalues[rs[i]:re[i]] <- -log10(pvo[[i]])
        exp.x[rs[i]:re[i]] <- -log10((rank(pvo[[i]], ties.method="first")-.5)/nn[i])
      } else {
        pvalues[rs[i]:re[i]] <- pvo[[i]]
        exp.x[rs[i]:re[i]] <- -log10((nn[i]+1-rank(pvo[[i]], ties.method="first")-.5)/(nn[i]+1))
      }
    }
  } else {
    n <- length(pvalues)+1
    if (!already.transformed) {
      exp.x <- -log10((rank(pvalues, ties.method="first")-.5)/n)
      pvalues <- -log10(pvalues)
    } else {
      exp.x <- -log10((n-rank(pvalues, ties.method="first")-.5)/n)
    }
  }
  
  
  #this is a helper function to draw the confidence interval
  panel.qqconf<-function(n, conf.points=1000, conf.col="gray", conf.alpha=.05, ...) {
    require(grid)
    conf.points = min(conf.points, n-1);
    mpts<-matrix(nrow=conf.points*2, ncol=2)
    for(i in seq(from=1, to=conf.points)) {
      mpts[i,1]<- -log10((i-.5)/n)
      mpts[i,2]<- -log10(qbeta(1-conf.alpha/2, i, n-i))
      mpts[conf.points*2+1-i,1]<- -log10((i-.5)/n)
      mpts[conf.points*2+1-i,2]<- -log10(qbeta(conf.alpha/2, i, n-i))
    }
    grid.polygon(x=mpts[,1],y=mpts[,2], gp=gpar(fill=conf.col, lty=0), default.units="native")
  }
  
  #reduce number of points to plot
  if (should.thin==T) {
    if (!is.null(grp)) {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places),
                                grp=grp))
      grp = thin$grp
    } else {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places)))
    }
    pvalues <- thin$pvalues
    exp.x <- thin$exp.x
  }
  gc()
  
  prepanel.qqunif= function(x,y,...) {
    A = list()
    A$xlim = range(x, y)*1.02
    A$xlim[1]=0
    A$ylim = A$xlim
    return(A)
  }
  
  #draw the plot
  xyplot(pvalues~exp.x, groups=grp, xlab=xlab, ylab=ylab, aspect=aspect,
         prepanel=prepanel, scales=list(axs="i"), pch=pch,
         panel = function(x, y, ...) {
           if (draw.conf) {
             panel.qqconf(n, conf.points=conf.points, 
                          conf.col=conf.col, conf.alpha=conf.alpha)
           };
           panel.xyplot(x,y, ...);
           panel.abline(0,1);
         }, par.settings=par.settings, ...
  )
}

tiff(filename = "QQ_Plot_PDDS_Longitudinal.tiff", width = 3, height = 3, units="in", compression = "lzw", res=300)
qqunif.plot(-log10(table_coefs_5$`PDDS p value MS`), conf.alpha = 0.05, conf.points = 10000)
dev.off()
tiff(filename = "QQ_Plot_MSRSR_Longitudinal.tiff", width = 3, height = 3, units="in", compression = "lzw", res=300)
qqunif.plot(-log10(table_coefs_5$`MSRSR p value MS`), conf.alpha = 0.05, conf.points = 10000)
dev.off()
tiff(filename = "QQ_Plot_PROMIS_Longitudinal.tiff", width = 3, height = 3, units="in", compression = "lzw", res=300)
qqunif.plot(-log10(table_coefs_5$`PROMIS p value MS`), conf.alpha = 0.05, conf.points = 10000)
dev.off()
