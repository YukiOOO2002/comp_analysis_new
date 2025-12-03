rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(broom)
library(gt)
library(modelsummary)
library(plm)
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 
df_fs <- df %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 

model_plm_dum <- plm(
  formula = contribute~
    alpha_med+beta_med+
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df_fs,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multi <- plm(
  formula = contribute~
    alpha_med + beta_med +
    a015 + alpha_med*a015 + beta_med*a015 +
    a03 + alpha_med*a03 + beta_med*a03 +
    a045+ alpha_med*a045 + beta_med *a045 +
    age + gender + affiliation,
  data = df_fs,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FErandom_Regression_except_outlier_FS.tex"
full_path <- paste0(save_dir, file_name)

mean_contribution <- df_fs %>% 
  filter(Base == 1) %>%
  pull(contribute) %>% 
  mean(na.rm = TRUE)


rows_to_add <- data.frame(
  Term = "Mean (Contribute)",
  "FE-Dum" = mean_contribution,    # 列名に "FE-Dum" を使用
  "FE-Multi" = mean_contribution   # 列名に "FE-Multi" を使用
)

modelsummary(list(model_plm_dum, model_plm_arr_multi),
             title = "Random Effect Regression",
             stars = TRUE, 
             output = full_path,
             add_rows = rows_to_add,
             gof_map = c(
               "nobs",
               "r.squared",
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA,
               "Mean.of.Dependent.Variable"
             ))

summary <- summary(df_fs)
print(summary)
