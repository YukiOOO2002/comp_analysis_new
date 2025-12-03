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
setwd("././././.")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 
df_fs <- df %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 

model_plm_dum <- plm(
  formula = contribute_full~
    alpha_med+beta_med+
    TL + TM + TH+
    age + gender + affiliation,
  data = df_fs,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)


model_plm_arr_multi <- plm(
  formula = contribute_full~
    alpha_med + beta_med +
    TL + alpha_med*TL + beta_med*TL +
    TM + alpha_med*TM + beta_med*TM +
    TH+ alpha_med*TH + beta_med *TH +
    age + gender + affiliation,
  data = df_fs,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_dum_non <- plm(
  formula = contribute_non~
    alpha_med+beta_med+
    TL + TM + TH+
    age + gender + affiliation,
  data = df_fs,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multi_non <- plm(
  formula = contribute_non~
    alpha_med + beta_med +
    TL + alpha_med*TL + beta_med*TL +
    TM + alpha_med*TM + beta_med*TM +
    TH+ alpha_med*TH + beta_med *TH +
    age + gender + affiliation,
  data = df_fs,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FErandom_Probability_Regression_follow_FS_full.tex"
full_path <- paste0(save_dir, file_name)

mean_contribution <- df_fs %>% 
  filter(T0 == 1) %>%
  pull(contribute) %>% 
  mean(na.rm = TRUE)


rows_to_add <- data.frame(
  Term = "Mean (Contribute)",
  "full" = mean_contribution,    # 列名に "FE-Dum" を使用
  "full_interaction" = mean_contribution
)

modelsummary(list(model_plm_dum, model_plm_arr_multi),
             title = "Random Effect  Probability Regression",
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

file_name1 <- "FErandom_Probability_Regression_follow_FS_non.tex"
full_path1 <- paste0(save_dir, file_name1)

modelsummary(list(model_plm_dum,model_plm_arr_multi_non),
             title = "Random Effect  Probability Regression",
             stars = TRUE, 
             output = full_path1,
             add_rows = rows_to_add,
             gof_map = c(
               "nobs",
               "r.squared",
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA,
               "Mean.of.Dependent.Variable"
             ))



