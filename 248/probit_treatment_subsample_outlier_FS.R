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


df_base <- df_fs %>% 
  filter(group_type =="T0")
df_015 <- df_fs %>% 
  filter(group_type == "TL")
df_03 <-  df_fs %>% 
  filter(group_type =="TM")
df_045 <- df_fs %>% 
  filter(group_type == "TH")

save_dir <- "015/regression/" 

modelglm_arr0 <- glm(
  formula = contribute_full~
    alpha_med + beta_med+
    age +gender +affiliation,
  data = df_base,
  family = binomial(link = "probit")
)


modelglm_arr1 <- glm(
  formula = contribute_full~
    alpha_med + beta_med +
    age + gender  +affiliation,
  data = df_015,
  family = binomial(link = "probit") 
)

modelglm_arr3 <- glm(
  formula = contribute_full~
    alpha_med + beta_med +
    age + gender +affiliation ,
  data = df_03,
  family = binomial(link = "probit") 
)


modelglm_arr4 <- glm(
  formula = contribute_full~
    alpha_med + beta_med +
    age + gender +affiliation,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name1 <- "resultsprobit_subsample_outlier_FS_continuous.tex"
full_path1<- paste0(save_dir, file_name1)

stargazer(modelglm_arr0,modelglm_arr1,modelglm_arr3,modelglm_arr4,
          type = "latex",
          out = full_path1, 
          title = "Probit Model Results",
          omit = c("age", "gender", "affiliation"))

list_of_models <- list(
  "Base" = modelglm_arr0,
  "015"  = modelglm_arr1,
  "03"   = modelglm_arr3,
  "045"  = modelglm_arr4
)

list_of_ME_sub <- lapply(
  list_of_models,
  FUN = margins  # 各モデルに対して margins 関数を適用
)

output_file <- "marginal_effects_table_subsample_outlier_FS.tex"

msummary(
  list_of_ME_sub,
  estimate = "{estimate} {stars}",
  output = output_file,
  title = "AverageMarginal Effects on Contribution",
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01",
  gof_map = c("nobs", "aic"),
  # 修正点: 標準誤差の自動的な2行目表示を抑制するため、vcovをNULLに設定
  vcov = NULL
)


modelglm_arrnon0 <- glm(
  formula = contribute_non~
    alpha_med+beta_med+
    age+gender +affiliation ,
  data=df_base,
  family = binomial(link="probit")
)
modelglm_arrnon1 <- glm(
  formula = contribute_non~
    alpha_med + beta_med +
    age + gender  +affiliation,
  data = df_015,
  family = binomial(link = "probit") 
)

modelglm_arrnon3 <- glm(
  formula = contribute_non~
    alpha_med + beta_med +
    age + gender  +affiliation,
  data = df_03,
  family = binomial(link = "probit") 
)


modelglm_arrnon4 <- glm(
  formula = contribute_non~
    alpha_med + beta_med +
    age + gender  +affiliation,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name2 <- "probit_subsample_c0_outlier_FS_continuous.tex"
full_path2 <- paste0(save_dir, file_name2)

stargazer(modelglm_arrnon0, modelglm_arrnon1,modelglm_arrnon3, modelglm_arrnon4,
          type = "latex",
          out = full_path2, 
          title = "Probit Model Results ")

list_of_modelsc0 <- list(
  "T0" = modelglm_arrnon0,
  "TL"  = modelglm_arrnon1,
  "TM"   = modelglm_arrnon3,
  "TH"  = modelglm_arrnon4
)

list_of_ME_sub_c0 <- lapply(
  list_of_modelsc0,
  FUN = margins  # 各モデルに対して margins 関数を適用
)

output_filec0 <- "marginal_effects_table_subsample_c0_outlier_FS.tex"

msummary(
  list_of_ME_sub_c0,
  estimate = "{estimate} {stars}",
  output = output_filec0,
  title = "Average Marginal Effects on Contribution",
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01",
  gof_map = c("nobs", "aic"),
  # 修正点: 標準誤差の自動的な2行目表示を抑制するため、vcovをNULLに設定
  vcov = NULL
)

