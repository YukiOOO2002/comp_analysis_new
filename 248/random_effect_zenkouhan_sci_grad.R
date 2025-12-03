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

df_zen <- df %>% 
  filter(round<=5)
df_kou <- df %>% 
  filter(round>=6)
model_plm_dum_zen <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science+graduate+
    age + gender ,
  data = df_zen,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_dum_kou <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science+graduate+
    age + gender ,
  data = df_kou,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FErandom_Regression_zenkouhan_sci_grad.tex"
full_path <- paste0(save_dir, file_name)

mean_contribution_base_zen <- df_zen %>% 
  filter(Base == 1) %>%
  pull(contribute) %>% 
  mean(na.rm = TRUE)

mean_contribution_base_kou <- df_kou%>% 
  filter(Base == 1) %>%
  pull(contribute) %>% 
  mean(na.rm = TRUE)


rows_to_add <- data.frame(
  Term = "Mean (Contribute)",
  "zenhan" = mean_contribution_base_zen,    # 列名に "FE-Dum" を使用
  "kouhan" = mean_contribution_base_kou   # 列名に "FE-Multi" を使用
)

modelsummary(list(model_plm_dum_zen, model_plm_dum_kou),
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

model_plm_arr_multi_zen <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science+graduate+
    age + gender ,
  data = df_zen,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multi_kou <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science+graduate+
    age + gender ,
  data = df_kou,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)


file_name1 <- "FErandom_Regression_interaction_sci_grad.tex"
full_path1<- paste0(save_dir, file_name1)
modelsummary(list(model_plm_arr_multi_zen,model_plm_arr_multi_kou),
             title = "Random effect Regression",
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


