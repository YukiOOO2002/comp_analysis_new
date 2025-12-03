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
model_plm_dum <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multi <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FErandom_Regression.tex"
full_path <- paste0(save_dir, file_name)

mean_contribution_base <- df %>% 
  filter(Base == 1) %>%
  pull(contribute) %>% 
  mean(na.rm = TRUE)
  

rows_to_add <- data.frame(
  Term = "Mean (Contribute)",
  "FE-Dum" = mean_contribution_base,    # 列名に "FE-Dum" を使用
  "FE-Multi" = mean_contribution_base   # 列名に "FE-Multi" を使用
)

modelsummary(list(model_plm_dum, model_plm_arr_multi),
             title = "FEtime Regression",
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

row_to_add <- data.frame(
  Term = "Mean (Contribute)",
  "FE-Dum" = mean_contribution_base
)
file_name1 <- "FErandom_Regression_simple.tex"
full_path1<- paste0(save_dir, file_name1)
model_list <- list("FE-Dum" = model_plm_dum)
modelsummary(model_list,
             title = "Random effect Regression",
             stars = TRUE, 
             output = full_path1,
             add_rows = row_to_add,
             gof_map = c(
               "nobs",
               "r.squared",
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA,
               "Mean.of.Dependent.Variable"
             ))


