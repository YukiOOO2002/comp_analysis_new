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
  formula = contribute_full~
    alpha_positive+beta_positive+
    TL + TM + TH+
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)


model_plm_arr_multi <- plm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    TL + alpha_positive*TL + beta_positive*TL +
    TM + alpha_positive*TM + beta_positive*TM +
    TH+ alpha_positive*TH + beta_positive *TH +
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_dum_non <- plm(
  formula = contribute_non~
    alpha_positive+beta_positive+
    TL + TM + TH+
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multi_non <- plm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    TL + alpha_positive*TL + beta_positive*TL +
    TM + alpha_positive*TM + beta_positive*TM +
    TH+ alpha_positive*TH + beta_positive *TH +
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FErandom_Probability_Regression_full_non_single.tex"
full_path <- paste0(save_dir, file_name)

mean_contribution <- df %>% 
  filter(T0 == 1) %>%
  pull(contribute) %>% 
  mean(na.rm = TRUE)


rows_to_add <- data.frame(
  Term = "Mean (Contribute)",
  "full" = mean_contribution,    # 列名に "FE-Dum" を使用
  "full_interaction" = mean_contribution
)

modelsummary(list(model_plm_dum,model_plm_dum_non ),
             title = "Random Effect Probability Regression",
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

file_name1 <- "FErandom_Probability_Regression_full_non_interaction.tex"
full_path1 <- paste0(save_dir, file_name1)

modelsummary(list(model_plm_arr_multi,model_plm_arr_multi_non),
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



