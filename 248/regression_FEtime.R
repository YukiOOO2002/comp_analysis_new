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


pdata <- pdata.frame(df, index = c("participant.label", "round"))
table <- table(index(pdata), useNA = "ifany")
print(table)
df_table <- as.data.frame(table)
sum(df_table$Freq)



model_plm_dum <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "within", 
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
  model = "within", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FEtime Regression.tex"
full_path <- paste0(save_dir, file_name)

stargazer(model_plm_dum,model_plm_arr_multi,
          type = "latex",
          out = full_path,
          omit = c("age","gender","affiliation"))

model_pooled <- plm(
    formula = contribute~
      alpha_positive + beta_positive +
      a015 + a03 + a045+
      science + graduate +
      age + gender ,
    data = df,
    model = "pooling", 
    index = c("participant.label", "round")
)

model_plm_dum <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science + graduate +
    age + gender ,
  data = df,
  effect = "time",
  model = "within", 
  index = c("participant.label", "round")
)

model_plm_arr_multi <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    science + graduate +
    age + gender,
  data = df,
  effect = "time",
  model = "within", 
  index = c("participant.label", "round")
)

save_dir <- "015/regression/" 
file_name <- "FEtime Regression_with_science_grad.html"
full_path <- paste0(save_dir, file_name)

stargazer(model_plm_dum,model_plm_arr_multi,
          type = "html",
          out = full_path,
          omit = c("age","gender","affiliation"))



model_plm_dumr <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

model_plm_arr_multir <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round"))

stargazer(model_plm_dumr,
          type = "latex",
          out = "FEtime_randome_1.tex",
          title = "Random Effect Model")

stargazer(model_plm_dumr,model_plm_arr_multir,
          type = "latex",
          out = "FEtime_random.tex",
          title = "Randomtime Regression")

pF <- pFtest(model_pooled,model_plm_dum)
print(pF)
hausman <- phtest(model_plm_dum,model_plm_dumr)
print(hausman)

hausman2 <- phtest(model_plm_arr_multi,model_plm_arr_multir)
print(hausman2)

models <- list("(1)" = model_plm_dum,"(2)" = model_plm_arr_multi)

modelsummary(models, 
             title = "FEtime Regression",
             stars = TRUE, 
             output = "robust.png",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))
