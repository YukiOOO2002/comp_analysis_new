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
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 

model_lm_dum <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multi <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3
)



models <- list("(1)" = model_lm_dum,"(2)" = model_lm_arr_multi)

modelsummary(models, 
             title = "Robust Regression",
             stars = TRUE, 
             output = "robust.png",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))
help(modelsummary)

model_lm_dumbase <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multibase <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + alpha_positive*Base + beta_positive*Base +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3
)

model_lm_dum03<- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + a015 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multi03 <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + alpha_positive*Base + beta_positive*Base +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3
)

models <- list("(1)" = model_lm_dum, "(2)" = model_lm_arr_multi,
               "(3)" = model_lm_dumbase, "(4)" = model_lm_arr_multibase,
               "(5)" = model_lm_dum03, "(6)" = model_lm_arr_multi03)



modelsummary(models, 
             title = "Robust Regression",
             stars = TRUE, 
             output = "robust.html",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))

model_lm_dumbase <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multibase <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + alpha_positive*Base + beta_positive*Base +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3
)

models <- list("treatment only" = model_lm_dumbase, "add parameter" = model_lm_arr_multibase)


modelsummary(models, 
             title = "Robust Regression",
             stars = TRUE, 
             output = "markdown",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))


model_lm_dum03<- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + a015 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multi03 <- lm_robust(
  formula = contribute~
    alpha_positive + beta_positive +
    Base + alpha_positive*Base + beta_positive*Base +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3
)

models <- list("treatment only" = model_lm_dum03, "add parameter" = model_lm_arr_multi03)


modelsummary(models, 
             title = "Robust Regression",
             stars = TRUE, 
             output = "markdown",
             gof_map = c(
               "R2 Adj" = NA,
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA
             ))
