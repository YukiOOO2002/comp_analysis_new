rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(modelsummary)
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/merge_long.csv" 
df <- read_csv(path, show_col_types = FALSE) 

df2 <- df %>% 
  mutate(
    contribute_full = if_else(contribute == 10,1,0),
    contribute_non = if_else(contribute == 0,1,0),
  )

df3 <- df2 %>%
  mutate(
    is_base = as.integer(grepl("base", group_type, ignore.case = TRUE)),
    a015 = as.integer(grepl("015", group_type)),
    a03 = as.integer(grepl("03", group_type)),
    a045 = as.integer(grepl("045", group_type))
  )

df_1st <- df3 %>% 
  filter(round == 1)

modelglm_paradum <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df_1st,
  family = binomial(link = "probit") 
)
modelglm_paradum_multi <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df_1st,
  family = binomial(link = "probit") 
)
stargazer(modelglm_paradum,modelglm_paradum_multi, type = "text", title = "Probit Model Results (contribute_non)")

modelglm_paradum_non <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
modelglm_paradum_multi_non <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + alpha_positive*a015 + beta_positive*a015 +
    a03 + alpha_positive*a03 + beta_positive*a03 +
    a045+ alpha_positive*a045 + beta_positive *a045 +
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)
stargazer(modelglm_paradum_non,modelglm_paradum_multi_non, type = "text", title = "Probit Model Results (contribute_non)")

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

models <- list("treatment only" = model_lm_dum, "add parameter" = model_lm_arr_multi)


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


model_lm_duma <- lm_robust(
  formula = contribute~
    alpha_positive +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df3,
)

model_lm_arr_multia <- lm_robust(
  formula = contribute~
    alpha_positive +
    a015 + alpha_positive*a015 +
    a03 + alpha_positive*a03 +
    a045+ alpha_positive*a045 +
    age + gender + affiliation,
  data = df3
)

models <- list("treatment only" = model_lm_duma, "add parameter" = model_lm_arr_multia)


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
