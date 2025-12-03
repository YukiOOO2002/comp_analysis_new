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

df_base <- df %>% 
  filter(group_type =="base")
df_015 <- df %>% 
  filter(group_type == "015")
df_03 <-  df %>% 
  filter(group_type =="03")
df_045 <- df %>% 
  filter(group_type == "045")


modelglm_arr <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    +a015 + a03 + a045+
    science + graduate +
    age + gender ,
  data = df,
  family = binomial(link = "probit") 
)

modelglm_arrnon <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    a015 + a03 + a045+
    science + graduate +
    age + gender ,
  data = df,
  family = binomial(link = "probit") 
)

save_dir <- "015/regression/" 
file_name <- "resultsprobit_sci_grad.html"
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr, modelglm_arrnon,
          type = "html",
          out = full_path, 
          title = "Probit Model Results",
          omit = c("age", "gender", "affiliation"))

modelglm_arr1 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_015,
  family = binomial(link = "probit") 
)

modelglm_arrnon1 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_015,
  family = binomial(link = "probit") 
)

file_name <- "results015probit.html"
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr1, modelglm_arrnon1,
          type = "html",
          out = full_path, 
          title = "Probit Model Results ")

modelglm_arr3 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_03,
  family = binomial(link = "probit") 
)

modelglm_arrnon3 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_03,
  family = binomial(link = "probit") 
)

file_name <- "results03probit.html"
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr3, modelglm_arrnon3,
          type = "html",
          out = full_path, 
          title = "Probit Model Results ")


modelglm_arr4 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_045,
  family = binomial(link = "probit") 
)

modelglm_arrnon4 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name <- "results045probit.html"
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr4, modelglm_arrnon4,
          type = "html",
          out = full_path, 
          title = "Probit Model Results ")
