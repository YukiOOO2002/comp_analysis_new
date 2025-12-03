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


save_dir <- "015/regression/" 

modelglm_arr0 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive+
    science + graduate +
    age +gender,
  data = df_base,
  family = binomial(link = "probit")
)


modelglm_arr1 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_015,
  family = binomial(link = "probit") 
)

modelglm_arr3 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_03,
  family = binomial(link = "probit") 
)


modelglm_arr4 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name1 <- "resultsprobit_subsample_sci_grad.tex"
full_path1<- paste0(save_dir, file_name1)

stargazer(modelglm_arr0,modelglm_arr1,modelglm_arr3,modelglm_arr4,
          type = "latex",
          out = full_path1, 
          title = "Probit Model Results",
          omit = c("age", "gender", "affiliation"))


modelglm_arrnon0 <- glm(
  formula = contribute_non~
    alpha_positive+beta_positive+
    science + graduate +
    age+gender ,
  data=df_base,
  family = binomial(link="probit")
)
modelglm_arrnon1 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_015,
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


modelglm_arrnon4 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    science + graduate +
    age + gender ,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name2 <- "probit_subsample_c0_sci_grad.tex"
full_path2 <- paste0(save_dir, file_name2)

stargazer(modelglm_arrnon0, modelglm_arrnon1,modelglm_arrnon3, modelglm_arrnon4,
          type = "latex",
          out = full_path2, 
          title = "Probit Model Results ")
