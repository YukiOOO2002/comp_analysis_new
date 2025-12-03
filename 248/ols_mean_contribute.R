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
df <- read_csv(path, show_col_types = FALSE) 
df_fs <- df %>% 
  filter(alpha_med>=-2) %>% 
  filter(alpha_med<=2) %>% 
  filter(beta_med>=-2) %>% 
  filter(beta_med<=2) 

df_agg <- df_fs %>% 
  group_by(participant.label,group_type,alpha_med,beta_med,a015,a03,a045,age,gender,affiliation) %>% 
  summarise(mean_contribute=mean(contribute,na.rm=TRUE),
            .groups = "drop")

model_lm_dum <- lm(
  formula = mean_contribute~
    alpha_med + beta_med +
    a015 + a03 + a045+
    age + gender + affiliation,
  data = df_agg, 
)
summary(model_lm_dum)

model_lm_multi <- lm(
  formula = mean_contribute~
    alpha_med + beta_med +
    a015 + alpha_med*a015 + beta_med*a015 +
    a03 + alpha_med*a03 + beta_med*a03 +
    a045+ alpha_med*a045 + beta_med *a045 +
    age + gender + affiliation,
  data = df_agg,
)

summary(model_lm_multi)
