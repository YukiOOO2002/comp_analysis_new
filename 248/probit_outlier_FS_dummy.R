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

df_fs <- df_fs %>%
  mutate(
    is_base = as.integer(grepl("base", group_type, ignore.case = TRUE)),
    TL = as.integer(grepl("015", group_type)),
    TM = as.integer(grepl("03", group_type)),
    TH = as.integer(grepl("045", group_type))
  )

df_base <- df_fs %>% 
  filter(group_type =="base")
df_015 <- df_fs %>% 
  filter(group_type == "015")
df_03 <-  df_fs %>% 
  filter(group_type =="03")
df_045 <- df_fs %>% 
  filter(group_type == "045")

save_dir <- "015/regression/" 

modelglm_arr0 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive+
    age +gender +affiliation,
  data = df_base,
  family = binomial(link = "probit")
)


modelglm_arr1 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    age + gender  +affiliation,
  data = df_015,
  family = binomial(link = "probit") 
)

modelglm_arr3 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    age + gender +affiliation ,
  data = df_03,
  family = binomial(link = "probit") 
)


modelglm_arr4 <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    age + gender +affiliation,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name1 <- "resultsprobit_subsample_outlier_FS.tex"
full_path1<- paste0(save_dir, file_name1)

stargazer(modelglm_arr0,modelglm_arr1,modelglm_arr3,modelglm_arr4,
          type = "latex",
          out = full_path1, 
          title = "Probit Model Results",
          omit = c("age", "gender", "affiliation"))


modelglm_arrnon0 <- glm(
  formula = contribute_non~
    alpha_positive+beta_positive+
    age+gender +affiliation ,
  data=df_base,
  family = binomial(link="probit")
)
modelglm_arrnon1 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    age + gender  +affiliation,
  data = df_015,
  family = binomial(link = "probit") 
)

modelglm_arrnon3 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    age + gender  +affiliation,
  data = df_03,
  family = binomial(link = "probit") 
)


modelglm_arrnon4 <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    age + gender  +affiliation,
  data = df_045,
  family = binomial(link = "probit") 
)

file_name2 <- "probit_subsample_c0_outlier_FS.tex"
full_path2 <- paste0(save_dir, file_name2)

stargazer(modelglm_arrnon0, modelglm_arrnon1,modelglm_arrnon3, modelglm_arrnon4,
          type = "latex",
          out = full_path2, 
          title = "Probit Model Results ")