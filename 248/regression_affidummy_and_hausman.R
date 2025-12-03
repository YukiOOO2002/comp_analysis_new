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

# modelglm_norm <- glm(
#   formula = contribute_full~
#     TL + TM + TH+
#     science + graduate +
#     age + gender ,
#   data = df,
#   family = binomial(link = "probit") 
# )
# modelglm_arr <- glm(
#   formula = contribute_full~
#     alpha_positive + beta_positive +
#     TL + TM + TH+
#     science + graduate +
#     age + gender ,
#   data = df,
#   family = binomial(link = "probit") 
# )
# 
# modelglm_arr11 <- glm(
#   formula = contribute_full~
#     alpha_positive + beta_positive +
#     TL + TM + TH+
#     TL*beta_positive + TM*beta_positive + TH*beta_positive+
#     science + graduate +
#     age + gender ,
#   data = df,
#   family = binomial(link = "probit") 
# )
# 
# modelglm_arrnon <- glm(
#   formula = contribute_non~
#     alpha_positive + beta_positive +
#     TL + TM + TH+
#     science + graduate +
#     age + gender  ,
#   data = df,
#   family = binomial(link = "probit") 
# )
# 
# modelglm_arr11non <- glm(
#   formula = contribute_non~
#     alpha_positive + beta_positive +
#     TL + TM + TH+
#     TL*beta_positive + TM*beta_positive + TH*beta_positive+
#     science + graduate +
#     age + gender ,
#   data = df,
#   family = binomial(link = "probit") 
# )
# 
# 
save_dir <- "015/regression/"
# file_name <- "resultsaffidummmy.html"
# full_path <- paste0(save_dir, file_name)
# 
# stargazer(modelglm_arr, modelglm_arr11, modelglm_arrnon, modelglm_arr11non,
#           type = "html",
#           out = full_path, 
#           title = "Probit Model Results WITH AFFILIATION DUMMY")

model_plm_dum <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender+affiliation  ,
  data = df,
  effect = "time",
  model = "within",
  index = c("participant.label", "round")
)

# model_plm_dum <- plm(
#   formula = contribute~
#     alpha_positive + beta_positive +
#     TL + TM + TH+
#     science + graduate + science*graduate+
#     age + gender  ,
#   data = df,
#   effect = "time",
#   model = "within",
#   index = c("participant.label", "round")
# )

model_plm_arr_multi <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    TL + alpha_positive*TL + beta_positive*TL +
    TM + alpha_positive*TM + beta_positive*TM +
    TH+ alpha_positive*TH + beta_positive *TH +
    age + gender+affiliation  ,
  data = df,
  effect = "time",
  model = "within", 
  index = c("participant.label", "round")
)
# model_plm_arr_multi <- plm(
#   formula = contribute~
#     alpha_positive + beta_positive +
#     TL + alpha_positive*TL + beta_positive*TL +
#     TM + alpha_positive*TM + beta_positive*TM +
#     TH+ alpha_positive*TH + beta_positive *TH +
#     science + graduate +
#     age + gender  ,
#   data = df,
#   effect = "time",
#   model = "within",
#   index = c("participant.label", "round")
# )

model_plm_dumr <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender+affiliation  ,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)
# model_plm_dumr <- plm(
#   formula = contribute~
#     alpha_positive + beta_positive +
#     TL + TM + TH+
#     science + graduate +
#     age + gender  ,
#   data = df,
#   effect = "time",
#   model = "random", 
#   index = c("participant.label", "round")
# )

model_plm_arr_multir <- plm(
  formula = contribute~
    alpha_positive + beta_positive +
    TL + alpha_positive*TL + beta_positive*TL +
    TM + alpha_positive*TM + beta_positive*TM +
    TH+ alpha_positive*TH + beta_positive *TH +
    age + gender+affiliation  ,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)

# model_plm_arr_multir <- plm(
#   formula = contribute~
#     alpha_positive + beta_positive +
#     TL + alpha_positive*TL + beta_positive*TL +
#     TM + alpha_positive*TM + beta_positive*TM +
#     TH+ alpha_positive*TH + beta_positive *TH +
#     science + graduate +
#     age + gender  ,
#   data = df,
#   effect = "time",
#   model = "random", 
#   index = c("participant.label", "round"))

# model_plm_arr_multii <- plm(
#   formula = contribute~
#     alpha_positive + beta_positive +
#     TL + alpha_positive*TL + beta_positive*TL +
#     TM + alpha_positive*TM + beta_positive*TM +
#     TH+ alpha_positive*TH + beta_positive *TH +
#     science + graduate + science*graduate+
#     age + gender  ,
#   data = df,
#   effect = "time",
#   model = "within", 
#   index = c("participant.label", "round")
# )
# 
# model_plm_arr_multiri <- plm(
#   formula = contribute~
#     alpha_positive + beta_positive +
#     TL + alpha_positive*TL + beta_positive*TL +
#     TM + alpha_positive*TM + beta_positive*TM +
#     TH+ alpha_positive*TH + beta_positive *TH +
#     science + graduate + science*graduate+
#     age + gender  ,
#   data = df,
#   effect = "time",
#   model = "random", 
#   index = c("participant.label", "round")
# )

# file_name1 <- "FEtime_afficumyy.html"
# full_path1 <- paste0(save_dir, file_name1)
# 
# 
# stargazer(model_plm_arr_multii,model_plm_arr_multiri,
#           type = "html",
#           out = full_path1,
#           title = "FEtime Regression")

model_listf <- list("FE" = model_plm_dum,
                   "Random" = model_plm_dumr)
file_namef <- "FEorRandom_simple.tex"
full_pathf<- paste0(save_dir, file_namef)


modelsummary(model_listf,
             title = "Comparison of FE and Random effect model Regression",
             stars = TRUE, 
             output = full_pathf,
             gof_map = c(
               "nobs",
               "r.squared",
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA,
               "Mean.of.Dependent.Variable"
             ))
hausman <- phtest(model_plm_dum,model_plm_dumr)
print(hausman)


model_listff <- list("FE" = model_plm_arr_multi,
                    "Random" = model_plm_arr_multir)
file_nameff <- "FEorRandom_interaction.tex"
full_pathff<- paste0(save_dir, file_nameff)


modelsummary(model_listff,
             title = "Comparison of FE and Random effect model Regression interaction",
             stars = TRUE, 
             output = full_pathff,
             gof_map = c(
               "nobs",
               "r.squared",
               "AIC" = NA,
               "BIC" = NA,
               "RMSE" = NA,
               "Mean.of.Dependent.Variable"
             ))

hausman2 <- phtest(model_plm_arr_multi,model_plm_arr_multir)
print(hausman2)
