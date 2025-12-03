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

modelglm_arr <- glm(
  formula = contribute_full~
    alpha_med + beta_med +
    TL + TM + TH+
    age + gender + affiliation,
  data = df_fs,
  family = binomial(link = "probit") 
)

modelglm_arrnon <- glm(
  formula = contribute_non~
    alpha_med + beta_med +
    TL + TM + TH+
    age + gender + affiliation,
  data = df_fs,
  family = binomial(link = "probit") 
)

save_dir <- "015/regression/" 
file_name <- "probit_except_outlier_FS.tex" 
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr, modelglm_arrnon,
          type = "latex", 
          out = full_path, 
          title = "Probit Model Results ",
          omit = c("age", "gender", "affiliation")
          
)
probit_ME <- margins(modelglm_arr)
probit_ME_non <- margins(modelglm_arrnon)

library(modelsummary)
list_of_ME <- list(
  "Contribute Full" = probit_ME,
  "Contribute Non" = probit_ME_non
)
output_file <- "marginal_effects_table_except_outlier_FS.tex"

# msummary()を使ってLaTeX形式でファイルに出力
msummary(
  list_of_ME,
  estimate = "{estimate} {stars}", # 推定値と標準誤差、p値スターの表示形式
  output = output_file,
  title = "Probit Models: Marginal Effects on Contribution", # テーブルのタイトル
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01", # ノート
  gof_map = c("nobs", "aic") # 表示するGoodness-of-Fit統計量（marginsオブジェクトでもnobsは表示可能）
)
