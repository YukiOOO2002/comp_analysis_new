rm(list = ls())
library(tidyverse) 
library(lme4)
library(stargazer)
library(dplyr)
library(ggplot2) 
library(estimatr)
library(modelsummary)
library(margins)
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
    TL = as.integer(grepl("015", group_type)),
    TM = as.integer(grepl("03", group_type)),
    TH = as.integer(grepl("045", group_type))
  )


modelglm_arr <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

modelglm_arrnon <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender + affiliation,
  data = df3,
  family = binomial(link = "probit") 
)

save_dir <- "015/regression/" 
file_name <- "resultsprobit_tukauyou_treatmenthenkou.tex" 
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
output_file <- "marginal_effects_table.tex"

# msummary()を使ってLaTeX形式でファイルに出力
msummary(
  list_of_ME,
  estimate = "{estimate} ({std.error}){stars}", # 推定値と標準誤差、p値スターの表示形式
  output = output_file,
  title = "Probit Models: Marginal Effects on Contribution", # テーブルのタイトル
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01", # ノート
  gof_map = c("nobs", "aic") # 表示するGoodness-of-Fit統計量（marginsオブジェクトでもnobsは表示可能）
)
