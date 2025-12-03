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

df_zen <- df3 %>% 
  filter(round>=6)
df_kou <- df3 %>% 
  filter(round<=5)


modelglm_arr_zen <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender + affiliation,
  data = df_zen,
  family = binomial(link = "probit") 
)


modelglm_arr_kou <- glm(
  formula = contribute_full~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender + affiliation,
  data = df_kou,
  family = binomial(link = "probit") 
)

save_dir <- "015/regression/" 
file_name <- "probit_zenhan_kouhan.tex" 
full_path <- paste0(save_dir, file_name)

stargazer(modelglm_arr_zen, modelglm_arr_kou,
          type = "latex", 
          out = full_path, 
          title = "Probit Model Results ",
          omit = c("age", "gender", "affiliation"))

modelglm_arrnon_zen <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender + affiliation,
  data = df_zen,
  family = binomial(link = "probit") 
)

modelglm_arrnon_kou <- glm(
  formula = contribute_non~
    alpha_positive + beta_positive +
    TL + TM + TH+
    age + gender + affiliation,
  data = df_kou,
  family = binomial(link = "probit") 
)
file_name1 <- "probit_zenhan_kouhan_c0.tex" 
full_path1 <- paste0(save_dir, file_name1)

stargazer(modelglm_arrnon_zen, modelglm_arrnon_kou,
          type = "latex", 
          out = full_path1, 
          title = "Probit Model Results ",
          omit = c("age", "gender", "affiliation")
          
)

AME_list <- list(
  "1"=modelglm_arr_zen,
  "2"=modelglm_arr_kou
)

AME_listnon <- list(
  "3"=modelglm_arrnon_zen,
  "4"=modelglm_arrnon_kou
)

list_of_AME <-  lapply(AME_list, FUN=margins)
list_of_AMEnon <- lapply(AME_listnon, FUN=margins)
output_file <- "marginal_effects_table_by_round.tex"
output_filenon <- "marginal_effects_table_by_round_c0.tex"
# msummary()を使ってLaTeX形式でファイルに出力
modelsummary(
  list_of_AME,
  estimate = "{estimate}{stars}", # 推定値と標準誤差、p値スターの表示形式
  output = output_file,
  title = "Probit Models: Marginal Effects on Contribution", # テーブルのタイトル
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01", # ノート
  gof_map = c("nobs", "aic") 
)
modelsummary(
  list_of_AMEnon,
  estimate = "{estimate}{stars}", # 推定値と標準誤差、p値スターの表示形式
  output = output_filenon,
  title = "Probit Models: Marginal Effects on Contribution", # テーブルのタイトル
  notes = "Standard errors in parentheses. * p < 0.1, ** p < 0.05, *** p < 0.01", # ノート
  gof_map = c("nobs", "aic") 
)
