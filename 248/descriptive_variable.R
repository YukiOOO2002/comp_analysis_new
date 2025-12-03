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
library(car)
library(tidyr)
library(knitr)
library(kableExtra)
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE)
df <- df %>% 
  mutate(man = case_when(
    gender ==0 ~1,
    TRUE ~ 0
    
  ))

# 3. 集計対象の変数リスト
all_vars <- c("alpha_positive", "beta_positive","alpha_med","beta_med", "age", "science", "graduate", "man")
# 4. 各変数の集計結果を格納するリスト
summary_list <- list()

N_all <- nrow(df)
N_filtered <- nrow(df)

# 5. 全変数の集計処理
for (var in all_vars) {
  # 集計に使用するデータフレームと標本数を設定
  if (var == "gender") {
    df_use <- df
    N_use <- N_filtered
  } else {
    df_use <- df
    N_use <- N_all
  }
  
  # 変数が factor の場合、数値に変換
  if (is.factor(df_use[[var]])) {
    x <- as.numeric(df_use[[var]])
  } else {
    x <- df_use[[var]]
  }
  
  # 統計量の計算 (SDに変更)
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE) # Standard Deviation を計算
  max_val <- max(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  
  # 結果をデータフレーム形式に整形 (英語表記に変更)
  summary_list[[var]] <- data.frame(
    Variable = paste0(stringr::str_to_title(var)),
    Obs. = N_use,
    Mean = mean_val,
    SD = sd_val, # 列名を変更
    Max = max_val,
    Min = min_val
  )
}

# 6. 全ての結果を結合
final_summary_table <- bind_rows(summary_list)

# 7. 表示の調整（小数点第2位に丸め）
final_summary_table <- final_summary_table %>%
  mutate(
    Mean = round(Mean, 3),
    SD = round(SD, 3), # SDの丸め
    Max = round(Max, 3),
    Min = round(Min, 3)
  )

help(round)
# 最終的な要約統計表の表示
print(final_summary_table)

output_dir <- "015/analysis/"
output_filename <- "summary_table_syuusei.tex"
output_path <- paste0(output_dir, output_filename)
final_summary_table %>%
  # データを kable 形式に変換
  kbl(
    caption = "要約統計量テーブル",
    align = 'c',
    booktabs = TRUE,
    format = "latex"
  ) %>%
  # kableExtraでスタイルを適用 (見やすくするために)
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  # 指定された相対パスにHTMLファイルとして保存
  save_kable(file = output_path)



