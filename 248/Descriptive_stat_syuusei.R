rm(list = ls())
library(xtable)
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

# 0. 初期設定とデータの読み込み
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")

# データファイルパス
path <- "015/data/merge_long.csv" 
data <- read_csv(path, show_col_types = FALSE)

# 1. 貢献額 (contribute) の記述統計量の計算（グループが行、統計量が列）
stats_df_cols <- data %>%
  group_by(group_type) %>%
  summarise(
    # 列名の調整: LaTeX出力時の列名になる
    `観測値の数 (Obs.)` = n(),                 
    `平均値 (Mean)` = round(mean(contribute), 3),
    `標準偏差 (S.D.)` = round(sd(contribute), 3),
    `中央値 (Median)` = median(contribute),
    `最小値 (Min)` = min(contribute),
    `最大値 (Max)` = max(contribute)
  ) %>%
  ungroup()

# 2. 結果の確認 (コンソール出力)
print("--- グループが行、統計量が列のデータフレーム ---")
print(stats_df_cols)
print("-------------------------------------------------")


# 3. データフレームをLaTeX形式に変換し、指定の相対パスに出力
# 出力ディレクトリの相対パスを設定
output_dir <- "015/analysis/" 

# ディレクトリが存在しない場合は作成
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  print(paste0("ディレクトリ '", output_dir, "' を作成しました。"))
}

# 出力ファイルのフルパス (ファイル名を変更)
output_file_name <- "descriptive_stats_grouped_transposed.tex" 
output_path <- paste0(output_dir, output_file_name)


# xtableオブジェクトの作成
xtable_obj <- xtable(
  stats_df_cols, 
  caption = "グループ別 貢献額に関する記述統計量 (転置形式)", 
  label = "tab:descriptive_statistics_transposed",
  # group_type列(左寄せ), その他の統計量列(右寄せ)
  align = c("l", "l", rep("r", ncol(stats_df_cols) - 1)) 
)

# テーブルをファイルに出力
print(
  xtable_obj,
  file = output_path,
  include.rownames = FALSE, # 行番号を非表示
  include.colnames = TRUE,  # 列名を表示
  booktabs = TRUE,          # \toprule, \midrule, \bottomrule を使用
  floating = TRUE
)

print(paste0("LaTeX形式の表がファイル '", output_path, "' に出力されました。"))