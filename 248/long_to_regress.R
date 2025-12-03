rm(list = ls())
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)

# group_type の望ましい順序を定義
desired_order <- c("T0", "TL", "TM", "TH")

# -----------------------------------------------------------
# ステップ 1: 初期設定とデータ読み込み
# -----------------------------------------------------------

setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") # 実行環境に合わせて変更してください

path <- "015/data/merge_final_dummy.csv" # ファイルパスを適切に設定してください
df <- read_csv(path, show_col_types = FALSE)

path2 <- "015/data/merge_final_dummy_c0.csv"
df2 <- read_csv(path2, show_col_types = FALSE)

# -----------------------------------------------------------
# ステップ 2: グループ順序の固定 (ファクター変換)
# -----------------------------------------------------------

# df の group_type を factor に変換し、順序を固定
df <- df %>%
  mutate(group_type = factor(group_type, levels = desired_order))

# df2 の group_type を factor に変換し、順序を固定
df2 <- df2 %>%
  mutate(group_type = factor(group_type, levels = desired_order))

# -----------------------------------------------------------
# ステップ 3: ロング形式への変換
# -----------------------------------------------------------

df_long <- df %>%
  pivot_longer(
    # 'round n_'で始まる全ての列を指定
    cols = starts_with("round"),
    
    # 正規表現で列名を分解し、新しい列名を定義
    names_to = c("round", ".value"), 
    names_pattern = "round (\\d+)_(.*)"
  ) %>%
  # round列を整数型に変換
  mutate(round = as.integer(round)) %>%
  # ✨ 修正箇所: group_type を最優先でソートに含める
  # group_typeはfactorなので、levelsの順序でソートされる
  arrange(group_type, `participant.label`, round)

df_long2 <- df2 %>%
  pivot_longer(
    # 'round n_'で始まる全ての列を指定
    cols = starts_with("round"),
    
    # 正規表現で列名を分解し、新しい列名を定義
    names_to = c("round", ".value"), 
    names_pattern = "round (\\d+)_(.*)"
  ) %>%
  # round列を整数型に変換
  mutate(round = as.integer(round)) %>%
  # ✨ 修正箇所: group_type を最優先でソートに含める
  arrange(group_type, `participant.label`, round)

# -----------------------------------------------------------
# ステップ 4: CSVファイルの書き出し
# -----------------------------------------------------------

write.csv(
  df_long, 
  file = "015/data/merge_long.csv",
  row.names = FALSE,   
  quote = FALSE       
)
write.csv(
  df_long2, 
  file = "015/data/merge_long_c0.csv",
  row.names = FALSE,   
  quote = FALSE       
)