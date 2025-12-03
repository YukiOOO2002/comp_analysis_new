# 必要なライブラリを読み込みます
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

rm(list = ls()) 
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") 
path <- "015/data/merge_final.csv"
df <- read_csv(path, show_col_types = FALSE)

df <- df %>%
  mutate(
    group_type = str_extract(`participant.label`, "^base|^0\\d{2}|^\\d{2}")
  )

num_rounds <- 10

# ラウンドごとに処理を繰り返すための関数を定義（再掲）
calculate_max_flags <- function(data, round_num) {
  r_contribute <- paste0("round ", round_num, "_contribute")
  r_payoff <- paste0("round ", round_num, "_payoff")
  r_id_subsession <- paste0("round ", round_num, "_id_subsession")
  
  # 新しい列名
  new_col_contribute <- paste0("round ", round_num, "_max_contribute")
  new_col_payoff <- paste0("round ", round_num, "_max_payoff")
  
  # グループ化と最大値の計算
  data <- data %>%
    group_by(group_type, across(all_of(r_id_subsession))) %>%
    mutate(
      # contributeの最大値と比較してフラグを設定
      !!new_col_contribute := ifelse(
        .data[[r_contribute]] == max(.data[[r_contribute]], na.rm = TRUE),
        1,
        0
      ),
      
      # payoffの最大値と比較してフラグを設定
      !!new_col_payoff := ifelse(
        .data[[r_payoff]] == max(.data[[r_payoff]], na.rm = TRUE),
        1,
        0
      )
    ) %>%
    ungroup()
  
  return(data)
}

# 全てのラウンドに対して処理を適用（df2を作成）
df2 <- df
for (i in 1:num_rounds) {
  df2 <- calculate_max_flags(df2, i)
}

# --- 新しい処理: max_c_and_p列の作成 ---

# 新しい列を作成するためのループ
for (i in 1:num_rounds) {
  # 既存のフラグ列名
  col_contribute <- paste0("round ", i, "_max_contribute")
  col_payoff <- paste0("round ", i, "_max_payoff")
  
  # 新しい結合フラグ列名
  new_col_combined <- paste0("round ", i, "_max_c_and_p")
  
  # 両方の条件が1である場合に1、それ以外は0を設定
  df2 <- df2 %>%
    mutate(
      # `&` 演算子で両方の条件（1かつ1）を満たすか確認します
      # どちらかの列が存在しない場合はエラーを防ぐため、存在チェックを推奨しますが、
      # このコードでは前のステップで作成されていることが保証されていると仮定します。
      !!new_col_combined := ifelse(
        .data[[col_contribute]] == 1 & .data[[col_payoff]] == 1,
        1,
        0
      )
    )
}

# --- 結果の確認 ---
cat("--- 処理されたデータ（新しい結合フラグ列を含む）---\n")

# 確認用に、ラウンド1のフラグ列と新しい結合フラグ列を表示
select_cols_check <- c(
  "participant.label", "group_type", 
  "round 1_contribute", "round 1_payoff", 
  "round 1_max_contribute", "round 1_max_payoff",
  "round 1_max_c_and_p" # 新しく追加された列
)

existing_cols_check <- intersect(select_cols_check, names(df2))

write.csv(
  df2, 
  file = "015/data/wide_dummy_p_and_c.csv",
  row.names = FALSE,   
  quote = FALSE       
)