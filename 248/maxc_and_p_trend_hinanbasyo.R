# 必要なライブラリを読み込みます
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)


rm(list = ls())
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") 
path <- "015/data/merge_final.csv"
df <- read_csv(path, show_col_types = FALSE)


df <- df %>%
  mutate(
    group_type = str_extract(`participant.label`, "T0|TL|TM|TH")
  )

# 処理するラウンド数を設定
num_rounds <- 10

# ラウンドごとに処理を繰り返すための関数を定義
calculate_max_flags <- function(data, round_num) {
  # 動的な列名の生成
  r_contribute <- paste0("round ", round_num, "_contribute")
  r_payoff <- paste0("round ", round_num, "_payoff")
  r_id_subsession <- paste0("round ", round_num, "_id_subsession")
  
  # 新しいフラグ列名
  new_col_contribute <- paste0("round ", round_num, "_max_contribute")
  new_col_payoff <- paste0("round ", round_num, "_max_payoff")
  
  # グループ化と最大値の計算
  data <- data %>%
    # group_type と round n_id_subsession でグループ化
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

# 全てのラウンドに対して最大値フラグの計算処理を適用
df2 <- df
for (i in 1:num_rounds) {
  df2 <- calculate_max_flags(df2, i)
}

for (i in 1:num_rounds) {
  col_contribute <- paste0("round ", i, "_max_contribute")
  col_payoff <- paste0("round ", i, "_max_payoff")
  new_col_combined <- paste0("round ", i, "_max_c_and_p")
  
  # 両方の条件 (1かつ1) が満たされた場合に1を設定
  df2 <- df2 %>%
    mutate(
      !!new_col_combined := ifelse(
        .data[[col_contribute]] == 1 & .data[[col_payoff]] == 1,
        1,
        0
      )
    )
}

# 処理対象となる round n_max_c_and_p の列名をリスト化
max_c_and_p_cols <- paste0("round ", 1:num_rounds, "_max_c_and_p")

# df2をロング形式に変換し、グループごとに集計
df_summary <- df2 %>%
  # group_type列と max_c_and_pの列のみを選択
  select(group_type, all_of(max_c_and_p_cols)) %>%
  
  # ロング形式に変換 (max_c_and_pの列のみを対象とする)
  pivot_longer(
    cols = all_of(max_c_and_p_cols), 
    names_to = "round_col",
    values_to = "flag_value"
  ) %>%
  
  # ラウンド番号を抽出
  mutate(
    round = as.numeric(str_extract(round_col, "\\d+")), 
    .before = round_col
  ) %>%
  
  # group_typeとroundでグループ化して集計
  group_by(group_type, round) %>%
  summarise(
    # round n_max_c_and_p の合計を計算
    total_flag_sum = sum(flag_value, na.rm = TRUE),
    
    # 指示通り、合計を48で割って平均値を計算
    average_max_c_and_p_48 = total_flag_sum / 48, 
    
    .groups = 'drop'
  ) %>%
  # 必要な列のみに絞る
  select(group_type, round, average_max_c_and_p = average_max_c_and_p_48)

# --- 新しいデータフレームの確認 ---
cat("\n--- ラウンド別、群別の平均値（新しいデータフレーム）---\n")
print(df_summary)

# --- 変更点: group_typeの値を置換し、グラフを作成する ---

# 折れ線グラフを作成
plot_max_c_and_p_by_round <- df_summary %>%
  # グラフ化対象の4群にデータをフィルタリング
  filter(group_type %in% c("base", "015", "03", "045")) %>%
  
  # group_type の値を新しいラベルに置換
  mutate(
    group_type = case_when(
      group_type == "base" ~ "T0",
      group_type == "015"  ~ "TL",
      group_type == "03"   ~ "TM",
      group_type == "045"  ~ "TH",
      TRUE ~ group_type # 上記以外はそのまま
    )
  ) %>%
  
  # group_type の表示順序を指定 (グラフの凡例順序を制御するため)
  # 任意: 順序を指定したい場合は、factor型に変換してレベルを設定
  mutate(
    group_type = factor(group_type, levels = c("T0", "TL", "TM", "TH"))
  ) %>%
  
  ggplot(aes(x = round, y = average_max_c_and_p, color = group_type, group = group_type)) +
  geom_line(linewidth = 1) + # 折れ線
  geom_point(size = 2) +     # 各ラウンドの点を表示
  labs(
    title = "the proportion of simultaneous gain of max contribute and payoff",
    x = "round",
    y = "proportion",
    color = "treatment"
  ) +
  scale_x_continuous(breaks = 1:num_rounds) + # 横軸を1から10までの整数のみにする
  scale_y_continuous(limits = c(0, NA)) + # y軸は0から開始
  theme_gray() + # シンプルなテーマとフォント
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# グラフを表示
print(plot_max_c_and_p_by_round)

save_dir <- "015/analysis/"
all_trend <- paste0(save_dir, "max_cont_and_payoff_proportion_T_labels1127.png") # ファイル名を変更
ggsave(
  filename = all_trend,  
  plot = plot_max_c_and_p_by_round,
  device = "png",
  width = 8,
  height = 5,
  units = "in"
)

# グラフと保存処理の間の区切り
cat("\n--- グラフの保存先 ---\n")
cat(paste0("グラフは '", all_trend, "' として保存されました。\n"))