rm(list = ls())
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)


setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ") 

path <- "015/data/merge_final.csv" # ファイルパスを適切に設定してください
df <- read_csv(path, show_col_types = FALSE)

# group_type 列の作成
df <- df %>%
  mutate(
    group_type = str_extract(`participant.label`, "^T0|^TL|^TM|~TH")
  )

num_rounds <- 10

# calculate_max_flags 関数（contribute != 0 の条件を削除）
calculate_max_flags <- function(data, round_num) {
  r_contribute <- paste0("round ", round_num, "_contribute")
  r_payoff <- paste0("round ", round_num, "_payoff")
  r_id_subsession <- paste0("round ", round_num, "_id_subsession")
  
  new_col_contribute <- paste0("round ", round_num, "_max_contribute")
  new_col_payoff <- paste0("round ", round_num, "_max_payoff")
  
  data <- data %>%
    group_by(group_type, across(all_of(r_id_subsession))) %>%
    mutate(
      # contributeの最大値を取得 (ヘルパー列)
      max_contribute_group = max(.data[[r_contribute]], na.rm = TRUE),
      
      # 【修正箇所】: contributeが最大であることのみを条件とする
      !!new_col_contribute := ifelse(
        .data[[r_contribute]] == max_contribute_group,
        1,
        0
      ),
      
      !!new_col_payoff := ifelse(
        .data[[r_payoff]] == max(.data[[r_payoff]], na.rm = TRUE),
        1,
        0
      ),
      
      max_contribute_group = NULL  
    ) %>%
    ungroup()
  
  return(data)
}

# 全てのラウンドに対して最大値フラグの計算処理を適用
df2 <- df
for (i in 1:num_rounds) {
  df2 <- calculate_max_flags(df2, i)
}

# max_c_and_p列の作成
for (i in 1:num_rounds) {
  col_contribute <- paste0("round ", i, "_max_contribute")
  col_payoff <- paste0("round ", i, "_max_payoff")
  new_col_combined <- paste0("round ", i, "_max_c_and_p")
  
  df2 <- df2 %>%
    mutate(
      !!new_col_combined := ifelse(
        .data[[col_contribute]] == 1 & .data[[col_payoff]] == 1,
        1,
        0
      )
    )
}

# -----------------------------------------------------------
# ステップ 3: グループフラグ列の作成関数と適用
# -----------------------------------------------------------

#' participant.labelに基づいてバイナリフラグ列を作成する関数
#'
#' @param data データフレーム
#' @param group_names フラグ列を作成したいグループ名のベクトル (例: c("base", "015", "03", "045"))
#' @return 新しいフラグ列が追加されたデータフレーム
create_group_flag_columns <- function(data, group_names) {
  
  for (group in group_names) {
    # '015', '03', '045' に X プレフィックスを付けず、そのままの列名を使用
    col_name <- ifelse(group == "T0", "T0", group)  
    
    data <- data %>%
      mutate(
        # !!as.name(col_name) はバッククォートで囲まれた列名として解釈される
        !!as.name(col_name) := ifelse(
          group_type == group,
          1,
          0
        )
      )
  }
  
  return(data)
}

# 関数を適用して新しい列を作成
target_groups <- c("T0", "TL", "TM", "TH")
df2 <- create_group_flag_columns(df2, target_groups)

# 作成された列の確認 (最初の数行と、新しく追加された列)
cat("\n--- 新規追加されたグループフラグ列の確認 ---\n")
# バッククォートを使用して数字から始まる列名を選択
print(head(df2 %>% select(group_type, T0, `TL`, `TM`, `TH`, `participant.label`)))


# -----------------------------------------------------------
# ステップ 4: 集計とグラフ化
# -----------------------------------------------------------

# 処理対象となる round n_max_c_and_p の列名をリスト化
max_c_and_p_cols <- paste0("round ", 1:num_rounds, "_max_c_and_p")

# group_type の望ましい順序を定義
desired_order <- c("T0", "TL", "TM", "TH") # 修正箇所: 順序を明示的に指定

# df2をロング形式に変換し、グループごとに集計
df_summary <- df2 %>%
  select(group_type, all_of(max_c_and_p_cols)) %>%
  pivot_longer(
    cols = all_of(max_c_and_p_cols), 
    names_to = "round_col",
    values_to = "flag_value"
  ) %>%
  mutate(
    # ✨ 修正箇所: group_type を factor に変換し、順序を指定する
    group_type = factor(group_type, levels = desired_order),
    round = as.numeric(str_extract(round_col, "\\d+")), 
    .before = round_col
  ) %>%
  group_by(group_type, round) %>%
  summarise(
    total_flag_sum = sum(flag_value, na.rm = TRUE),
    average_max_c_and_p_48 = total_flag_sum / 48, 
    .groups = 'drop'
  ) %>%
  select(group_type, round, average_max_c_and_p = average_max_c_and_p_48)

# --- 新しいデータフレームの確認 ---
cat("\n--- ラウンド別、群別の平均値（新しいデータフレーム）---\n")
# factor化により望ましい順序で表示されることを確認
print(df_summary)

# 折れ線グラフを作成
plot_max_c_and_p_by_round <- df_summary %>%
  # factor化したので filter は levels の順序に影響しないが、念のため残す
  filter(group_type %in% c("T0", "TL", "TM", "TH")) %>% 
  ggplot(aes(x = round, y = average_max_c_and_p, color = group_type, group = group_type)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 2) + 
  labs(
    title = "ラウンドを通じた最大貢献度＆最大ペイオフ獲得率の推移 (4群比較)",
    x = "ラウンド",
    y = "平均最大獲得フラグ（/48）",
    color = "群の種類"
  ) +
  scale_x_continuous(breaks = 1:num_rounds) + 
  scale_y_continuous(limits = c(0, NA)) + 
  theme_minimal(base_family = "sans") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# グラフを表示
print(plot_max_c_and_p_by_round)

# CSVファイルの書き出し
write.csv(
  df2, 
  file = "015/data/merge_final_dummy.csv",
  row.names = FALSE,  
  quote = FALSE      
)