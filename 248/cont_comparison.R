rm(list = ls())
library(tidyverse)
library(readr) 
library(dplyr)
library(stringr)

path_1st <- "015/Oct29_1030_015_1st/comp_demo_015_2025-10-29_1030.csv"
df_1st <- read_csv(path_1st, show_col_types = FALSE)
path_2nd <- "015/Oct29_1330_015_2nd/comp_demo_015_2025-10-29_1330.csv"
df_2nd <- read_csv(path_2nd, show_col_types = FALSE)
df_2nd <- subset(df_2nd, participant._current_page_name == "final")

path_base_1st <- "015/Oct28_1330_base_1st/comp_demo_base_2025-10-28.csv"
df_base_1st <- read_csv(path_base_1st, show_col_types = FALSE)
df_base_1st <- df_base_1st %>%
  
  mutate(
    participant.label = paste0("base", participant.label)
  )



df_2nd <- df_2nd %>%
  mutate(
    label_num_char = str_extract(participant.label, "\\d+"),
    new_num = as.numeric(label_num_char) + 24,
    new_label = paste0("sub", str_pad(new_num, width = 2, pad = "0")),
    participant.label = new_label
  ) %>%
  
  select(-label_num_char, -new_num, -new_label)

df_base_1st <- df_base_1st %>%
  mutate(is_base = 0)

df_1st <- df_1st %>%
  mutate(is_base = 1)

df_2nd <- df_2nd %>%
  mutate(is_base = 1)

df_combined <- bind_rows(df_base_1st, df_1st, df_2nd)

cols_to_drop <- c(
  "participant._is_bot",
  "participant._index_in_pages",
  "participant._max_page_index",
  "participant._current_app_name",
  "participant._current_page_name",
  "participant.time_started_utc",
  "participant.visited",
  "participant.mturk_worker_id",
  "participant.mturk_assignment_id",
  "player.role",
  "session.code",
  "session.label",
  "session.mturk_HITId",
  "session.mturk_HITGroupId",
  "session.comment",
  "session.is_demo",
  "player.payoff",
  "player.round_data",
  "group.individual_share"
)

df_combined <- df_combined %>%
  select(-all_of(cols_to_drop))

df_wide <- df_combined %>%
  rename(
    # あなたのデータに合わせて、元の列名を修正してください
    contribute = player.individual_choice, 
    mpcr = player.assigned_mpcr,
    payoff = player.round_payoff,
    total_contribution = group.total_contribution 
  ) %>%
  # is_base をIDとして含め、ワイド型へ変換します。
  pivot_wider(
    id_cols = c(participant.label, is_base), # ★修正: is_base を追加★
    names_from = subsession.round_number,
    values_from = c(contribute, mpcr, payoff, total_contribution),
    names_glue = "round {subsession.round_number}_{.value}"
  )


df_wide <- df_combined %>%
  # 3-1. pivot_widerで新しい列名を作成するために、元の列名をベース名にリネーム
  rename(
    contribute = player.individual_choice, 
    mpcr = player.assigned_mpcr,
    payoff = player.round_payoff,
    total_contribution = group.total_contribution
  ) %>%
  # 3-2. participant.labelとis_baseをキーとしてワイド型へ変換
  pivot_wider(
    id_cols = c(participant.label, is_base), 
    names_from = subsession.round_number, 
    values_from = c(contribute, mpcr, payoff, total_contribution), 
    names_glue = "round {subsession.round_number}_{.value}"
  )

# 4. is_base ごとのラウンド平均貢献額の計算とグラフ描画

# 4-1. グラフ作成のために、ワイド型から貢献額の列だけを抽出してロング型に戻す
df_comp_long_contribute <- df_wide %>%
  pivot_longer(
    cols = ends_with("contribute"),
    names_to = "round_col",
    values_to = "contribution"
  ) %>%
  mutate(
    round_number = as.integer(str_extract(round_col, "\\d+")),
    # is_base の値 (0, 1) をラベルに変換
    is_base_label = factor(is_base, 
                           levels = c(0, 1), 
                           labels = c("Baseline", "a=0.15"))
  )

# 4-2. is_base ごと、ラウンドごとに貢献額の平均を計算
df_average_contribution <- df_comp_long_contribute %>%
  group_by(is_base_label, round_number) %>% 
  summarise(
    average_contribution = mean(contribution, na.rm = TRUE),
    .groups = 'drop'
  )

# 4-3. 折れ線グラフの描画（is_base ごとに色分け）
plot_contribution <- df_average_contribution %>%
  # ★修正: colorとgroupを is_base_label で指定★
  ggplot(aes(x = round_number, y = average_contribution, 
             color = is_base_label, group = is_base_label)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  
  labs(
    title = "Average Contribution by Round (Base vs a=0.15)",
    x = "Round Number", 
    y = "The amount of average contribution",
    color = "Data Source" # 凡例のタイトル
  ) +
  
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) + 
  scale_x_continuous(breaks = unique(df_average_contribution$round_number)) +
  
  theme_minimal() 

# 4-4. グラフをPNG形式で保存
ggsave("average_contribution_base_vs_experiment_plot.png", plot = plot_contribution, width = 8, height = 5)

# 5. ラウンドごとのグループ合計貢献額の平均の計算とグラフ描画

# 5-1. グラフ作成のために、ワイド型から合計貢献額の列だけを抽出してロング型に戻す
# ★is_base ごとの合計貢献額の平均を計算するため、long型に変換し直す★
df_comp_long_total_contribute <- df_wide %>%
  # 'total_contribution'で終わる全ての列を選択
  pivot_longer(
    cols = ends_with("total_contribution"),
    names_to = "round_col",
    values_to = "total_contribution"
  ) %>%
  # round_col からラウンド番号を抽出
  mutate(
    round_number = as.integer(str_extract(round_col, "\\d+")),
    # is_base の値 (0, 1) をラベルに変換
    is_base_label = factor(is_base, 
                           levels = c(0, 1), 
                           labels = c("Baseline", "a=0.15"))
  )

# 5-2. is_base ごと、ラウンドごとにグループ合計貢献額の平均を計算
df_average_total_contribution <- df_comp_long_total_contribute %>%
  group_by(is_base_label, round_number) %>% # ★グループ化に is_base_label を追加★
  summarise(
    average_total_contribution = mean(total_contribution, na.rm = TRUE),
    .groups = 'drop'
  )

# 5-3. 折れ線グラフの描画（is_base ごとに色分け）
plot_total_contribution <- df_average_total_contribution %>%
  ggplot(aes(x = round_number, y = average_total_contribution,
             # ★色分けとグループ化を is_base_label で指定★
             color = is_base_label, group = is_base_label)) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  
  labs(
    title = "Average Total Contribution by Round (Base vs a=0.15)",
    x = "Round Number", 
    y = "The amount of average total contribution",
    color = "Data Source" # 凡例のタイトル
  ) +
  
  # 縦軸の範囲を0から30に固定
  scale_y_continuous(limits = c(0, 30)) + 
  
  scale_x_continuous(breaks = unique(df_average_total_contribution$round_number)) +
  
  theme_minimal()

# 5-4. グラフをPNG形式で保存
ggsave("average_total_contribution_base_vs_experiment_plot.png", plot = plot_total_contribution, width = 8, height = 5)


# 6. 全ラウンドのcontributionに関する記述統計量の計算 (標準偏差を含む)

# df_comp_long_contribute は前々回のコードで作成されています。



df_summary_contribute_sd <- df_comp_long_contribute %>%
  
  # グループ化はせず、データセット全体を対象とします
  
  summarise(
    
    # 平均値
    
    Mean = mean(contribution, na.rm = TRUE),
    
    
    
    # 中央値
    
    Median = median(contribution, na.rm = TRUE),
    
    
    
    # 標準偏差 (sd = Standard Deviation)
    
    Standard_Deviation = sd(contribution, na.rm = TRUE),
    
    
    
    # サンプル数 (欠損値を除いた観測値の数)
    
    N = sum(!is.na(contribution))
    
  )

library(ggplot2) # グラフ描画
library(stringr) # 文字列処理 (IDから番号を抽出)

# 7-1. 実験群 (is_base=1) のデータのみをフィルタリング（以前の処理を再利用）
df_experiment_individuals <- df_comp_long_contribute %>%
  filter(is_base == 1) 

# -------------------------------------------------------------
# 7-2. IDに基づいてグループ化し、10人ずつプロット
# -------------------------------------------------------------

# IDの数字部分を抽出し、10で割ったグループ番号 (1, 2, 3, 4, 5) を作成
df_grouped_individuals <- df_experiment_individuals %>%
  mutate(
    # participant.label から数字部分を抽出（例: "sub25" -> 25）
    participant_num = as.integer(str_extract(participant.label, "\\d+")),
    # 1〜10, 11〜20, 21〜30, ... のグループに分ける
    group_chunk = ceiling(participant_num / 10)
  )

# 各グループ（1〜5）ごとにプロットを作成し、保存するループ
for (i in unique(df_grouped_individuals$group_chunk)) {
  
  # i番目のグループのデータをフィルタリング
  df_plot <- df_grouped_individuals %>%
    filter(group_chunk == i)
  
  # グループに含まれる最初のIDと最後のIDを取得し、ファイル名とタイトルに使用
  min_id <- min(df_plot$participant.label)
  max_id <- max(df_plot$participant.label)
  
  plot_individual <- df_plot %>%
    ggplot(aes(x = round_number, y = contribution, 
               group = participant.label, color = participant.label)) +
    
    geom_line(alpha = 0.8, linewidth = 1) +
    geom_point(size = 2.5) +
    
    labs(
      title = paste0("Individual Contributions (", min_id, " to ", max_id, ")"),
      subtitle = "Experiment Group (a=0.15), 10 participants per plot",
      x = "Round Number",
      y = "Contribution Amount",
      color = "Participant ID"
    ) +
    
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) + 
    scale_x_continuous(breaks = unique(df_plot$round_number)) +
    
    theme_minimal() +
    theme(legend.position = "right")
  
  # ファイル名を動的に生成し、画像を保存
  file_name <- paste0("individual_contribution_group_", i, "_", min_id, "_to_", max_id, ".png")
  ggsave(file_name, plot = plot_individual, width = 10, height = 6)
  cat(paste0("グラフを保存しました: ", file_name, "\n"))
}

df_baseline_individuals <- df_comp_long_contribute %>%
  filter(is_base == 0) 

# IDの数字部分を抽出し、10で割ったグループ番号 (1, 2, 3, ...) を作成
df_grouped_baseline <- df_baseline_individuals %>%
  mutate(
    # participant.label から数字部分を抽出（例: "basesub05" -> 05）
    # ベースラインIDのプレフィックス "base" が邪魔にならないよう、ID全体から数字を抽出
    participant_num = as.integer(str_extract(participant.label, "\\d+")),
    # 1〜10, 11〜20, ... のグループに分ける
    group_chunk = ceiling(participant_num / 10)
  )

# 各グループごとにプロットを作成し、保存するループ
for (i in unique(df_grouped_baseline$group_chunk)) {
  
  # i番目のグループのデータをフィルタリング
  df_plot <- df_grouped_baseline %>%
    filter(group_chunk == i)
  
  # グループに含まれる最初のIDと最後のIDを取得
  min_id <- min(df_plot$participant.label)
  max_id <- max(df_plot$participant.label)
  
  plot_individual <- df_plot %>%
    ggplot(aes(x = round_number, y = contribution, 
               group = participant.label, color = participant.label)) +
    
    geom_line(alpha = 0.8, linewidth = 1) +
    geom_point(size = 2.5) +
    
    labs(
      title = paste0("Individual Contributions (", min_id, " to ", max_id, ")"),
      subtitle = "Baseline Group (is_base=0), 10 participants per plot",
      x = "Round Number",
      y = "Contribution Amount",
      color = "Participant ID"
    ) +
    
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) + 
    scale_x_continuous(breaks = unique(df_plot$round_number)) +
    
    theme_minimal() +
    theme(legend.position = "right")
  
  # ファイル名を動的に生成し、画像を保存
  file_name <- paste0("individual_contribution_baseline_group_", i, "_", min_id, "_to_", max_id, ".png")
  ggsave(file_name, plot = plot_individual, width = 10, height = 6)
  
  cat(paste0("グラフを保存しました: ", file_name, "\n"))
}  

