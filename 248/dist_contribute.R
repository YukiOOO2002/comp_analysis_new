rm(list = ls())
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)

Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("././././.")

path <- "015/data/merge_long.csv" # ファイルパスを適切に設定してください
df <- read_csv(path, show_col_types = FALSE)
df

# 分析対象の群と貢献度 (0-10) の範囲を設定
groups_to_keep <- c("T0", "TL", "TM", "TH")
contribute_levels <- factor(0:10, levels = 0:10) # X軸のカテゴリを保証するため

df_proportions <- df %>%
  # group_typeとcontributeの範囲でフィルタリング
  filter(group_type %in% groups_to_keep) %>%
  filter(contribute >= 0, contribute <= 10) %>%
  
  # group_typeとcontributeでグループ化し、割合を計算
  group_by(group_type, contribute) %>%
  summarise(count = n(), .groups = 'drop_last') %>% # 各グループ/貢献度のカウント
  mutate(proportion = count / sum(count)) %>%      # 各グループ内での割合を計算
  ungroup() %>%
  
  # contributeを因子型に変換 (プロットの順序を正しくするため)
  mutate(contribute = factor(contribute, levels = 0:10))


# すべての群のリスト
groups <- unique(df_proportions$group_type)

# Y軸の最大値を1.0に固定して比較しやすくする
y_limit <- 1.0

# プロットのループ処理
for (g in groups_to_keep) {
  # 現在の群のデータのみを抽出
  df_group <- df_proportions %>%
    filter(group_type == g)
  
  # ファイル名を設定
  filename <- paste0("contribute_proportion_", g, "1127.png")
  
  # プロットを作成
  plot <- ggplot(df_group, aes(x = contribute, y = proportion)) +
    # 棒グラフ
    geom_bar(stat = "identity", fill = "steelblue") +
    # タイトルとラベル
    labs(
      title = paste0("The Proportion of the Amount of Contribution in Treatment", g),
      x = "contribute",
      y = "proportion"
    ) +
    # Y軸の範囲を固定
    scale_y_continuous(limits = c(0, y_limit)) +
    # テーマを設定
    theme_gray() +
    theme(
      plot.title = element_text(hjust = 0.5), # タイトル中央揃え
      axis.text.x = element_text(angle = 0)
    )
  
  # PNGファイルとして保存
  ggsave(filename, plot, width = 8, height = 6, units = "in", dpi = 300)
  
  cat(paste0("プロットを ", filename, " として保存しました。\n"))
}

# (注: R環境外ではcat()で出力されたファイルは自動で提供されません)