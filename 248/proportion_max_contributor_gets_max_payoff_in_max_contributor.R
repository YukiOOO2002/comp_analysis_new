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
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 
df_summary <- df %>%
  # max_contributeが1であるデータに絞り込む
  filter(max_contribute == 1) %>%
  # roundとgroup_typeでグループ化
  group_by(round, group_type) %>%
  # 新しい変数 proportion_max_payoff_1 を作成 (max_payoffの平均値=割合)
  summarise(
    proportion_max_payoff_1 = mean(max_payoff == 1),
    .groups = 'drop' # グループ化を解除
  )
desired_order <- c("T0", "TL", "TM", "TH")
df_summary$group_type <- factor(df_summary$group_type, levels = desired_order)

# 3. １つの図表に４つの折れ線グラフを重ねて作成する
p <- ggplot(df_summary, aes(x = round, y = proportion_max_payoff_1, color = group_type)) +
  # group_typeを色に割り当てて、線を描画
  geom_line(linewidth = 1) +
  # プロット点を描画
  geom_point(size = 2) +
  
  # ラベルの設定
  labs(
    title = "the Proportion of Max Contributor Got Max Payoff in Max Contributor",
    x = "round",
    y = "Proportion",
    color = "グループタイプ" # 凡例のタイトル
  ) +
  
  # Y軸をパーセント表示に設定
  scale_y_continuous(
    labels = scales::percent, 
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1) 
  ) +
  # X軸の目盛りを調整（全ラウンドを表示）
  scale_x_continuous(breaks = scales::breaks_pretty(n=10)) +
  
  # 凡例をグラフの下部に配置
  theme_gray() +
  theme(legend.position = "bottom")

# 4. グラフを画像ファイルとして保存
ggsave("proportion_max_payoff_stacked_by_group.png", plot = p, width = 8, height = 5)