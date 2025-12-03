
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe") 


rm(list = ls())
library(tidyverse)
library(gt)
library(webshot2)

setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")

path <- "015/data/merge_long.csv"
df <- read_csv(path, show_col_types = FALSE)

groups_to_keep <- c("base", "015", "03", "045")

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

# --- 3. 4つの群それぞれに対してプロットを作成し、指定のディレクトリに保存 ---

# 保存ディレクトリを設定
save_dir <- "015/analysis/"

# 保存ディレクトリが存在しない場合は作成 (必須のステップ)
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# Y軸の最大値を1.0に固定
y_limit <- 1.0

# プロットのループ処理
for (g in groups_to_keep) {
  # 現在の群のデータのみを抽出
  df_group <- df_proportions %>%
    filter(group_type == g)
  
  # ファイル名と保存パスを設定
  # ファイル名は contribute_proportion_{群名}.png とします
  file_basename <- paste0("contribute_proportion_", g, ".png")
  all_trend <- paste0(save_dir, file_basename) 
  
  # プロットを作成
  plot <- ggplot(df_group, aes(x = contribute, y = proportion)) +
    # 棒グラフ
    geom_bar(stat = "identity", fill = "steelblue") +
    # タイトルとラベル
    labs(
      title = paste0("群: ", g, " における貢献度 (contribute) の出現割合"),
      x = "貢献度 (contribute)",
      y = "出現割合 (Proportion)"
    ) +
    # Y軸の範囲を固定
    scale_y_continuous(limits = c(0, y_limit)) +
    # テーマを設定
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5), # タイトル中央揃え
      axis.text.x = element_text(angle = 0)
    )
  
  # 指定されたパス (save_dir) にPNGファイルとして保存
  ggsave(
    filename = all_trend,
    plot = plot,
    width = 8, 
    height = 6, 
    units = "in", 
    dpi = 300
  )
  
  cat(paste0("プロットを ", all_trend, " として保存しました。\n"))
}