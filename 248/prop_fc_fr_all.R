rm(list = ls())
library(tidyverse)
library(readr) 
library(dplyr)
library(stringr)
library(ggplot2) 

# データの読み込み
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path_base <- "015/cleaning/merge_base.csv"
path_015 <- "015/cleaning/merge_015.csv"
path_03 <- "015/cleaning/merge_03.csv"
path_045 <- "015/cleaning/merge_045.csv"

# データフレームをリストとして読み込む
data_list <- list(
  df_base = read_csv(path_base, show_col_types = FALSE),
  df_015 = read_csv(path_015, show_col_types = FALSE),
  df_03 = read_csv(path_03, show_col_types = FALSE),
  df_045 = read_csv(path_045, show_col_types = FALSE)
)

# 処理対象の列名を定義
contribute_cols <- paste0("round ", 1:10, "_contribute")
fullcontribute_cols <- paste0("round ", 1:10, "_fullcontribute")
freeride_cols <- paste0("round ", 1:10, "_freeride")

# --- 処理を適用する関数 ---
process_data_v2 <- function(df) {
  # ... (process_data_v2の定義は省略しませんが、変更はないためそのまま使用します) ...
  df_fc <- df %>% select(all_of(contribute_cols)) 
  df_fc[, fullcontribute_cols] <- lapply(df_fc[, contribute_cols], function(x) { as.numeric(x == 10) })
  df_fc <- df_fc %>% select(all_of(fullcontribute_cols))
  
  df_fr <- df %>% select(all_of(contribute_cols)) 
  df_fr[, freeride_cols] <- lapply(df_fr[, contribute_cols], function(x) { as.numeric(x == 0) })
  df_fr <- df_fr %>% select(all_of(freeride_cols))
  
  df_mean_fc <- df_fc %>% summarise(across(everything(), mean)) %>% pivot_longer(cols = everything(), names_to = "round", values_to = "mean_fc") %>% select(-round)
  df_mean_fr <- df_fr %>% summarise(across(everything(), mean)) %>% pivot_longer(cols = everything(), names_to = "round", values_to = "mean_fr") %>% select(-round)
  
  df_mm <- bind_cols(df_mean_fc, df_mean_fr)
  
  return(list(mean_df = df_mm, fc_df = df_fc, fr_df = df_fr))
}

# --- データ処理と結合 ---
processed_results_v2 <- lapply(data_list, process_data_v2)

df_mm_base <- processed_results_v2$df_base$mean_df
df_mm_015 <- processed_results_v2$df_015$mean_df
df_mm_03 <- processed_results_v2$df_03$mean_df
df_mm_045 <- processed_results_v2$df_045$mean_df

df_base_prep <- df_mm_base %>% mutate(round = 1:10, dataset = "T0")
df_015_prep <- df_mm_015 %>% mutate(round = 1:10, dataset = "TL")
df_03_prep <- df_mm_03 %>% mutate(round = 1:10, dataset = "TM")
df_045_prep <- df_mm_045 %>% mutate(round = 1:10, dataset = "TH")

# 全てのデータを結合
df_all_prep <- bind_rows(df_base_prep, df_015_prep, df_03_prep, df_045_prep)

# グラフ描画用のロング形式データを作成
df_all_long <- df_all_prep %>%
  pivot_longer(
    cols = c(mean_fc, mean_fr),
    names_to = "trend_type",
    values_to = "proportion"
  ) %>%
  mutate(
    trend_type = case_when(
      trend_type == "mean_fc" ~ "Full Contribute",
      trend_type == "mean_fr" ~ "Freeride"
    )
  )

# --- グラフの生成と保存（修正済み） ---

# 群のリストを定義
datasets <- c("T0", "TL", "TM", "TH")
save_dir <- "015/analysis/" # 保存先ディレクトリ（末尾に "/" があることを確認）

# フォルダが存在しない場合に作成
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

# ループ処理で各群のグラフを作成・保存
for (dset in datasets) {
  # 該当する群のデータのみを抽出
  df_plot <- df_all_long %>%
    filter(dataset == dset)
  
  # グラフの作成
  plot_combined <- ggplot(df_plot, aes(x = round, y = proportion, color = trend_type, group = trend_type)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    labs(
      title = paste("Trend of Full Contribute and Freeride for Group:", toupper(dset)),
      x = "Round",
      y = "Proportion",
      color = "Trend Type"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) + 
    scale_x_continuous(breaks = 1:10) + 
    theme_gray() + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # ★★★ 修正箇所 ★★★
  # filenameをループ変数 dset に基づいて生成し、一意のファイル名にする
  filename <- paste0(save_dir, dset, "_fc_fr_combined_trend1127.png")
  
  ggsave(
    filename = filename,  # ループごとに異なるファイル名を使用
    plot = plot_combined, 
    device = "png", 
    width = 8, 
    height = 5, 
    units = "in"
  )
  cat(paste0("✅ グラフを '", filename, "' に保存しました。\n"))
}