rm(list = ls())
library(tidyverse)
library(readr)


setwd("././././.")

path <- "015/data/merge_long.csv"
df <- read_csv(path, show_col_types = FALSE)

save_dir <- "015/analysis/"

# ディレクトリが存在しない場合は作成
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
  message(paste("Created directory:", save_dir))
}

# 4. グループごとに処理を実行し、散布図を作成・保存
group_names <- unique(df$group_type)

# ループ処理で各グループの散布図を作成し、保存
for (g_name in group_names) {
  
  # 4-1. 特定のグループのデータを抽出、alpha_medとbeta_medで集計
  df_plot_group <- df %>%
    filter(group_type == g_name) %>%
    group_by(alpha_med, beta_med) %>%
    summarise(
      count = n(),
      .groups = 'drop'
    )
  
  # 4-2. 散布図の作成
  scatter_plot <- df_plot_group %>%
    ggplot(aes(x = alpha_med, y = beta_med)) +
    geom_point(aes(size = count), color = "#0072B2", alpha = 0.8) +
    scale_x_continuous(breaks = seq(-10,10,1)) +
    scale_y_continuous(breaks = scales::breaks_width(1)) +
    guides(size = "none") +
    coord_cartesian(xlim = c(-10, 10), ylim = c(-7, 2)) +
    labs(
      x = expression(alpha[med]),
      y = expression(beta[med]),
      title = paste0("alpha/beta Distribution (", g_name, ")")
    ) +
    scale_size_continuous(range = c(2, 10)) +
    theme_gray() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray")+
    facet_wrap(~gender)
  
  # 4-3. ファイル名の定義と保存
  filename <- paste0("alpha_beta_distribution_", g_name, "_bygender.png")
  save_path <- paste0(save_dir, filename)
  
  # ggsaveでpng形式で保存
  ggsave(
    filename = save_path,
    plot = scatter_plot,
    device = "png",
    width = 8,
    height = 5,
    units = "in"
  )
  
  message(paste("Saved:", save_path))
}

# --- おまけ: 全データを結合した散布図も保存する場合 ---

# # 全データ結合の集計
 df_plot_all <- df %>%
   group_by(alpha_med, beta_med) %>%
   summarise(
     count = n(),
     .groups = 'drop'
   )

# # 全データ結合のプロット (タイトルのみ変更)
 scatter_plot_all <- df_plot_all %>%
   ggplot(aes(x = alpha_med, y = beta_med)) +
   geom_point(aes(size = count), color = "#0072B2", alpha = 0.8) +
   scale_x_continuous(breaks = seq(-10,10,1)) +
   scale_y_continuous(breaks = scales::breaks_width(1)) +
   guides(size = "none") +
　  coord_cartesian(xlim = c(-10, 10), ylim = c(-7, 2)) +
   labs(
     x = expression(alpha[med]),
     y = expression(beta[med]),
     title = "alpha/beta Distribution (All)" # タイトル変更
   ) +
   scale_size_continuous(range = c(2, 10)) +
   theme_gray() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
     axis.title.x = element_text(size = 14),
     axis.title.y = element_text(size = 14)
   ) +
   geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
   geom_vline(xintercept = 0, linetype = "dashed", color = "gray")+
   facet_wrap(~gender)

# # ファイル名と保存
 all_filename <- paste0("alpha_beta_distribution_", "all", "_bygender.png")
 all_save_path <- paste0(save_dir, all_filename)

 ggsave(
   filename = all_save_path,
   plot = scatter_plot_all,
   device = "png",
   width = 8,
   height = 5,
   units = "in"
 )
# message(paste("Saved:", all_save_path))
 