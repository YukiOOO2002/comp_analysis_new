rm(list = ls())
library(tidyverse)
library(readr) 
library(dplyr)
library(stringr)

#データ読み込み（メイン＋質問コントロール＋）
setwd("C:/Users/FMV/OneDrive - OUMail (Osaka University)/デスクトップ")
path_015 <- "015/cleaning/015_merge.csv"
df_015 <- read_csv(path_015, show_col_types = FALSE)

path_03 <- "015/cleaning/03_merge.csv"
df_03 <- read_csv(path_03, show_col_types = FALSE)

path_base <- "015/cleaning/base_merge.csv"
df_base <- read_csv(path_base, show_col_types = FALSE)

# 必要なパッケージを読み込む
# (もしインストールされていない場合は install.packages("tidyverse") を実行してください)
library(tidyverse)

process_df <- function(df, group_name) {
  df %>%
    # contribution 列 ('round 1_contribute'から'round 10_contribute') のみを選択
    select(starts_with("round") & ends_with("_contribute")) %>%
    
    # ワイド形式からロング形式へ変換
    pivot_longer(
      cols = everything(), # 選択した全ての列を対象
      names_to = "Round_Variable", # 元の列名を格納する新しい列名
      values_to = "Contribution" # セル値を格納する新しい列名
    ) %>%
    
    # Round_Variable (例: "round 1_contribute") からラウンド番号 (1, 2, ...) を抽出
    mutate(
      Round = as.integer(gsub("round (\\d+)_contribute", "\\1", Round_Variable)),
      Group = group_name
    ) %>%
    
    # 必要な列のみを選択
    select(Group, Round, Contribution)
}

# 各データフレームに関数を適用 (group_nameは分かりやすい名前のまま)
df_03_long <- process_df(df_03, "Group 03")
df_015_long <- process_df(df_015, "Group 015")
df_base_long <- process_df(df_base, "Group Base")

# 3つのデータフレームを結合
combined_df <- bind_rows(df_03_long, df_015_long, df_base_long)

# ★★★ 凡例項目名から「Group 」を削除する修正 ★★★
combined_df <- combined_df %>%
  mutate(
    # Group列の値（例: "Group 03"）から "Group " の文字列を削除する
    Group = str_remove(Group, "Group ") 
  )
# ----------------------------------------------------------------------
# 2. ラウンドごと、群ごとの平均 Contribution を計算
# ----------------------------------------------------------------------
summary_df <- combined_df %>%
  group_by(Group, Round) %>%
  summarise(
    Mean_Contribution = mean(Contribution, na.rm = TRUE), # 平均を計算 (欠損値は無視)
    .groups = 'drop'
  )

# ----------------------------------------------------------------------
# 3. 図表の作成 (ラウンドごとの平均 Contribution の推移)
# ----------------------------------------------------------------------

# ggplot2を用いた折れ線グラフの作成
plot_contribution_trend <- ggplot(summary_df, aes(x = Round, y = Mean_Contribution, color = Group, group = Group)) +
  
  # 折れ線を追加
  geom_line(linewidth = 1) + 
  
  # ポイントを追加
  geom_point(size = 3) + 
  
  # X軸のスケールをラウンド番号に合わせて整数で表示 (1から10)
  scale_x_continuous(breaks = 1:10) +
  
  # ★ Y軸の目盛りを整数値に設定 ★
  # breaks = scales::breaks_width(1) は目盛り間隔を1（整数）に設定します
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  
  # グラフのタイトルとラベル
  labs(
    title = "Average Contribution by Round",
    x = "Round Number",
    y = "Average Contribution",
    color = "treatment" # 凡例タイトル
  ) +
  
  # ★ 背景をグレーのデフォルトテーマ (theme_grey) に変更 ★
  # 以前のグラフの背景はこれに近いことが多いです。
  theme_grey() +
  
  # 凡例の位置を調整
  theme(legend.position = "bottom")

# グラフを表示
print(plot_contribution_trend)

# グラフをファイルに保存する場合の例 (任意)
ggsave("contribution_trend_comparison3tre.png", plot_contribution_trend, width = 8, height = 5)

# 集計結果の確認 (任意)
# print(summary_df)
