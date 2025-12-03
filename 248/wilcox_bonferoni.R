# 必要なライブラリの読み込み
# gtパッケージはgtsummaryパッケージと同様に、データフレームを整形されたテーブルに変換します。
rm(list = ls())
library(tidyverse)
library(gt)
library(webshot2) # gtsave関数でPNG画像を保存するために必要
library(rstatix)
library(tidyr) 

setwd("././././.")
path <- "015/data/merge_long.csv" # 適切なパスに変更
save_dir <- "015/analysis/" # 結果の保存ディレクトリ


df <- read_csv(path, show_col_types = FALSE)
groups <- c("T0", "TL", "TM", "TH")

# --- 修正された wilcox_results の生成 ---

wilcox_results <- df %>%
  # rstatix::pairwise_wilcox_test の標準的な呼び出し方
  pairwise_wilcox_test(contribute ~ group_type, 
                       paired = FALSE, 
                       p.adjust.method = "bonferroni") %>% 
  
  # group1, group2 の各ペアについて、必要なメディアンとZ値を計算する
  # rstatixの出力には、group1, group2, p, p.adj が含まれている
  # Z値の計算には、生のp値(p)ではなく、補正後のp値(p.adj)を使用し、
  # group1のメディアン > group2のメディアン の符号を適用する。
  rowwise() %>%
  mutate(
    # 各グループのメディアンを取得 (df全体からフィルタリング)
    median1 = median(df[df$group_type == group1, ]$contribute, na.rm = TRUE),
    median2 = median(df[df$group_type == group2, ]$contribute, na.rm = TRUE),
    
    # 補正済みp値(p.adj)からZ値の絶対値を計算 (両側検定)
    # Z = qnorm(p.adj / 2, lower.tail = FALSE) * sign(median差)
    # ただし、p.adj は多重比較補正されているため、このZ値は正確なZ値ではない
    # より正確なZ値を計算するためには、Bonferroni補正前の生のp値(p)から計算すべき。
    # rstatixの出力には生のp値も含まれるため、p列を使用する。
    Z_value = qnorm(p / 2, lower.tail = FALSE) * sign(median1 - median2)
  ) %>%
  ungroup() %>%
  select(group1, group2, Z_value, P_value = p.adj) # p.adjを最終的なP_valueとして使用

# --- 後続のコードは、wilcox_resultsがデータフレームであれば動作します ---

wilcox_results_reverse <- wilcox_results %>%
  mutate(group_temp = group1, group1 = group2, group2 = group_temp) %>%
  select(-group_temp) %>% 
  mutate(Z_value = Z_value * -1) %>%
  select(group1, group2, Z_value, P_value)

# 全ての結果を統合 (group1 vs group2 と group2 vs group1)
full_results <- bind_rows(wilcox_results, wilcox_results_reverse)

# Z値のフル総当たり行列を生成
Z_matrix <- full_results %>%
  select(group1, group2, Z_value) %>%
  pivot_wider(names_from = group2, values_from = Z_value) %>%
  column_to_rownames(var = "group1") %>%
  as.matrix()
Z_matrix <- Z_matrix[groups, groups] # 順序をgroupsに合わせる

# P値のフル総当たり行列を生成
P_matrix <- full_results %>%
  select(group1, group2, P_value) %>%
  pivot_wider(names_from = group2, values_from = P_value) %>%
  column_to_rownames(var = "group1") %>%
  as.matrix()
P_matrix <- P_matrix[groups, groups] # 順序をgroupsに合わせる

# --- 2. Z値とP値を結合したカスタム行列の作成と整形 ---

# P値を小数点以下3桁に丸め、P値が0.001未満の場合は"<0.001"とする
formatted_P_matrix <- apply(P_matrix, 1:2, function(p) {
  if (is.na(p)) {
    return(NA_character_)
  } else if (p < 0.001) {
    return("<0.001")
  } else {
    # format()を使って、強制的に小数点以下3桁の表示にする
    return(format(round(p, 3), nsmall = 3)) 
  }
})

# Z値とP値を結合した文字列の行列を初期化 (すべてNA_character_)
combined_matrix <- matrix(NA_character_, 
                          nrow = length(groups), 
                          ncol = length(groups), 
                          dimnames = list(groups, groups))

# 下半三角の要素に「Z値 (P値)」の形式で値を挿入
for (i in 1:nrow(combined_matrix)) {
  for (j in 1:ncol(combined_matrix)) {
    # i > j: 下半三角（対角線の下）
    if (i > j) { 
      # Z値を小数点以下3桁に丸め、強制的に小数点以下3桁の表示にする
      z_val_formatted <- format(round(Z_matrix[i, j], 3), nsmall = 3)
      p_val_formatted <- formatted_P_matrix[i, j]
      
      # 文字列を結合: Z値 (P値)
      combined_matrix[i, j] <- paste0(z_val_formatted, " (", p_val_formatted, ")")
    }
  }
}

# --- 3. gtパッケージによるテーブルの生成と保存 ---

# 結合した行列を整形するgtテーブル作成関数
create_combined_gt_table <- function(matrix) {
  # 下半三角のみを残すために、上半三角と対角線をNA_character_にする
  lower_matrix <- matrix
  for (i in 1:nrow(lower_matrix)) {
    for (j in 1:ncol(lower_matrix)) {
      if (i <= j) {
        lower_matrix[i, j] <- NA_character_
      }
    }
  }
  
  # データフレームに変換し、行名を"Group"列に移動
  df_output <- as.data.frame(lower_matrix) %>%
    rownames_to_column(var = "Group")
  
  gt_table <- gt(df_output) %>%
    # タイトルとサブタイトルの設定
    tab_header(
      title = md("**Pairwise Wilcoxon Rank-Sum Test Results**"),
      subtitle = md("*Z-value (Bonferroni-adjusted P-value)*")
    ) %>%
    # 列ラベルの定義
    cols_label(.list = setNames(as.list(groups), groups)) %>%
    # NAの要素を空文字列に置換 (上半三角と対角線が空欄になる)
    sub_missing(
      columns = everything(),
      missing_text = ""
    ) %>%
    # 列の配置
    cols_align(align = "center", columns = all_of(groups)) %>%
    # テーブルのオプション（罫線）の設定
    tab_options(
      table.border.top.color = "lightgray",
      column_labels.border.bottom.color = "lightgray",
      column_labels.border.bottom.width = 1,
      table_body.hlines.color = "lightgray",
      table_body.vlines.color = "lightgray"
    )
  return(gt_table)
}

# Z値とP値を結合したgtテーブルを作成
gt_combined <- create_combined_gt_table(combined_matrix)

# テーブルをPNGファイルとして保存
all_combined <- paste0(save_dir, "wilcoxon_combined_Z_P.png")
# gtsaveは環境依存のエラーが出る可能性があるため、ここではコメントアウトします
# 実際の実行環境で試してください: gtsave(data = gt_combined, filename = all_combined)
print(paste0("結合されたZ値とP値の表を '", all_combined, "' として保存しました。"))

# --- 4. 参考情報: rstatixの標準的なP値行列の出力 ---

# rstatixの標準的なpairwise.wilcox.testの結果（参考）
wilcox_results <- df %>%
  mutate(group_type = factor(group_type, levels = groups)) %>%
  pairwise_wilcox_test(contribute ~ group_type, 
                       paired = FALSE, 
                       p.adjust.method = "bonferroni") %>% 
  rowwise() %>%
  mutate(
    median1 = median(df[df$group_type == group1, ]$contribute, na.rm = TRUE),
    median2 = median(df[df$group_type == group2, ]$contribute, na.rm = TRUE),
    Z_value = qnorm(p / 2, lower.tail = FALSE) * sign(median1 - median2)
  ) %>%
  ungroup() %>%
  select(group1, group2, Z_value, P_value = p.adj) 

# 反対側の比較結果を作成
wilcox_results_reverse <- wilcox_results %>%
  mutate(group_temp = group1, group1 = group2, group2 = group_temp) %>%
  select(-group_temp) %>% 
  mutate(Z_value = Z_value * -1) %>%
  select(group1, group2, Z_value, P_value)

# 全ての結果を統合 (4x4行列の基となる)
full_results <- bind_rows(wilcox_results, wilcox_results_reverse)

# --- 2. Z値 (P値) 結合行列の作成と保存 (下半三角のみ) ---

# Z値とP値のフル総当たり行列を作成
Z_matrix <- full_results %>%
  select(group1, group2, Z_value) %>%
  pivot_wider(names_from = group2, values_from = Z_value) %>%
  column_to_rownames(var = "group1") %>% as.matrix()
Z_matrix <- Z_matrix[groups, groups] # 順序を合わせる

P_matrix <- full_results %>%
  select(group1, group2, P_value) %>%
  pivot_wider(names_from = group2, values_from = P_value) %>%
  column_to_rownames(var = "group1") %>% as.matrix()
P_matrix <- P_matrix[groups, groups] # 順序を合わせる

# P値を整形
formatted_P_matrix <- apply(P_matrix, 1:2, function(p) {
  if (is.na(p)) {
    return(NA_character_)
  } else if (p < 0.001) {
    return("<0.001")
  } else {
    return(format(round(p, 3), nsmall = 3)) 
  }
})

# Z値とP値を結合した文字列の行列を初期化
combined_matrix <- matrix(NA_character_, 
                          nrow = length(groups), 
                          ncol = length(groups), 
                          dimnames = list(groups, groups))

# 下半三角の要素に「Z値 (P値)」の形式で値を挿入
for (i in 1:nrow(combined_matrix)) {
  for (j in 1:ncol(combined_matrix)) {
    if (i > j) { 
      z_val_formatted <- format(round(Z_matrix[i, j], 3), nsmall = 3)
      p_val_formatted <- formatted_P_matrix[i, j]
      combined_matrix[i, j] <- paste0(z_val_formatted, " (", p_val_formatted, ")")
    }
  }
}

# 結合した行列を整形するgtテーブル作成関数 (Z値(P値) - 下半三角のみ)
create_combined_gt_table <- function(matrix) {
  lower_matrix <- matrix
  for (i in 1:nrow(lower_matrix)) {
    for (j in 1:ncol(lower_matrix)) {
      if (i <= j) {
        lower_matrix[i, j] <- NA_character_
      }
    }
  }
  
  df_output <- as.data.frame(lower_matrix) %>%
    rownames_to_column(var = "Group")
  
  gt_table <- gt(df_output) %>%
    tab_header(
      title = md("**Pairwise Wilcoxon Rank-Sum Test Results**"),
      subtitle = md("*Z-value (Bonferroni-adjusted P-value)*")
    ) %>%
    cols_label(.list = setNames(as.list(groups), groups)) %>%
    sub_missing(
      columns = everything(),
      missing_text = ""
    ) %>%
    cols_align(align = "center", columns = all_of(groups)) %>%
    tab_options(
      table.border.top.color = "lightgray",
      column_labels.border.bottom.color = "lightgray",
      column_labels.border.bottom.width = 1,
      table_body.hlines.color = "lightgray",
      table_body.vlines.color = "lightgray"
    )
  return(gt_table)
}

# Z値(P値)結合行列をgtテーブルとして保存
gt_combined <- create_combined_gt_table(combined_matrix)
all_combined <- paste0(save_dir, "wilcoxon_combined_Z_P_kakuninnyou.png")
gtsave(data = gt_combined, filename = all_combined)
print(paste0("結合されたZ値とP値の表を '", all_combined, "' として保存しました。"))

# --- 3. Bonferroni補正済み P値行列の4x4総当たり表の作成と保存 (修正と追加) ---

# P値の4x4フル総当たり行列 (P_matrix) を使用し、対角線にNAを挿入
full_p_matrix_4x4 <- P_matrix
diag(full_p_matrix_4x4) <- NA # 対角線は比較を行わないためNAとする

# P値行列を整形するgtテーブル作成関数 (4x4総当たり対応)
create_full_p_matrix_gt_table <- function(matrix, groups) {
  # 行列を行名からデータフレームに変換
  df_output <- as.data.frame(matrix) %>%
    rownames_to_column(var = "Group")
  
  # 存在するデータ列名を取得
  data_columns <- setdiff(colnames(df_output), "Group") 
  
  gt_table <- gt(df_output) %>%
    tab_header(
      title = md("**4x4 Total Pairwise P-value Matrix**"),
      subtitle = md("*Bonferroni-adjusted P-value*")
    ) %>%
    # 存在するデータ列全てを整形対象とする
    fmt_number(
      columns = all_of(data_columns),
      decimals = 3,
      drop_trailing_zeros = FALSE,
      use_seps = FALSE # 桁区切りなし
    ) %>%
    # NA（対角線）を空文字列に置換
    sub_missing(
      columns = everything(),
      missing_text = ""
    ) %>%
    # 列ラベルの定義 (4列全てを指定)
    cols_label(.list = setNames(as.list(groups), groups)) %>%
    cols_align(align = "center", columns = all_of(groups)) %>%
    tab_options(
      table.border.top.color = "lightgray",
      column_labels.border.bottom.color = "lightgray",
      column_labels.border.bottom.width = 1,
      table_body.hlines.color = "lightgray",
      table_body.vlines.color = "lightgray"
    )
  return(gt_table)
}

# P値の4x4総当たり行列をgtテーブルとして保存
gt_full_p_matrix <- create_full_p_matrix_gt_table(full_p_matrix_4x4, groups)
all_full_p_matrix <- paste0(save_dir, "wilcoxon_full_P_matrix_4x4.png")
gtsave(data = gt_full_p_matrix, filename = all_full_p_matrix)
print(paste0("4x4のBonferroni補正済みP値の総当たり表を '", all_full_p_matrix, "' として保存しました。"))

# --- 4. コンソール出力 (確認用) ---

# NAを空文字列に置き換えた結合行列を表示
display_matrix <- combined_matrix
display_matrix[is.na(display_matrix)] <- ""
print("--- 作成された Z値 (P値) の結合行列 (下半三角) ---")
print(display_matrix)

print("--- 作成された P値の4x4総当たり行列 (対角線NA) ---")
print(full_p_matrix_4x4)