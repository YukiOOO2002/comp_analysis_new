rm(list = ls())
library(tidyverse)
library(gt)
library(webshot2)
library(rstatix)
library(tidyr) 

# --- ファイルパスの設定 ---

Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("././././.")
path <- "015/data/merge_long.csv"
# ---------------------------

df <- read_csv(path, show_col_types = FALSE)

# 処置群の定義

groups <- c("base", "015", "03", "045")



# 連続変数 (contribute) を使用したペアワイズWilcoxon順位和検定の実行

wilcox_results <- df %>%
  
  select(group_type, contribute) %>%
  
  mutate(group_type = factor(group_type, levels = groups)) %>%
  
  wilcox_test(contribute ~ group_type, 
              
              paired = FALSE, 
              
              p.adjust.method = "bonferroni") %>% 
  
  
  group_by(group1, group2) %>%
  
  mutate(
    
    median1 = median(df[df$group_type == group1, ]$contribute, na.rm = TRUE),
    
    median2 = median(df[df$group_type == group2, ]$contribute, na.rm = TRUE),
    
    Z_value = qnorm(p/2, lower.tail = FALSE) * sign(median1 - median2)
    
  ) %>%
  
  ungroup() %>%
  
  select(group1, group2, Z_value, p) %>%
  
  rename(P_value = p)


wilcox_results_reverse <- wilcox_results %>%
  
  # group1 と group2 の値を入れ替える
  
  mutate(group_temp = group1, group1 = group2, group2 = group_temp) %>%
  
  select(-group_temp) %>% 
  
  # Z値の符号を反転させる (Z(B vs A) = -Z(A vs B))
  
  mutate(Z_value = Z_value * -1) %>%
  
  select(group1, group2, Z_value, P_value)


full_results <- bind_rows(wilcox_results, wilcox_results_reverse)


# Z値のフル総当たり表を生成

Z_matrix <- full_results %>%
  
  select(group1, group2, Z_value) %>%
  
  pivot_wider(names_from = group2, values_from = Z_value) %>%
  
  column_to_rownames(var = "group1") %>%
  
  as.matrix()

Z_matrix <- Z_matrix[groups, groups] 



# P値のフル総当たり表を生成

P_matrix <- full_results %>%
  
  select(group1, group2, P_value) %>%
  
  pivot_wider(names_from = group2, values_from = P_value) %>%
  
  column_to_rownames(var = "group1") %>%
  
  as.matrix()

P_matrix <- P_matrix[groups, groups] 

create_gt_table <- function(matrix, fmt_type) { # 引数から title_part を削除
  
  # 上半三角と対角線にNAを挿入
  
  lower_matrix <- matrix
  
  for (i in 1:nrow(lower_matrix)) {
    
    for (j in 1:ncol(lower_matrix)) {
      
      if (i <= j) { 
        
        lower_matrix[i, j] <- NA
        
      }
      
    }
    
  }
  
  df_output <- as.data.frame(lower_matrix) %>%
    
    rownames_to_column(var = "Group")
  
  gt_table <- gt(df_output) %>%
    
    tab_header(
      
      title = md(""), # タイトルを空に設定
      
      subtitle = md("*Wilcoxon Rank-Sum Test (Variable: contribute)*")
      
    ) %>%
    
    cols_label(.list = setNames(as.list(groups), groups)) %>%
    
    sub_missing(
      
      columns = everything(),
      
      missing_text = ""
      
    ) %>% 
    
    data_color(
      columns = all_of(groups),
      domain = range(matrix, na.rm = TRUE),
      palette = c("white"), # 背景色を白に統一
      na_color = "transparent"
    )

  if (fmt_type == "P" || fmt_type == "Z") {
    gt_table <- gt_table %>% fmt_number(
      columns = all_of(groups),
      decimals = 3
    ) 
  }
  
  gt_table <- gt_table %>% 
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

gt_p <- create_gt_table(P_matrix, "P") 

save_dir <- "015/analysis/"
all <- paste0(save_dir, "wilcoxon_p.png")

gtsave(
  data = gt_p,
  filename = all 
)
print(paste0("P値の表を '", all, "' として保存しました。"))

# Z値の表を作成・保存 (引数からタイトル部分を削除)
gt_z <- create_gt_table(Z_matrix, "Z")
all2 <- paste0(save_dir, "wilcoxon_z.png")
gtsave(
  data = gt_z,
  filename = all2 
)
print(paste0("Z値の表を '", all2, "' として保存しました。"))
wilcox_bonferroni_result <- pairwise.wilcox.test(
  x = df$contribute,
  g = df$group_type,
  paired = FALSE,
  p.adjust.method = "bonferroni"
)
print(wilcox_bonferroni_result)
p_matrix <- wilcox_bonferroni_result$p.value

# 2. P値の行列を小数点以下3桁に丸める
rounded_p_matrix <- round(p_matrix, digits = 3)
# 3. 結果を表示
print(rounded_p_matrix)

