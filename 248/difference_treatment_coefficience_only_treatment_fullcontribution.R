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
library(car)
Sys.setenv(CHROMOTE_CHROME = "C:/Users/FMV/AppData/Local/Google/Chrome/Application/chrome.exe")
setwd("././././.")
path <- "015/data/bunsekiyou_dummy.csv" 
df <- read_csv(path, show_col_types = FALSE) 


model_plm_dumr <- plm(
  formula = contribute_full~
    TL + TM + TH+
    age + gender + affiliation,
  data = df,
  effect = "time",
  model = "random", 
  index = c("participant.label", "round")
)
help(linearHypothesis)
a <- linearHypothesis(model = model_plm_dumr,
                      hypothesis="TL=0")
print(a)
aa <- linearHypothesis(model = model_plm_dumr,
                       hypothesis="TM=0")
aaa <- linearHypothesis(model = model_plm_dumr,
                        hypothesis="TH=0")
b <- linearHypothesis(model=model_plm_dumr,
                      hypothesis="TL-TM=0")
bb <- linearHypothesis(model=model_plm_dumr,
                       hypothesis="TL-TH=0")
c <- linearHypothesis(model=model_plm_dumr,
                      hypothesis="TM-TH=0")
group_names <- c("T0", "TL", "TM", "TH")
n_groups <- length(group_names)

result_table <- data.frame(matrix(NA, nrow = n_groups, ncol = n_groups))
colnames(result_table) <- group_names
rownames(result_table) <- group_names

extract_test_result <- function(lh_result) {
  last_row <- lh_result[nrow(lh_result), ]
  
  chisq_stat <- tryCatch(last_row$Chisq, error = function(e) NA)
  p_value <- tryCatch(last_row$`Pr(>Chisq)`, error = function(e) NA) 
  
  if (is.numeric(chisq_stat) && length(chisq_stat) == 1 && !is.na(chisq_stat) &&
      is.numeric(p_value) && length(p_value) == 1 && !is.na(p_value)) {
    return(sprintf("%.3f (p=%.3f)", as.numeric(chisq_stat), as.numeric(p_value)))
  } else {
    return("N/A: Error")
  }
}

group_names <- c("T0", "TL", "TM", "TH")
n_groups <- length(group_names)
result_table <- data.frame(matrix(NA, nrow = n_groups, ncol = n_groups))
colnames(result_table) <- group_names
rownames(result_table) <- group_names

res_a <- extract_test_result(a)
res_aa <- extract_test_result(aa)
res_aaa <- extract_test_result(aaa)
res_b <- extract_test_result(b)
res_bb <- extract_test_result(bb)
res_c <- extract_test_result(c)

result_table["TL", "T0"] <- res_a
result_table["TM", "T0"] <- res_aa
result_table["TH", "T0"] <- res_aaa
result_table["TM", "TL"] <- res_b
result_table["TH", "TL"] <- res_bb
result_table["TH", "TM"] <- res_c

table_matrix <- as.matrix(result_table)
n <- nrow(table_matrix)

for (i in 1:n) {
  for (j in i:n) {
    table_matrix[i, j] <- ""
  }
}

final_result_table <- as.data.frame(table_matrix, stringsAsFactors = FALSE)

library(knitr)

html_output <- kable(
  final_result_table, 
  format = "latex", 
  caption = "the difference between treatment effect in prrobit \\c_i=10(Chisq-stat (p-value))"
)


print(html_output)

save_dir <- "015/analysis/"
file_path <- paste0(save_dir, "hypothesis_test_summary_random_only_treatment_fullcontribution.tex")
writeLines(html_output, file_path)