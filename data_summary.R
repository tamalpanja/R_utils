# Function to get summary of a R data.frame

library(dplyr)
import::from(fBasics, basicStats)

data_summary <- function(data){
  df_summ <- data.frame(
    variable = colnames(data),
    var_type = sapply(data, typeof),
    n_obs = nrow(data),
    n_miss = colSums(is.na(data)),
    pct_miss = round(colSums(is.na(data))/nrow(data)*100, 2),
    n_unique = sapply(data, n_distinct, na.rm = T),
    row.names = NULL
  )
  
  num_vars <- names(which(sapply(data, is.numeric)))
  null_vars <- as.character(df_summ$variable[which(df_summ$n_obs == df_summ$n_miss)])
  non_null_num_vars <- setdiff(num_vars, null_vars)
  df_num_summ <- data.frame(t(basicStats(data[non_null_num_vars])[c("Minimum", "1. Quartile", "Median", "Mean", "3. Quartile", "Maximum", "Stdev"),]), check.names = F)
  df_num_summ$variable <- rownames(df_num_summ)
  rownames(df_num_summ) <- NULL
  
  df_summ$varnum <- 1:nrow(df_summ)
  
  df_summ_complete <- merge(df_summ, df_num_summ, by = "variable", all.x = T)
  df_summ_complete <- df_summ_complete %>% arrange(varnum) %>% select(-varnum)
  
  return(df_summ_complete)
}
