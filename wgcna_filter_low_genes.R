# Takes in dataframe with first column list of gene names, and every other column
# individual samples

wgcna_filter_low_genes <- function(df, min_expr = 10, percent_cutoff = .9) {
  if (is.character(df[, 1]) == FALSE) {
    stop("Make sure first column in dataframe is a list of gene names")
    }
  else {
    library(tidyverse)
    rownames(df) <- df[, 1]
    df2 <- df[, -1]
    df2$equal_or_less_10 <- rowSums(df2 <= min_expr)
    cutoff <- (1-percent_cutoff)*(ncol(df2)-1)
    df_trim <- df2 %>% dplyr::filter(equal_or_less_10 <= cutoff)
    number_of_cut_genes <- nrow(df2) - nrow(df_trim)
    df_trim <- df_trim %>% dplyr::select(-equal_or_less_10)
    df_trim_t <- as.data.frame(t(df_trim))
    message("ATTENTION: ", number_of_cut_genes, 
            " genes were filtered out for low expression.")
    return (df_trim_t)
  }
}