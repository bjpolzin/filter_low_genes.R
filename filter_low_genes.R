## Assumes you have both "dplyr" and "tidyverse" installed and loaded ##
filter_low_genes <- function(df, min_expr = 10, percent_cutoff = 90,
                             metric = "units") {
  if (is.character(rownames(df)) == F) {
    stop("Make sure rownames in dataframe are sample IDs.")
  }
  else {
    samples_num <- nrow(df)
    dec_cutoff <- percent_cutoff/100
    df_t_orig <- df %>% t() %>% as.data.frame()
    df_t <- df_t_orig
    df_t$samples_w_min_expr <- rowSums(df_t >= min_expr)
    cutoff <- dec_cutoff * ncol(df_t_orig)
    rounded_cutoff <- ceiling(cutoff)
    df_t_filt <- df_t %>% dplyr::filter(samples_w_min_expr >= rounded_cutoff)
    number_of_rm_genes <- nrow(df_t) - nrow(df_t_filt)
    df_t_filt_final <- df_t_filt %>% dplyr::select(-samples_w_min_expr)
    df_filt <- df_t_filt_final %>% t() %>% as.data.frame()
    all_gene_cols <- colnames(df_filt)
    df_filt_final <- df_filt %>% dplyr::mutate(across(all_of(all_gene_cols), as.numeric))
    message("ATTENTION: A total of ", 
            number_of_rm_genes, 
            " genes were removed. This was based on your parameters that stated a minimum of ",
            min_expr,
            " ",
            metric,
            " must be in at least ",
            percent_cutoff,
            "% (≥",
            rounded_cutoff,
            ") of the ",
            samples_num,
            " samples provided.")
    return(df_filt_final)
  }
}
