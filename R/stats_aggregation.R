#' @export
stats_by_category <- function(data_results){
  stat_categories <- list(pass = list(match_pattern = "pass_", na_impute = pass_impute),
                          rush = list(match_pattern = "rush_", na_impute = rush_impute),
                          rec = list(match_pattern = "rec", na_impute = rec_impute))

  data_cat <- lapply(stat_categories, function(sc){
    sc_tbl <- bind_rows(
      lapply(data_results, get_stat_cols, match_pattern = sc[["match_pattern"]])
    )

    impute_nas = sc[["na_impute"]]
    sc_tbl <- impute_nas(sc_tbl)
  })


}
