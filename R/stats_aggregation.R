#' @include impute_funcs.R
#' @export
stats_by_category <- function(data_results){
  rm_dupe_rows <- function(t)t[!duplicated(t),]
  no_rows <- function(t)(nrow(t) == 0)

  data_cat <- list(tibble())

  if(any(names(data_results) %in% c("QB", "RB", "WR", "TE"))){
    data_cat <- map(list(pass = "^pass_", rush = "^rush_", rec = "^rec"),
                    lapply, X = data_results, FUN = get_stat_cols) %>%
      map(bind_rows) %>% map(rm_dupe_rows) %>% map(impute_na_off)
  }

  if("K" %in% names(data_results)){
    data_cat$kick <- map(data_results, get_stat_cols,
                         match_pattern = "^fg|^xp" ) %>%
      bind_rows() %>% kick_impute()
  }

  if("DST" %in% names(data_results)){
    data_cat$dst <- map(data_results, get_stat_cols,
                         match_pattern = "^dst" ) %>%
      bind_rows()
  }

  if(any(names(data_results) %in% c("DL", "LB", "DB"))){
    data_cat$idp <- map(data_results, get_stat_cols,
                        match_pattern = "^idp" ) %>%
      bind_rows() %>% rm_dupe_rows()
  }

 return(Filter(Negate(no_rows), data_cat))
}




