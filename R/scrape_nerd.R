#' @export 
scrape_fantasynerd <- function(season = NULL, week = NULL, 
                               position = c("QB", "RB", "WR", "TE", "K"), 
                               ffn_key = "test"){
  
  data_type <- ifelse(is.null(week), "draft", "weekly")
  week_no <- ifelse(is.null(week), "", as.character(week))
  json_elem <- ifelse(is.null(week), "DraftProjections", "Projections")
  
  ffn_url <- sprintf(
    "http://www.fantasyfootballnerd.com/service/%s-projections/json/%s/%s/%s",
    data_type, ffn_key, position, week_no
  )
  
  ffn_data <- httr::content(httr::GET(ffn_url))
  
  return(dplyr::bind_rows(lapply(ffn_data[[json_elem]], data.frame)))
}

