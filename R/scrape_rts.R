#' @export
scrape_rts <- function(position = c("QB", "RB", "WR", "TE", "K", "DST")){
  
  p <- c(QB = 0, RB = 1, WR = 2, TE = 3, K = 4, DST = 5)
  rts_url <-paste0("https://www.freedraftguide.com/football/draft-guide-rankings-provider.php?POS=", p[position])
  
  rts_list <- httr::content(httr::GET(rts_url))
  
  
  
  rts_data  <- dplyr::bind_rows(lapply(rts_list$player_list, 
                                       function(pl)dplyr::bind_cols(
                                         data.frame(pl[c("stats_id", "name", "position", "nfl_team")]),
                                         data.frame(pl$stats))))
  
  rts_data$position <- names(grep(rts_data$position,p, value = TRUE))
  return(rts_data)
}


