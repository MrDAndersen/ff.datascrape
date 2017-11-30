
scrape_nfl <- function(season = NULL, week = NULL,
                       position = c("QB", "RB", "WR", "TE", "K" , "DEF", "DL", "LB", "DB")){

  nfl_stats <- dplyr::bind_rows(
    lapply(httr::content(httr::GET("http://api.fantasy.nfl.com/v1/game/stats?format=json"))$stats,
           data.frame)
  )

  position <- match.arg(position)
  
  nfl_base <- httr::build_url(httr::parse_url("http://api.fantasy.nfl.com/v1/players/stats"))

  if(is.null(week)){
    nfl_type <- "seasonProjectedStats"
  } else {
    nfl_type <- "weekProjectedStats"
  }

  nfl_qry <- list(statType = nfl_type)

  if(!is.null(season))
    nfl_qry$season <- season
  
  if(!is.null(week)){
    week <- as.character(week)
    week <- match.arg(week, choices = 1:21)
    nfl_qry$week <- week
  }
  
  if(!is.null(position))
    nfl_qry$position <- position

  nfl_qry$format <- "json"

  nfl_url <- httr::modify_url(nfl_base, query = nfl_qry)

  print(nfl_url)
  nfl_proj <- httr::content(httr::GET(nfl_url))

  nfl_data <-dplyr::bind_rows(lapply(nfl_proj$players, function(p){

    player_data <- p[which(names(p) != "stats")]
    player_stats <- p$stats
    names(player_stats) <- nfl_stats$shortName[nfl_stats$id %in% names(p$stats)]

    player_table <- data.frame(append(player_data, player_stats))

    return(player_table)

  }))

  return(nfl_data)

}

