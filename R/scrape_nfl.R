#' @export
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

  nfl_proj <- httr::content(httr::GET(nfl_url))

  nfl_data <-dplyr::bind_rows(
    lapply(nfl_proj$players, function(p){

    player_data <- p[which(names(p) != "stats")]
    player_stats <- p$stats
    names(player_stats) <- nfl_stats$shortName[nfl_stats$id %in% names(p$stats)]

    player_row <- append(player_data, player_stats)

    player_table <- data.frame(t(unlist(player_row)), stringsAsFactors = FALSE)

    return(player_table)

  })
  )

  nfl_data <- nfl_data %>%
    rename(player = "name", pos = "position", team = "teamAbbr", nfl_id = "id", games = "GP")


  if(position %in% c("QB", "RB", "WR", "TE")){
    names(nfl_data) <- names(nfl_data) %>%
      gsub("TD$", "TDs", .) %>%
      gsub("Fum", "fumbles", .) %>%
      gsub("X2PT", "two pts", .) %>%
      offensive_columns()
  }

  if(position == "K"){
    names(nfl_data) <- names(nfl_data) %>%
      gsub("([^0-9])([0-9])([^0-9])", "\\10\\2\\3", . ) %>%
      gsub("([0-9]+)\\.([0-9]+)", "\\1\\2", .) %>%
      gsub("miss", "", ignore.case = TRUE, .) %>%
      gsub("PAT.Made", "xp", ignore.case = TRUE,.)
  }

  if(position == "DEF"){
    def_col <- c(dst_sack = "Sack", dst_int = "Int", dst_fum_rec = "Fum.Rec",
                 dst_safety = "Saf", dst_td = "TD", dst_blk = "Block",
                 dst_ret_td = "Return.TD", dst_pts_allow ="Pts.Allow" )
    nfl_data <- nfl_data %>% rename(!!!def_col)
  }

  if(position %in% c("DL", "LB", "DB")){
    idp_col <- c(idp_solo = "Tack", idp_asst = "Ast", idp_sack = "Sack",
                 idp_fum_rec = "Fum.Rec", idp_fum_force = "Frc.Fum",
                 idp_blk = "Blk", idp_pd = "Pass.Def", idp_int ="Int")
    nfl_data <- nfl_data %>% rename(!!!idp_col)
  }
  nfl_data <- janitor::clean_names(nfl_data) %>%
    clean_format() %>%  type_convert()

  structure(nfl_data, source = "NFL", season = season, week = week, position = position)
}

