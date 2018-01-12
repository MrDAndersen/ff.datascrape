#' @export
scrape_fantasynerd <- function(season = NULL, week = NULL,
                               position = c("QB", "RB", "WR", "TE", "K", "DEF")){

  data_type <- ifelse(is.null(week), "draft", "weekly")
  week_no <- ifelse(is.null(week), "", as.character(week))
  json_elem <- ifelse(is.null(week), "DraftProjections", "Projections")

  ffn_key <- getOption("ffdata.ffn_api")

  if(ffn_key == "test"){
    warning("Using 'test' for FantasyFootball Nerd API will not provide accurate data.\n",
            "Set the api key with options('ffdata.ffn_api' = 'apikey').", call. = FALSE)
  }
  ffn_url <- sprintf(
    "http://www.fantasyfootballnerd.com/service/%s-projections/json/%s/%s/%s",
    data_type, ffn_key, position, week_no
  )

  ffn_data <- httr::content(httr::GET(ffn_url))
  ffn_table <- dplyr::bind_rows(lapply(ffn_data[[json_elem]], data.frame, stringsAsFactors = FALSE))

  nerd_cols <- c(pass_att = "passAtt", pass_comp = "passCmp", pass_yds = "passYds",
                 pass_tds = "passTD", pass_int = "passInt", rush_att = "rushAtt",
                 rush_yds = "rushYds", rush_tds = "rushTD", fumbles_lost = "fumblesLost",
                 rec = "receptions", rec_yds = "recYds", rec_tds = "recTD", fg_att = "fgAtt",
                 dst_int = "defInt", dst_fum_rec = "defFR", dst_fum_force = "defFF",
                 dst_sack = "defSack" , dst_td = "defTD", dst_ret_td = "defRetTD",
                 dst_safety = "defSafety", dst_pts_allow = "defPA",
                 dst_yds_allow = "defYdsAllowed", player = "displayName",
                 fantasynerd_id = "playerId", pos = "position")

  ffn_table <- ffn_table %>% rename(!!!nerd_cols)

  has_num <- function(x)!all(x == "0.0")
  ffn_table <- ffn_table %>% select_if(has_num)

  ffn_table <- janitor::clean_names(ffn_table) %>%
    clean_format() %>%  type_convert()

  structure(ffn_table, source = "FantasyFootballNerd", season = season, week = week,
            position = position)
}

