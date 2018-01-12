#' Scrape data from FantasyFootballNerd
#' 
#' Use this function to srape fantasy football projections from FantasyFootballNerd.
#' The function uses the FantasyFootball Nerd API. If you don't have an API key
#' to use for this you will not get accuracte results. You can register for an
#' API key at fantasyfootballnerd.com. 
#' You set the API key for this scrape via \code{options('ffdata.ffn_api' = 'apikey')}
#' @param week The week that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#' @export
scrape_fantasynerd <- function(week = NULL,
                               position = c("QB", "RB", "WR", "TE", "K", "DST")){

  if(is.null(week))
    week <- 0
  
  position <- match.arg(position)
  
  if(week != 0 & !(week %in% 1:17)){
    stop("When specifying a week please only use numbers between 1 and 17", call. = FALSE)
  }
  
  data_type <- ifelse(week = 0, "draft", "weekly")
  week_no <- ifelse(week == 0, "", as.character(week))
  json_elem <- ifelse(week = 0, "DraftProjections", "Projections")

  ffn_key <- getOption("ffdata.ffn_api")

  if(ffn_key == "test"){
    warning("Using 'test' for FantasyFootball Nerd API will not provide accurate data.\n",
            "Set the api key with options('ffdata.ffn_api' = 'apikey').", call. = FALSE)
  }
  ffn_url <- sprintf(
    "http://www.fantasyfootballnerd.com/service/%s-projections/json/%s/%s/%s",
    data_type, ffn_key, ifelse(position == "DST", "DEF", position), week_no
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

  if(any(names(ffn_table) == "fantasynerd_id"))
    ffn_table <- ffn_table %>% add_column(id = id_col(ffn_table$fantasynerd_id, "fantasynerd_id"), .before = 1)
  
  structure(ffn_table, source = "FantasyFootballNerd", season = current_season(), 
            week = week, position = position)
}

