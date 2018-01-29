#' Scrape data from ESPN
#'
#' Use this function to srape fantasy football projections from ESPN
#' @param season The year that data will be scraped for
#' @param week The week that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#' @import tidyverse httr rvest
#' @export
scrape_espn <- function(season, week = NULL, position = c("QB", "RB", "WR", "TE", "DST", "K")){
  espn_positions <- c("QB" = 0, "RB" = 2, "WR" = 4, "TE" = 6, "DST" = 16, "K" = 17)
  espn_base <- str_to_url("http://games.espn.com/ffl/tools/projections")

  position <- match.arg(position)

  if(season > current_season()){
    stop("Invalid season. Please specify ", current_season(), " or earlier", call. = FALSE)
  }

  if(season < current_season()){
    warning("Requesting data from before the ", current_season(),
            " may yield inaccurate data.", call. = FALSE)
  }

  espn_qry <- list(slotCategoryId = espn_positions[[position]])

  if(is.null(week) || week == 0){
    espn_qry$seasonTotals <- "true"
  } else {
    if(!(week %in% 1:17))
      stop("When specifying a week please only use numbers between 1 and 17", call. = FALSE)
    espn_qry$scoringPeriodId <- week
  }

  espn_qry$seasonId <- season

  espn_qry$startIndex <- 0

  espn_url <- modify_url(espn_base, query = espn_qry)

  espn_data <- scrape_html_data(espn_url)

  if(position != "DST"){
    espn_data <- espn_data %>%
      extract("PLAYER, TEAM POS", into = c("PLAYER", "TEAM", "POS", "STATUS"),
              "([A-Za-z .'-\\*]+),\\s([A-Za-z]+)\\s*([A-Za-z]+)\\s*([A-Za-z]*)")
  } else {
    espn_data <- espn_data %>%
      extract("PLAYER, TEAM POS", into = c("PLAYER", "POS"),
              "([A-Za-z0-9 ./'-\\*]+)\\s([A-Za-z/]+)")
  }

  if(any(names(espn_data) == "PASSING C/A")){
    espn_data <- espn_data %>%
      extract(col = "PASSING C/A", c("PASSING COMP", "PASSING ATT"),
              "([0-9]+\\.*[0-9]*)/([0-9]+\\.*[0-9]*)")
  }

  switch(position,
         "K" = {
           names(espn_data) <- gsub("KICKING XP", "XP", names(espn_data))
           names(espn_data) <- gsub("KICKING", "FG", names(espn_data))
           names(espn_data) <- gsub("([^0-9]+)([0-9])([^0-9]+)", "\\10\\2\\3", names(espn_data))
           names(espn_data) <- gsub("[[:punct:]]", "", names(espn_data))

           espn_data <- espn_data %>%
             separate("FG 0139", c("FG 0039", "FG ATT 0039"), sep ="/") %>%
             separate("FG 4049", c("FG 4049", "FG ATT 4049"), sep ="/") %>%
             separate("FG 50", c("FG 50", "FG ATT 50"), sep ="/") %>%
             separate("FG TOT", c("FG", "FG ATT"), sep ="/") %>%
             separate("XP", c("XP", "XP ATT"), sep ="/")

         },
         "DST" = {
           names(espn_data) <- names(espn_data) %>%
             gsub("DEFENSIVE", "DST", .) %>%
             gsub("TT", "Tackles", .) %>%
             gsub("SCK", "Sack", .) %>%
             gsub("FF", "fum_force", .) %>%
             gsub("FR", "fum_rec", .) %>%
             gsub("TD RETURNS ITD", "int_ret_td", .) %>%
             gsub("TD RETURNS FTD", "fum_ret_td", .)

         },
         names(espn_data) <- offensive_columns(names(espn_data))
  )

  espn_data <- ff_clean_names(espn_data)

  structure(espn_data, source = "ESPN", season = season, week = week, position = position)
}


