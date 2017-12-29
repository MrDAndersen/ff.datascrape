#' @import tidyverse httr rvest
#' @export
scrape_espn <- function(season, week, position = c("QB", "RB", "WR", "TE", "DST", "K")){
  espn_positions <- c("QB" = 0, "RB" = 2, "WR" = 4, "TE" = 6, "DST" = 16, "K" = 17)
  espn_base <- str_to_url("http://games.espn.com/ffl/tools/projections")
  espn_qry <- list(slotCategoryId = espn_positions[[position]])

  if(week == 0){
    espn_qry$seasonTotals <- "true"
  } else {
    espn_qry$scoringPeriodId <- week
  }

  espn_qry$seasonId <- season

  espn_qry$startIndex <- 0

  espn_url <- modify_url(espn_base, query = espn_qry)

  espn_session <- html_session(espn_url)

  espn_data <- data.frame()
  repeat({
    espn_page <- read_html(espn_session)

    espn_tbl <- espn_page %>%
      html_node("#playertable_0") %>%
      html_table()

    names(espn_tbl) <- gsub("DEFENSIVE PLAYERS |PLAYERS |KICKERS ", "",
                            paste(names(espn_tbl), espn_tbl[1,]))

    espn_tbl <- espn_tbl[-1,]

    if(position != "DST"){
      espn_tbl <- espn_tbl %>%
        extract("PLAYER, TEAM POS", into = c("PLAYER", "TEAM", "POS", "STATUS"),
                "([A-Za-z .'-\\*]+),\\s([A-Za-z]+)\\s*([A-Za-z]+)\\s*([A-Za-z]*)")
    } else {
      espn_tbl <- espn_tbl %>%
        extract("PLAYER, TEAM POS", into = c("PLAYER", "POS"),
                "([A-Za-z0-9 ./'-\\*]+)\\s([A-Za-z/]+)")
    }
    espn_ids <- espn_page %>%
      html_nodes("table td.playertablePlayerName a.flexpop:first-of-type") %>%
      html_attr("playerid")

    espn_tbl <- espn_tbl %>% add_column(espn_id = espn_ids, .before = 1)

    if(any(names(espn_tbl) == "PASSING C/A")){
      espn_tbl <- espn_tbl %>%
        extract(col = "PASSING C/A", c("PASSING COMP", "PASSING ATT"),
                "([0-9]+\\.*[0-9]*)/([0-9]+\\.*[0-9]*)")
    }


    espn_data <- bind_rows(espn_data, espn_tbl)

    next_url <- espn_page %>%
      html_node("a:contains('NEXT')") %>%
      html_attr("href")

    if(is.na(next_url))
      break

    espn_session <- espn_session %>% jump_to(next_url)

  })

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


  espn_data <- espn_data %>% janitor::clean_names() %>%
    clean_format() %>%  type_convert()

  structure(espn_data, source = "ESPN", season = season, week = week, position = position)
}


