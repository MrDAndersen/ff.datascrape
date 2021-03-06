#' Scrape data from NumberFire
#' 
#' Use this function to srape fantasy football projections from NumberFire
#' @param week The week that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("Off", "QB", "RB", "WR", "TE", "RB/WR", "K", "DST", "IDP")}. If omitted 
#' data for all offensive positions (QB, RB, WR, and TE) will be scraped. 
#' @import tidyverse httr rvest
#' @export
scrape_numfire <- function(week = NULL,
                           position = c("Off", "QB", "RB", "WR", "TE", "RB/WR", "K", "DST", "IDP")){
  position <- match.arg(position)
  
  nmf_pos <- switch(position,
                    "Off" = "",
                    "RB/WR" = "rbwr",
                    "DST" = "d",
                    tolower(position))

  if(is.null(week) || week == 0){
    nmf_path <-  "remaining-projections"
  } else{
    nmf_path <- "fantasy-football-projections"
  }

  if(position != "Off")
    nmf_path <- paste(nmf_path, nmf_pos, sep = "/")

  nmf_base <- str_to_url("https://www.numberfire.com/nfl/fantasy/")

  nmf_url <- modify_url(nmf_base, path = paste0(parse_url(nmf_base)$path, nmf_path))

  nmf_session <- html_session(nmf_url)
  nmf_proj <- read_html(nmf_session)

  nmf_tables <- html_nodes(nmf_proj, "table")

  nmf_ids <-  nmf_proj %>%
    html_nodes("td[class='player'] a") %>%
    html_attr("href") %>% basename()

  nmf_player_table <- html_table(nmf_tables[[1]])
  names(nmf_player_table) <- nmf_player_table[1,]
  nmf_player_table <- nmf_player_table %>% slice(-1)

  nmf_player_table <- nmf_player_table %>%
    extract(Player, c("Player", "Abbr Name", "Pos", "Team"),
            "([A-Za-z ,.'-/]+)\\n *([A-Za-z ,.'-/]+)\\n *\\(([A-Z]+), ([A-Z]+)\\)")


  nmf_player_table <- nmf_player_table %>% add_column(numberfire_id = nmf_ids, .before = -1)

  if(position == "DST"){
    nmf_player_table$Player <- trimws(gsub("D/ST", "", nmf_player_table$Player))
    nmf_player_table$`Abbr Name` <- NULL
    nmf_player_table$Pos <- "DST"
  }

  nmf_stat_table <- html_table(nmf_tables[[2]])
  nmf_columns <- paste(names(nmf_stat_table), nmf_stat_table[1,])
  nmf_columns <- gsub("(FanDuel|DraftKings|Yahoo)", "\\L\\1", nmf_columns, perl = TRUE)
  if(position == "K"){
    nmf_columns <- nmf_columns %>%  gsub(".Made.By.Distance.", " ", .) %>%
      gsub("Kicking.", "", .) %>%
      gsub("([^0-9])([0-9])([^0-9])", "\\10\\2\\3", . ) %>%
      gsub("(FG|XP)M", "\\1", .) %>%  gsub("(FG|XP)(A)", "\\1 \\2tt", . ) %>%
      gsub("[[:punct:]]", "", .)
  }
  names(nmf_stat_table) <- nmf_columns
  nmf_stat_table <- nmf_stat_table %>% slice(-1)

  if(any(names(nmf_stat_table) == "Passing C/A")){
    nmf_stat_table <-  nmf_stat_table %>%
      extract("Passing C/A", c("Pass Completions", "Pass Attempts"),
              "([[:digit:]]+\\.[[:digit:]]+)/([[:digit:]]+\\.[[:digit:]]+)")
  }

  nmf_data <- bind_cols(nmf_player_table, nmf_stat_table)

  if(position %in% c("Off", "QB", "RB", "WR", "TE", "RB/WR")){
    names(nmf_data) <- offensive_columns(names(nmf_data))
  }

  if(position == "DST"){
    names(nmf_data) <-names(nmf_data) %>%
      gsub("defense", "dst", ., ignore.case = TRUE) %>%
      gsub("yards", "yds", ., ignore.case = TRUE) %>%
      gsub("points", "pts", ., ignore.case = TRUE) %>%
      gsub("tds$", "td", ., ignore.case = TRUE) %>%
      gsub("ints$", "int", ., ignore.case = TRUE) %>%
      gsub("sacks$", "sack", ., ignore.case = TRUE) %>%
      gsub("allowed$", "allow", ., ignore.case = TRUE) %>%
      gsub("fumbles", "fum_rec", ., ignore.case = TRUE)
  }
  if(position == "IDP"){
    names(nmf_data) <-names(nmf_data) %>%
      gsub("defense", "idp", ., ignore.case = TRUE) %>%
      gsub("tackles", "solo", ., ignore.case = TRUE) %>%
      gsub("tds$", "td", ., ignore.case = TRUE) %>%
      gsub("ints$", "int", ., ignore.case = TRUE) %>%
      gsub("sacks$", "sack", ., ignore.case = TRUE) %>%
      gsub("passes defended", "pd", ., ignore.case = TRUE)
  }
  nmf_data <- janitor::clean_names(nmf_data) %>%
    clean_format() %>%  type_convert()

  if(any(names(nmf_data) == "numberfire_id"))
    nmf_data <- nmf_data %>% add_column(id = id_col(nmf_data$numberfire_id, "numfire_id"), .before = -1)
  
  structure(data.frame(nmf_data), source = "NumberFire", season = current_season(),
            week = week, position = position)
}
