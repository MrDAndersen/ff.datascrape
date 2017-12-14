#' @import tidyverse httr rvest
#' @export
scrape_numfire <- function(season = NULL, week = NULL,
                           position = c("Off", "QB", "RB", "WR", "TE", "RB/WR", "K", "DEF", "IDP")){


  nmf_pos <- switch(position,
                    "Off" = "",
                    "RB/WR" = "rbwr",
                    "DEF" = "d",
                    tolower(position))

  if(is.null(week)){
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


  nmf_player_table <- nmf_player_table %>% add_column(id = nmf_ids, .before = -1)

  if(position == "DEF"){
    nmf_player_table$Player <- trimws(gsub("D/ST", "", nmf_player_table$Player))
    nmf_player_table$`Abbr Name` <- NULL
    nmf_player_table$Pos <- "DEF"
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
  structure(data.frame(nmf_data), source = "NumberFire", season = season,
            week = week, position = position)
}
