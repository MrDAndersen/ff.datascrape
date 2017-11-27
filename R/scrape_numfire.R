#' @import xml2 rvest httr tidyr
#' @export
scrape_numfire <- function(type = c("season", "weekly"),
                           position = c("Off", "QB", "RB", "WR", "TE", "RB/WR", "K", "DEF", "IDP")){

  nmf_pos <- switch(position,
                    "Off" = "",
                    "RB/WR" = "rbwr",
                    "DEF" = "d",
                    tolower(position))

  nmf_path <- switch(type,
                     "season" = "remaining-projections",
                     "weekly" = "fantasy-football-projections")

  if(position != "Off")
    nmf_path <- paste(nmf_path, nmf_pos, sep = "/")

  nmf_base <- httr::build_url(httr::parse_url("https://www.numberfire.com/nfl/fantasy/"))

  nmf_url <- httr::modify_url(nmf_base, path = paste0(httr::parse_url(nmf_base)$path, nmf_path))

  nmf_proj <- xml2::read_html(nmf_url)
  nmf_tables <- rvest::html_table(nmf_proj)

  player_cells <- xml2::xml_find_all(nmf_proj, "//td[@class='player']")
  nmf_ids <- basename(unlist(lapply(xml2::xml_children(player_cells), xml2::xml_attr, attr = "href")))

  nmf_player_table <- nmf_tables[[1]]
  names(nmf_player_table) <- "Player"

  nmf_player_table <- tidyr::extract(nmf_player_table, "Player", c("Player", "Abbr Name", "Pos", "Team"),
                 "([A-Za-z ,.'-/]+)\\n *([A-Za-z ,.'-/]+)\\n *\\(([A-Z]+), ([A-Z]+)\\)")

  nmf_player_table <- nmf_player_table[-1,]

  nmf_player_table$id <- nmf_ids
  if(position == "DEF"){
    nmf_player_table$Player <- trimws(gsub("D/ST", "", nmf_player_table$Player))
    nmf_player_table$`Abbr Name` <- NULL
    nmf_player_table$Pos <- "D/ST"
  }

  nmf_stat_table <- nmf_tables[[2]]
  names(nmf_stat_table) <- paste(names(nmf_stat_table), nmf_stat_table[1,])
  nmf_stat_table <- nmf_stat_table[-1, ]

  if(any(names(nmf_stat_table) == "Passing C/A"))
    nmf_stat_table <- tidyr::extract(data = nmf_stat_table, col = "Passing C/A",
                                     into = c("Pass Completions", "Pass Attempts"),
                                     "([[:digit:]]+\\.[[:digit:]]+)/([[:digit:]]+\\.[[:digit:]]+)")

  return(data.table::data.table(cbind(nmf_player_table, nmf_stat_table)))

}
