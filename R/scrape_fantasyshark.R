#' @export
scrape_fantasysharks <- function(season = NULL,  week = NULL,
                                 position =c("QB", "RB", "WR", "TE", "K", "DEF",
                                             "DL", "LB", "DB")){

  fs_pos <- c("QB" = 1, "RB" =2 , "WR" = 4, "TE" = 5, "K" = 7, "DEF" = 6, "DL" = 8, "LB"=9, "DB" = 10)

  fs_base <- str_to_url("https://www.fantasysharks.com/apps/bert/forecasts/projections.php")

  fs_qry <- list(League=-1, scoring=1, uid=4)

  fs_qry$Segment <- shark_segment(season, week)

  fs_qry$Position <- fs_pos[position]

  fs_url <- modify_url(fs_base, query = fs_qry)

  fs_page <- read_html(fs_url)

  fs_page %>% html_nodes("tr.separator") %>% xml_remove()
  fs_page %>% html_nodes("#toolData tr[valign ='middle']:not(:first-child)") %>% xml_remove()
  fs_page %>% html_nodes("#toolData tr[height ='20px']") %>% xml_remove()
  fs_table <- fs_page %>% html_node("#toolData") %>% html_table()

  fs_table <- fs_table %>% repair_names()

  fs_ids <- fs_page %>% html_nodes("td.playerLink a") %>% html_attr("href") %>%
    sapply(., function(u)parse_url(u)$query$id, USE.NAMES = FALSE)

  ### NOTE: Fantasysharks is using the same id as MFL
  fs_table <- fs_table %>% add_column(id = str_pad(fs_ids, 4, "left", "0"), .before = 1)

  return(fs_table)
}
