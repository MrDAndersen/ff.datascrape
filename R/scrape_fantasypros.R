#' @export
scrape_fantasypros <- function(season = NULL, week = NULL,
                               position = c("QB", "RB", "WR", "TE", "K", "DST")){

  fp_url <- str_to_url(
    sprintf("https://www.fantasypros.com/nfl/projections/%s.php", tolower(position))
  )

  if(is.null(week)){
    fp_url <- modify_url(fp_url, query = list(week="draft"))
  }

  fp_session <- html_session(fp_url)

  fp_page <- read_html(fp_session)

  fp_table <- fp_page %>% html_node("#data") %>% html_table(header = TRUE)

  names(fp_table) <- trimws(paste(names(fp_table), fp_table[1,]))
  fp_table <- fp_table[-1,]

  fp_table <- fp_table %>%
    extract("Player", c("Player", "Team"), "(.+)\\s([A-Z]{2,3}$)")

  fp_ids <- fp_page %>%
    html_nodes("a.player-name") %>%
    html_attr("href") %>%
    basename() %>% gsub(".php", "", .)

  fp_table <- fp_table %>%  add_column(id = fp_ids, .before = 1)
  fp_table <- fp_table %>%  add_column(Pos = position, .after = "Player")

  return(fp_table)
}
