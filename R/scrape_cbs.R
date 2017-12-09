#' Scrape data from CBS
#'
#' Use this function to scrape fantasy football projections from CBS.
#' @param week Week number that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#'
#' @return A \link{data.table} with results from the scrape.
#' @import tidyverse rvest httr stringr
#' @export
scrape_cbs <- function(week = NULL, position = c("QB", "RB", "WR", "TE", "K", "DST")){

  if(is.null(week))
    week <- 0

  position <- match.arg(position)

  cbs_base <- str_to_url("https://www.cbssports.com/fantasy/football/stats/weeklyprojections/")
  cbs_qry = list(print_rows = 9999)

  if(week == 0){
    cbs_path <- paste(position, "season/avg/standard", sep = "/")
  } else {
    cbs_path <- paste(position, week, "avg/standard", sep  = "/")
  }

  cbs_url <- httr::modify_url(cbs_base, path = paste0(httr::parse_url(cbs_base)$path, cbs_path), query = cbs_qry)

  cbs_session <- html_session(cbs_url)
  cbs_page <- read_html(cbs_session)


  html_nodes(cbs_page, "table")
  cbs_table <- html_table(html_node(cbs_page, "table"), fill = TRUE, header = TRUE)

  if(position %in% c("QB", "RB", "WR", "TE")){
    names(cbs_table) <- trimws(paste(cbs_table[1,], cbs_table[2,]))
    cbs_table <- cbs_table %>% slice(-c(1,2))
  } else {
    names(cbs_table) <- cbs_table[1,]
    cbs_table <- cbs_table %>% slice(-1)
  }

  if(length(grep("Pages: ", cbs_table[,1])) > 0 )
    cbs_table <- cbs_table[-grep("Pages: ", cbs_table[,1]),]

  player_links <- cbs_page %>%
    html_nodes("table a[href *= 'playerpage']") %>%
    html_attr("href")

  player_ids <- player_links %>% str_extract("[0-9]{3,6}")

  cbs_table <- cbs_table %>%
    extract("Player", c("Player", "Team"), "([A-Za-z'-. ]+),\\s([A-Za-z]+)")

  if(length(player_ids) == nrow(cbs_table)){
    cbs_table <- cbs_table %>% add_column(id = player_ids, .before = 1)
  }

  cbs_table <- cbs_table %>% add_column(Pos = position, .before = "Team")

  del_cols <- intersect(names(cbs_table), c("NA", "NA NA"))

  cbs_table <- cbs_table %>% select(-one_of(!!!del_cols)) %>%
    data.frame(stringsAsFactors = FALSE)

  structure(cbs_table, source = "CBS", week = week)
}

