#' Scrape data from CBS
#'
#' Use this function to scrape fantasy football projections from CBS.
#' @param week Week number that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#'
#' @return A data frame with results from the scrape.
#' @import tidyverse rvest httr stringr
#' @export
scrape_cbs <- function(week = NULL, position = c("QB", "RB", "WR", "TE", "K", "DST")){

  position <- match.arg(position)

  cbs_base <- str_to_url("https://www.cbssports.com/fantasy/football/stats/weeklyprojections/")
  cbs_qry = list(print_rows = 9999)

  if(is.null(week) || week == 0){
    cbs_path <- paste(position, "season/avg/standard", sep = "/")
  } else {
    if(!(week %in% 1:21))
      stop("When specifying a week please only use numbers between 1 and 21", call. = FALSE)

    cbs_path <- paste(position, week, "avg/standard", sep  = "/")
  }

  cbs_url <- httr::modify_url(cbs_base, path = paste0(httr::parse_url(cbs_base)$path, cbs_path), query = cbs_qry)

  cbs_table <- scrape_html_data(cbs_url)

  cbs_table <- cbs_table %>%
    extract("Player", c("Player", "Team"), "([A-Za-z'-. ]+),\\s([A-Za-z]+)")

  if(position == "DST"){
    cbs_table <- cbs_table %>% rowwise() %>% mutate(src_id = cbs_def_id[which(cbs_def == Team)])
    cbs_table <- cbs_table %>% add_column(id = id_col(cbs_table$src_id, "cbs_id"), .before = 1)
  }

  cbs_table <- cbs_table %>% add_column(Pos = position, .before = "Team")

  names(cbs_table) <- names(cbs_table) %>%
    gsub("Misc.FL", "fumbles lost", .) %>%
    gsub("YAtt", "YardsPerAttempt", .) %>%
    gsub("FGA", "fg Att", .) %>%
    gsub("^Int$", "dst int", .) %>%
    gsub("DFR", "dst fum rec", .) %>%
    gsub("FF", "dst fum force", .) %>%
    gsub("SACK", "dst sack", .) %>%
    gsub("DTD", "dst Td", .) %>%
    gsub("STY", "dst safety", .) %>%
    gsub("PA", "dst pts_Allow", .) %>%
    gsub("TYdA", "dst yds_Allow", .)

  if(position %in%  c("QB", "RB", "WR", "TE"))
    names(cbs_table) <- offensive_columns(names(cbs_table))

  cbs_table <- ff_clean_names(cbs_table)

  structure(cbs_table, source = "CBS", week = week)
}

