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

  cbs_session <- html_session(cbs_url)
  cbs_page <- read_html(cbs_session)

  html_nodes(cbs_page, "table")
  cbs_table <- html_table(html_node(cbs_page, "table"), fill = TRUE, header = TRUE)

  if(position %in% c("QB", "RB", "WR", "TE")){
    names(cbs_table) <- trimws(paste(cbs_table[1,], cbs_table[2,]))
    cbs_table <- cbs_table %>% slice(-c(1,2))
    test_col <- cbs_table$`Misc FPTS`
  } else {
    names(cbs_table) <- cbs_table[1,]
    cbs_table <- cbs_table %>% slice(-1)
    test_col <- cbs_table$FPTS
  }

  test_func <- function(x)!as.logical(length(unlist(regmatches(x, regexec("Pages", x)))))

  row_test <- sapply(test_col, test_func, USE.NAMES = FALSE)

  cbs_table <- cbs_table[row_test,]

  player_links <- cbs_page %>%
    html_nodes("table a[href *= 'playerpage']") %>%
    html_attr("href")

  pids <- player_links %>% str_extract("[0-9]{3,8}")

  cbs_table <- cbs_table %>%
    extract("Player", c("Player", "Team"), "([A-Za-z'-. ]+),\\s([A-Za-z]+)")

  if(length(pids) == nrow(cbs_table)){
    cbs_table <- cbs_table %>% add_column(cbs_id = pids, .before = 1)
  }

  print(names(cbs_table))
  if(position == "DST")
    cbs_table <- cbs_table %>% rowwise() %>% mutate(cbs_id = cbs_def_id[which(cbs_def == Team)])

  cbs_table <- cbs_table %>% add_column(Pos = position, .before = "Team")

  del_cols <- intersect(names(cbs_table), c("NA", "NA NA"))

  cbs_table <- cbs_table %>% select(-one_of(!!!del_cols)) %>%
    data.frame(stringsAsFactors = FALSE)

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

  cbs_table <- cbs_table %>% janitor::clean_names() %>%
    clean_format() %>%  type_convert()

  if(any(names(cbs_table) == "cbs_id"))
    cbs_table <- cbs_table %>% add_column(id = id_col(cbs_table$cbs_id, "cbs_id"), .before = 1)

  structure(cbs_table, source = "CBS", week = week)
}

