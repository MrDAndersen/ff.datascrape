#' Scrape data from CBS
#'
#' Use this function to scrape fantasy football projections from CBS.
#' @param week Week number that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#'
#' @return A \link{data.table} with results from the scrape.
#' @export
scrape_cbs <- function(week = NULL, position = c("QB", "RB", "WR", "TE", "K", "DST")){

  if(is.null(week))
    week <- 0

  position <- match.arg(position)

  cbs_base <- httr::build_url(httr::parse_url("https://www.cbssports.com/fantasy/football/stats/weeklyprojections/"))
  cbs_qry = list(print_rows = 9999)

  if(week == 0){
    cbs_path <- paste(position, "season/avg/standard", sep = "/")
  } else {
    cbs_path <- paste(position, week, "avg/standard", sep  = "/")
  }



  cbs_url <- httr::modify_url(cbs_base, path = paste0(httr::parse_url(cbs_base)$path, cbs_path), query = cbs_qry)

  cbs_page <- RCurl::getURL(cbs_url)
  if(position %in% c("QB", "RB", "WR", "TE"))
    skip = c(1:2)
  else
    skip = 1

  cbs_table <- XML::readHTMLTable(cbs_page, skip.rows = skip, header = TRUE, stringsAsFactors = FALSE, which = 1)

  if(position %in% c("QB", "RB", "WR", "TE")){
    top_tbl_header <- XML::getNodeSet(XML::htmlParse(cbs_page), "//table//tr")[[2]]

    th_list <- lapply(XML::xmlChildren(top_tbl_header), XML::xmlGetAttr, name = "colspan")
    th_values <- lapply( XML::xmlChildren(top_tbl_header), XML::xmlValue)


    col_category <- vector(mode = "character")
    for(th in seq_along(unlist(th_list))){
      if(as.numeric(th_list[[th]]) == 1)
        col_category <- c(col_category, "")
      else
        col_category <- c(col_category, rep(th_values[[th]], as.numeric(th_list[[th]])))
    }
    col_category <- trimws(gsub("Â|\\s$|î€‚", "", col_category))

    cbs_cols <- trimws(paste(col_category, names(cbs_table)))
    names(cbs_table) <- cbs_cols
  }
  if(length(grep("Pages: ", cbs_table[,1])) > 0 )
    cbs_table <- cbs_table[-grep("Pages: ", cbs_table[,1]),]

  cbs_links <- XML::getHTMLLinks(cbs_page)

  player_links <- cbs_links[grep("/fantasyfootball/players/playerpage/[0-9]{3,6}", cbs_links)]

  player_ids <- stringr::str_extract(player_links, "[0-9]{3,6}")

  name_teams_list <- strsplit(cbs_table[,1], ",Â\\s")
  player_names <- unlist(lapply(name_teams_list, function(n)n[[1]]))
  player_teams <- unlist(lapply(name_teams_list, function(n)n[[2]]))

  cbs_table[,1] <- player_names
  if(length(player_ids) == nrow(cbs_table))
  cbs_table$id <- player_ids
  cbs_table$Team <- player_teams
  cbs_table$Pos <- position

  return(cbs_table)
}
