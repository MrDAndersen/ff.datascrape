#' @export
scrape_espn <- function(season, week, position = c("QB", "RB", "WR", "TE")){
  espn_positions <- c("QB" = 0, "RB" = 2, "WR" = 4, "TE" = 6)
  espn_base <- httr::build_url(httr::parse_url("http://games.espn.com/ffl/tools/projections"))
  espn_qry <- list(slotCategoryId = espn_positions[[position]])

  if(week == 0){
    espn_qry$seasonTotals <- "true"
  } else {
    espn_qry$scoringPeriodId <- week
  }

  espn_qry$seasonId <- season

  espn_qry$startIndex <- 0

  espn_url <- httr::modify_url(espn_base, query = espn_qry)

  espn_dt <- data.table::data.table()

  repeat({
    espn_page <- xml2::read_html(espn_url)

    espn_tbl <- rvest::html_table(rvest::html_nodes(espn_page, "table")[[2]], header = FALSE)
    espn_tbl <- espn_tbl[-c(1,2),]

    if(nrow(espn_tbl) == 0)
      break

    tbl_header <- XML::getNodeSet(XML::htmlParse(espn_page), "//table//tr")[[3]]

    header_child <- XML::xmlChildren(tbl_header)

    espn_cols <- unlist(lapply(header_child[which(names(header_child) == "td")], function(td){
      if(any(names(XML::xmlAttrs(td)) == "title"))
        return(XML::xmlGetAttr(td, "title"))
      else{
        tdc <- XML::xmlChildren(td)
        if(any(names(tdc) == "span"))
          return(XML::xmlGetAttr(tdc[["span"]], "title"))
        else
          return(paste0(unlist(lapply(tdc, XML::xmlValue)), collapse = ""))
      }
    }), use.names = FALSE)

    espn_cols <- gsub("\n|, TEAM POS", "", espn_cols)

    names(espn_tbl) <- espn_cols

    espn_tbl$PLAYER = gsub("\\*", "", espn_tbl$PLAYER)

    espn_tbl <- tidyr::extract(data = espn_tbl, col = "PLAYER", into = c("Player", "Team", "Pos", "Status"),
                               "([A-Za-z .'-]+),\\s([A-Za-z]+)\\s*([A-Za-z]+)\\s*([A-Za-z]*)")

    playerid <- unique(unlist(lapply(XML::getNodeSet(XML::htmlParse(espn_page), "//table//a[@class='flexpop']"), XML::xmlGetAttr, name = "playerid")))

    espn_tbl$id <- playerid

    if(any(names(espn_tbl) == "Each Pass Completed")){
      espn_tbl <- tidyr::extract(data = espn_tbl, col = "Each Pass Completed",
                                       into = c("Passes Completed", "Passes Attempted"),
                                       "([0-9]+\\.*[0-9]*)/([0-9]+\\.*[0-9]*)")
    }

    espn_dt <- data.table::rbindlist(list(espn_dt, espn_tbl), fill = TRUE)

    espn_qry <- httr::parse_url(espn_url)$query
    espn_qry$startIndex <- as.numeric(espn_qry$startIndex) + 40

    espn_url <- httr::modify_url(espn_url, query = espn_qry)
  })


  return(espn_dt)
}


