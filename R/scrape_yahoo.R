#' @import httr XML RCurl
#' @export
scrape_yahoo <- function(league_id,
                         position = c("O", "DP", "QB", "RB", "WR", "TE", "K", "DEF",
                                      "D", "DB", "DL", "LB", "DT", "DE", "CB", "S"),
                         week = NULL, season = NULL){

  if(missing(position))
    stop("Please provide position to scrape for", call. = FALSE )

  position <- match.arg(position)

  if(missing(league_id))
    stop("Please provide your Yahoo League ID", call. = FALSE )

  if(is.null(season) & is.null(week))
    stop("Please supply either week number or season", call. = FALSE)

  if(week == 0 & is.null(season))
    stop("Please supply either season for week = 0", call. = FALSE)

  yahoo_base <- httr::build_url(httr::parse_url("https://football.fantasysports.yahoo.com/f1/"))

  yahoo_qry <- list(sort = "PTS", sdir = "1", status = "A", pos = position, stat1 = "", jsenabled = 1, count = 0)

  if(week > 0){
    yahoo_qry$stat1 <- paste0("S_PW_", week)
  } else {
    yahoo_qry$stat1 <- paste0("S_PS_", season)
  }

  yahoo_path <- paste(league_id, "players", sep = "/")
  yahoo_path <- paste0(httr::parse_url(yahoo_base)$path, yahoo_path)

  yahoo_url <- httr::modify_url(yahoo_base, path = yahoo_path, query = yahoo_qry)

  # Initializing a data frame to hold results
  yahoo_df <- data.frame()

  # Loop through the pages until we get a page with less than two rows
  repeat({
    yahoo_page <- xml2::read_html(yahoo_url)

    yahoo_html_tbl <- xml2::xml_find_all(yahoo_page, "//table")[[2]]

    yahoo_tbl <- rvest::html_table(yahoo_html_tbl)

    if(nrow(yahoo_tbl) <= 1)
      break

    # The yahoo page has a table header with two rows. We will use info from the first to add
    # to the second,
    names(yahoo_tbl) <- trimws(paste(names(yahoo_tbl), yahoo_tbl[1,]))
    yahoo_tbl <- yahoo_tbl[-1,]

    yahoo_tbl[, c(which(nchar(names(yahoo_tbl)) == 0 ), grep("NA", names(yahoo_tbl)))] <- NULL

    yahoo_tbl[,1] <- gsub("[[:cntrl:]]+", "", yahoo_tbl[,1])

    names(yahoo_tbl)[1] <- "Player"
    yahoo_tbl <- tidyr::extract(yahoo_tbl, Player, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
                                "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)"
    )

    if(any(names(yahoo_tbl) == "Forecast"))
      yahoo_tbl$Forecast <- NULL

    if(any(names(yahoo_tbl) == "Owner"))
      yahoo_tbl$Owner <- NULL

    yahoo_tbl[, c("Note", "Status/Game/Opp")] <- NULL

    names(yahoo_tbl) <- gsub("[^A-Za-z0-9]+$", "", iconv(names(yahoo_tbl)))

    # Getting Yahoo IDs from the player links
    yahoo_ids <- basename(xml2::xml_attr(
      xml2::xml_find_all(
        yahoo_page,
        "//a[contains(@href, 'nfl/players') and not(contains(@class, 'playernote'))]"),
      attr = "href"))

    if(length(yahoo_ids) == nrow(yahoo_tbl))
      yahoo_tbl$id <- yahoo_ids
    yahoo_df <- dplyr::bind_rows(yahoo_df, yahoo_tbl)

    yahoo_qry <- httr::parse_url(yahoo_url)$query
    yahoo_qry$count <- as.integer(yahoo_qry$count) + 25

    yahoo_url <- httr::modify_url(yahoo_url, query = yahoo_qry)

  })

  return(yahoo_df)
}
