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

  # Initializing a data table to hold results
  yahoo_dt <- data.table::data.table()

  # Loop through the pages until we get a page with less than two rows
  repeat({
    yahoo_page <- RCurl::getURL(yahoo_url)
    yahoo_tbl <- tryCatch(XML::readHTMLTable(yahoo_page, which = 2, stringsAsFactors = FALSE),
                          error = function(e)yahoo_dt[0])
    if(nrow(yahoo_tbl) <= 2)
      break


    # The yahoo page has a table header with two rows. We will use info from the first to add
    # to the second,
    top_tbl_header <- XML::getNodeSet(XML::htmlParse(yahoo_page), "//table//thead/tr")[[1]]
    th_list <- lapply(XML::xmlChildren(top_tbl_header), XML::xmlGetAttr, name = "colspan")
    th_values <- lapply( XML::xmlChildren(top_tbl_header), XML::xmlValue)

    th_list <- lapply(th_list, function(th)ifelse(is.null(th), "1", th))

    # Removing extra characters in the column names
    table_cols <- trimws(gsub("Â|\\s$|î€‚", "", names(yahoo_tbl)))

    # Based on the column span of the first header row we generate a vector of category
    # names
    col_category <- vector(mode = "character")
    for(th in seq_along(unlist(th_list))){
      if(as.numeric(th_list[[th]]) == 1)
        col_category <- c(col_category, "")
      else
        col_category <- c(col_category, rep(th_values[[th]], as.numeric(th_list[[th]])))
    }
    col_category <- trimws(gsub("Â|\\s$|î€‚", "", col_category))

    # Combining the category names with the original names and updating the table
    new_cols <- trimws(paste(col_category, table_cols))

    names(yahoo_tbl) <- new_cols
    yahoo_tbl[which(names(yahoo_tbl) == "")] <- NULL

    # Cleaning up the player name column
    yahoo_tbl[,1] <- gsub(" {2,15}", "", yahoo_tbl[,1])
    yahoo_tbl[,1] <- gsub("\\n", "", yahoo_tbl[,1])
    yahoo_tbl[,1] <- gsub("No new player Notes|New Player Note|Player Note", "", yahoo_tbl[,1])

    # Extracting name, team and position
    name_list <- strsplit(yahoo_tbl[,1], " - ")
    name_teams <- unlist(lapply(name_list, function(nl)nl[1]))
    name_team_list <- strsplit(name_teams, " ")

    player_pos <- unlist(lapply(name_list, function(nl){
      pos_list <- strsplit(nl[2], " ")
      return(pos_list[[1]][1])
    }))

    teams <- unlist(lapply(name_team_list, function(tm)tm[length(tm)]))
    player_names <- unlist(lapply(name_team_list, function(tm)paste(tm[1:(length(tm)-1)], collapse = " ")))

    # Getting Yahoo IDs from the player links
    yahoo_links <- XML::getHTMLLinks(yahoo_page)
    yahoo_ids <- basename((yahoo_links[grep("https://sports.yahoo.com/nfl/players/[0-9]{3,6}$", yahoo_links)]))

    if(length(yahoo_ids) == nrow(yahoo_tbl))
      yahoo_tbl$id <- yahoo_ids

    yahoo_tbl$Team <- teams
    yahoo_tbl$Pos <- player_pos
    yahoo_tbl[, 1] <- player_names

    yahoo_dt <- data.table::rbindlist(list(yahoo_dt, yahoo_tbl), fill = TRUE)

    yahoo_qry <- httr::parse_url(yahoo_url)$query
    yahoo_qry$count <- as.integer(yahoo_qry$count) + 25

    yahoo_url <- httr::modify_url(yahoo_url, query = yahoo_qry)

  })

  return(yahoo_dt)
}
