
#' @export
scrape_fleaflick <- function(
  position = c("QB", "RB", "WR", "TE", "Flex", "K", "DST", "DB", "DL", "LB", "IDP")
  )
{
  flea_positions = c("QB" = 4, "RB" = 1, "WR" = 2, "TE" = 8, "Flex" = 11, "K" = 16,
                     "DST" = 256, "DB" = 32, "DL" = 64, "LB" = 128, "IDP" = 224)

  flea_base <- httr::build_url(httr::parse_url("https://www.fleaflicker.com/nfl/leaders"))
  flea_qry <- list(statType = 7, sortMode = 1)

  flea_qry$position <- flea_positions[[position]]
  flea_qry$tableOffset <- 0

  flea_url <- httr::modify_url(flea_base, query = flea_qry)

  flea_dt <- data.table::data.table()

  repeat({

    flea_page <- RCurl::getURL(flea_url)

    flea_table <- tryCatch(
      XML::readHTMLTable(flea_page, which = 1, stringsAsFactors = FALSE, 
                         skip.rows = 1:2),
      error = function(e)return(flea_dt[0])
    )

    if(nrow(flea_table) == 0)
      break

    tbl_head <- XML::getNodeSet(XML::htmlParse(flea_page), "//thead//tr")[[1]]

    th_list <- lapply(XML::xmlChildren(tbl_head), XML::xmlGetAttr, name = "colspan")
    th_list <- lapply(th_list, function(th)ifelse(is.null(th), 1, th))

    th_values <- lapply(XML::xmlChildren(tbl_head), XML::xmlValue)

    col_category <- vector(mode = "character")
    for(th in seq_along(unlist(th_list))){
      if(as.numeric(th_list[[th]]) == 1)
        col_category <- c(col_category, "")
      else
        col_category <- c(col_category, rep(th_values[[th]], as.numeric(th_list[[th]])))
    }

    col_category[grep("Passing", col_category)] <- "Passing"
    col_category[grep("Defense", col_category)] <- "Defense"
    col_category[grep("Kicking", col_category)] <- "Kicking"
    
    names(flea_table) <-  make.unique(trimws(paste(col_category, names(flea_table))))

    flea_table[, which(names(flea_table) == "")] <- NULL
    flea_table[, which(names(flea_table) %in% paste0(".", 1:9))] <- NULL
    
    flea_table[, 1] <- gsub("^(Q|D|OUT|SUS|IR)([A-Z])", "\\2", flea_table[, 1])

    flea_table <- flea_table[1:20,]

    flea_table <- tidyr::extract(
        flea_table, Name, c("Player", "Pos", "Team", "Bye"), 
        "([A-Za-z0-9'-.\\s]+)\\s([QRWTDKL][BRESL/]*[SFT]*[T]*)\\s([ABCDFGHIJKLMNOPSTW][ACIOUHFTELNYBR][TLCFIUSRNEDGJKA]*)\\s*\\(*([0-9]*)\\)*"
      )
    player_links <- unlist(lapply(XML::getNodeSet(XML::htmlParse(flea_page), "
                                                  //a[@class='player-text']"), 
                                  XML::xmlGetAttr, name = "href"))
    
    player_id <- stringr::str_extract(player_links, "[0-9]{3,}$")

    if(length(player_id) < nrow(flea_table))
      flea_table <- flea_table[1:length(player_id),]
    flea_table$id <- player_id

    flea_dt <- data.table::rbindlist(list(flea_dt, flea_table), fill = TRUE)

    flea_qry <- httr::parse_url(flea_url)$query
    flea_qry$tableOffset <- as.integer(flea_qry$tableOffset) + 20

    flea_url <- httr::modify_url(flea_url, query = flea_qry)
  })

  return(flea_dt)
}

