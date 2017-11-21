
#' @export
scrape_fftoday <- function(
  season = NULL, week = NULL,
  position = c("QB", "RB", "WR", "TE", "K", "DEF", "DL", "LB", "DB")
)
{


  fft_positions <- c(QB = 10, RB=20, WR=30, TE=40, K = 80, DEF =
                       99, DL = 50, LB = 60, DB =70)
  fft_base <- httr:: build_url(
    httr::parse_url("http://www.fftoday.com/rankings/")
  )

  fft_qry <- list(Season = season)

  if(week == 0)
    week <- NULL

  fft_file <- as.character()

  if(is.null(week))
    fft_file <- "playerproj.php"
  else {
    fft_file <- "playerwkproj.php"
  }

  fft_path <- paste0(httr::parse_url(fft_base)$path, fft_file)

  if(!is.null(week))
    fft_qry$GameWeek <- week

  fft_qry$LeagueID <- 1

  fft_qry$PosID <- fft_positions[[position]]

  fft_qry$cur_page <- 0


  fft_url <- httr::modify_url(fft_base, path = fft_path, query = fft_qry)

  fft_dt <- data.table::data.table()

  repeat({
    fft_page <- RCurl::getURL(fft_url)

    fft_table <- tryCatch(
      XML::readHTMLTable(fft_page, which = 11, stringsAsFactors = FALSE,
                         header = TRUE, skip.rows = 1),
      error = function(e)return(NULL)
    )

    if(is.null(fft_table))
      break

    tbl_header <- XML::getNodeSet(
      XML::htmlParse(fft_page), "//table//tr[@class='tablehdr']"
    )[[1]]

    th_list <- lapply(XML::xmlChildren(tbl_header),
                      XML::xmlGetAttr, name = "colspan")
    th_values <- lapply(XML::xmlChildren(tbl_header), XML::xmlValue)


    th_list <- th_list[which(names(th_list) == "td")]
    th_values <- th_values[which(names(th_values) == "td")]

    th_list <- lapply(th_list, function(th)ifelse(is.null(th), 1, th))

    col_category <- vector(mode = "character")

    for(th in seq_along(unlist(th_list))){
      if(as.numeric(th_list[[th]]) == 1)
        col_category <- c(col_category, "")
      else
        col_category <- c(col_category,
                          rep(th_values[[th]], as.numeric(th_list[[th]])))
    }

    col_category <- gsub("[^[:alpha:]]", "", col_category)


    names(fft_table) <- trimws(
      gsub("[^[:alpha:]]|Sort|First:|Last:", "", names(fft_table))
    )

    names(fft_table) <- trimws(paste(col_category, names(fft_table)))
    fft_table[,2] <- gsub("^[^[:alpha:]]", "", fft_table[,2])

    fft_table[,1] <- trimws(gsub("Â", "", fft_table[,1]))
    fft_table[,2] <- trimws(gsub("Â", "", fft_table[,2]))

    fft_links <- XML::getHTMLLinks(fft_page)
    player_links <- fft_links[grep("/stats/players/", fft_links)]

    player_id <- stringr::str_extract(player_links, "[0-9]{2,6}")

    if(length(player_id) == nrow(fft_table))
      fft_table$id <- player_id

    fft_table$Pos <- position

    fft_dt <- data.table::rbindlist(list(fft_dt, fft_table), fill = TRUE)

    fft_qry <- httr::parse_url(fft_url)$query
    fft_qry$cur_page <- as.integer(fft_qry$cur_page) + 1

    fft_url <- httr::modify_url(fft_url, query = fft_qry)
  })

  data.table::setnames(fft_dt, trimws(gsub("Â", "", names(fft_dt))))

  return(fft_dt)

}
