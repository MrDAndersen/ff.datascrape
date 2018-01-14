#' Scrape data from RTSports
#'
#' Use this function to srape fantasy football projections from RTSports. Only
#' seasonal projections are available.
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#' @export
scrape_rts <- function(position = c("QB", "RB", "WR", "TE", "K", "DST")){
  position <- match.arg(position)
  p <- c(QB = 0, RB = 1, WR = 2, TE = 3, K = 4, DST = 5)
  rts_url <-paste0("https://www.freedraftguide.com/football/draft-guide-rankings-provider.php?POS=", p[position])

  rts_list <- httr::content(httr::GET(rts_url))

  rts_data  <- dplyr::bind_rows(lapply(rts_list$player_list,
                                       function(pl)dplyr::bind_cols(
                                         data.frame(pl[c("player_id", "name", "position", "nfl_team")],
                                                    stringsAsFactors = FALSE),
                                         data.frame(pl$stats, stringsAsFactors = FALSE))))

  rts_data$position <-   names(sapply(rts_data$position, grep, p, value = TRUE))


  rts_data <- rts_data %>% type_convert()

  not_zero <- function(x)!all(ifelse(is.na(x),0,x) == 0)
  rts_data <- rts_data %>% select_if(not_zero) %>%
    rename(rts_id = player_id, player = name, pos = position, team = nfl_team)

  names(rts_data) <- names(rts_data) %>%
    gsub("_rcpts", "", .) %>%
    gsub("atts$", "att", .) %>%
    gsub("kick_patm", "xp", .) %>%
    gsub("kick_fgm", "fg",.)

  if(position == "DST"){
    rts_data <- rts_data %>%
      rename(dst_pts_allow = "pts_allowed", dst_yds_allow = "yds_allowed",
             dst_sack = "sacks", dst_int = "ints", dst_fum_rec = "fumbles_recovered")
  }

  if(position %in% c("QB", "RB", "WR", "TE"))
    names(rts_data) <- names(rts_data) %>% offensive_columns()

  if(any(names(rts_data) == "rts_id"))
    rts_data <- rts_data %>% add_column(id = id_col(rts_data$rts_id, "rts_id"), .before = 1)

  structure(rts_data, source = "RTSports", position = position)
}


