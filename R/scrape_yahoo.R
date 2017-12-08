#' @import httr tidyverse rvest
#' @export
scrape_yahoo <- function(stat_type = c("Projected",  "Actual", "Remaining Season",
                                       "Next 4 weeks", "Last 4 Weeks", "Avg Last 4 Weeks" ),
                         position = c("O", "DP", "QB", "RB", "WR", "TE", "K", "DEF",
                                      "D", "DB", "DL", "LB", "DT", "DE", "CB", "S"),
                         season = NULL, week = NULL,
                         league_id = NULL){

  if(!missing(week)){
    week <- as.character(week)
    week = match.arg(week, choices = 1:17)
  }
  if(missing(position))
    stop("Please provide position to scrape for", call. = FALSE )

  position <- match.arg(position)

  if(missing(league_id))
    stop("Please provide your Yahoo League ID", call. = FALSE )

  if(is.null(season) & is.null(week))
    stop("Please supply either week number or season", call. = FALSE)

  yahoo_base <- str_to_url("https://football.fantasysports.yahoo.com/f1/")

  yahoo_qry <- list(sort = "PTS", sdir = "1", status = "A", pos = position,
                    stat1 = "", jsenabled = 1, count = 0)

  yahoo_qry$stat1 <- switch(
    stat_type,
    "Projected" = ifelse(week > 0, paste0("S_PW_", week) ,
                         paste0("S_PS_", season)),
    "Actual" = ifelse(week > 0, paste0("S_W_", week) ,
                      paste0("S_S_", season)),
    "Remaining Season" = paste0("S_PSR_", season),
    "Next 4 weeks" = "S_PN4W",
    "Last 4 Weeks" = "S_L4W",
    "Avg Last 4 Weeks" = "S_AL4W"
  )

  yahoo_path <- paste(league_id, "players", sep = "/")
  yahoo_path <- paste0(parse_url(yahoo_base)$path, yahoo_path)

  yahoo_url <- modify_url(yahoo_base, path = yahoo_path, query = yahoo_qry)

  yahoo_session <- yahoo_url %>% html_session()

  player_cols <- c("Offense", "Kickers", "Defense/Special Teams", "Defensive Players")

  yahoo_data <- data.frame()

  repeat({
    next_url <- yahoo_session %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")

    if(is.na(next_url))
      break

    yahoo_tbl <- yahoo_session %>%
      html_node("table[class *='Table-interactive']") %>%
      html_table()

    names(yahoo_tbl)  <- gsub("[^[:alnum:]]$", "",
                              trimws(paste(names(yahoo_tbl), yahoo_tbl[1,])))

    yahoo_tbl <- yahoo_tbl[-1,]

    names(yahoo_tbl) <- trimws(names(yahoo_tbl))
    yahoo_tbl <- yahoo_tbl %>% repair_names(prefix = "")

    tbl_cols <- setdiff(names(yahoo_tbl),
                        grep("^[0-9]+$", names(yahoo_tbl), value = TRUE))
    tbl_cols <- setdiff(tbl_cols, "NA")

    yahoo_tbl <- yahoo_tbl %>% select(one_of(tbl_cols))


    player_col <- intersect(player_cols, names(yahoo_tbl))
    names(player_col) <- "Yahoo_Player"
    yahoo_tbl <- yahoo_tbl %>% rename(!!!player_col)

    player_id <- yahoo_session %>%
      html_nodes("a[href *= 'nfl/players']:not(a[class *='playernote'])") %>%
      html_attr("href") %>%
      basename()

    if(length(player_id) > 0 )
      yahoo_tbl <- yahoo_tbl %>% add_column(id = player_id, .before = 1)

    yahoo_data <- bind_rows(yahoo_data, yahoo_tbl)

    yahoo_session <- next_url %>% jump_to(x=yahoo_session, url =.)
  })

  yahoo_data <- yahoo_data %>%
    extract(., Yahoo_Player, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
            "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)") %>%
    select(., -one_of(c("Note", "Status/Game/Opp")))

  attributes(yahoo_data)[c("source", "type", "position", "season", "week")] <- list("Yahoo", stat_type, position, season, week)
  return(yahoo_data)
}
