#' Scrape data from FantasyData
#'
#' Use this function to srape fantasy football projections from FantasyData
#' @param week The week that data will be scraped for. If omitted, season data
#' will be scraped.
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#' @export
scrape_fantasydata <- function(week = -1, position = c("QB", "RB", "WR", "TE", "K", "DST")){

  fd_positions = c("QB" = 1, "RB" = 2, "WR" = 3, "TE"= 4, "K" = 5, "DST"= 6)

  position <- match.arg(position)

  fd_base <- str_to_url("https://fantasydata.com/nfl-stats/fantasy-football-weekly-projections.aspx")


  fd_qry <- list(fs = 0, stype = 0, sn = 0, scope = 1, w = -1, ew = -1, s = "",
                 t = 0, p = 9, st="FantasyPoints", d = 1, ls = "", live="false",
                 pid="true", minsnaps=4)

  if(week > 0){
    if(!(week %in% 1:21))
      stop("When specifying a week please only use numbers between 1 and 21", call. = FALSE)

    fd_qry$w <- week
    fd_qry$ew <- week
    fd_qry$scope <- 1

    fd_qry$stype <- ifelse(week > 17, 1, 0)
  } else {
    fd_qry$scope <- 0
    fd_qry$stype <- 0
    fd_qry$w <- 0
    fd_qry$ew <- 0

  }

  fd_qry$p <- fd_positions[[position]]

  fd_url <- modify_url(fd_base, query = fd_qry)

  fd_session <- html_session(fd_url)

  fd_page <- read_html(fd_session)

  fd_table <- fd_page %>%
    html_node("table") %>% html_table()

  names(fd_table)[2:length(fd_table)] <- fd_page %>%
    html_nodes("table tr th a") %>%
    html_attr("href") %>%
    gsub("(^.+','Sort\\$)(.+)('\\))", "\\2", .) %>%
    gsub("Fantasy*", "", ., ignore.case = TRUE)

  fd_table <- fd_table %>% rename(rank = "Rk", fantasydata_id = "PlayerID",
                                  player = "Name", pos = "Position")

  if(position %in% c("QB", "RB", "WR", "TE")){
    names(fd_table) <- offensive_columns(names(fd_table)) %>%
      gsub("targets", "tgt", ., ignore.case = TRUE) %>%
      gsub("ydsperrec", "avg", ., ignore.case = TRUE)

    if(any(names(fd_table) %in% c( "Fumbles", "FumblesLost"))){
      fd_table <- fd_table %>% rename(fumbles_tot = Fumbles, fumbles_lost = FumblesLost)
    }

  }

  if(position == "K"){
    names(fd_table) <- names(fd_table) %>%
      gsub("FieldGoals*", "fg", ., ignore.case = TRUE) %>%
      gsub("ExtraPoints", "xp", ., ignore.case = TRUE) %>%
      gsub("Made", "", ., ignore.case = TRUE) %>%
      gsub("Attempted*", "_att", ., ignore.case = TRUE) %>%
      gsub("Longest*", "_long", ., ignore.case = TRUE) %>%
      gsub("percentage*", "_pct", ., ignore.case = TRUE)
  }

  fd_table <- fd_table %>% janitor::clean_names()

  if(position == "DST"){
    fd_table <- fd_table %>% rename(
      dst_TFL = "tacklesforloss", dst_sack = "sacks",
      dst_qb_hits = "quarterbackhits", dst_int= "interceptions",
      dst_fum_rec = "fumblesrecovered", dst_safety = "safeties",
      dst_def_td = "defensivetouchdowns", dst_st_td = "specialteamstouchdowns",
      dst_pts_allow = "pointsallowed"
    )
  }
  fd_table <- fd_table %>% janitor::clean_names() %>%
    clean_format() %>%  type_convert()

  if(any(names(fd_table) == "fantasydata_id"))
    fd_table <- fd_table %>% add_column(id = id_col(fd_table$fantasydata_id, "fantasydata_id"), .before = 1)

  structure(fd_table, source = "FantasyData", week = week, position = position)
}
