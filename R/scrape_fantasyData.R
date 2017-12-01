#' @export
scrape_fantasydata <- function(week = -1, position = c("QB", "RB", "WR", "TE", "K", "DST")){

  fd_positions = c("QB" = 1, "RB" = 2, "WR" = 3, "TE"= 4, "K" = 5, "DST"= 6)

  fd_base <- str_to_url("https://fantasydata.com/nfl-stats/fantasy-football-weekly-projections.aspx")

  fd_qry <- list(fs = 0, stype = 0, sn = 0, scope = 1, w = -1, ew = -1, s = "",
                 t = 0, p = 9, st="FantasyPoints", d = 1, ls = "", live="false",
                 pid="true", minsnaps=4)

  if(week > 0){
    fd_qry$w <- week
    fd_qry$ew <- week
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
    gsub("(^.+','Sort\\$)(.+)('\\))", "\\2", .)

  return(fd_table)
}
