#' Scrape data from multiple sources
#'
#' This function will scrape data from multiple sources.
#' \describe{
#'    \item{Season data: }{CBS, Yahoo, ESPN, FantasyData, FantasyPros,
#'    FantasySharks, FFToday, FleaFlicker, NFL, NumberFire, RTSports, Walterfootball}
#'    \item{Weekly data (regular seson): }{CBS, Yahoo, ESPN, FantasyData, FantasyPros,
#'    FantasySharks, FFToday, FleaFlicker, NFL, NumberFire}
#'    \item{Weekly data (post seson): }{CBS, FantasyData, FantasyPros,
#'    FantasySharks, FFToday, FleaFlicker, NumberFire}
#' }
#' @param season The season data should be scraped for. If omitted then current
#' season will be scraped
#' @param week The week data should be scraped for. If omitted the season data will
#' be scraped
#' @param position The position(s) to scrape for. Select positions from:
#' \code{c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")}. Multiple
#' postions are allowed.
#' @param sources The sources to scrape for. Select sources from
#' \code{c("CBS", "Yahoo", "ESPN", "FantasyData", "FantasyPros", "FantasySharks",
#' "FFToday", "FleaFlicker", "NFL","NumberFire", "RTSports", "Walterfootball")}.
#' Multiple sources are allowed.
#' @return A list with one data.frame by position.
#' @export
scrape_data <- function(season = NULL, week = 0,
                        position = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
                        sources = c("CBS", "Yahoo", "ESPN", "FantasyData", "FantasyPros",
                                    "FantasySharks", "FFToday", "FleaFlicker", "NFL",
                                    "NumberFire", "RTSports", "Walterfootball")){

  if(is.null(week))
    week <- 0

  if(is.null(season))
    season <- current_season()

  position <- match.arg(position, several.ok = TRUE)
  sources <- match.arg(sources, several.ok = TRUE)

  season_only <- season_only_sources
  playoffs <- playoff_sources

  sources <- check_sources(week, sources)

  return(get_src_data(season, week, position, sources))

}


