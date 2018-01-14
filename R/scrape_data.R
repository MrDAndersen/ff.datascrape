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

  season_only <- c("RTSports", "Walterfootball")

  sources <- match.arg(sources, several.ok = TRUE)

  playoffs <- c("CBS", "FantasyData", "FantasyPros", "FantasySharks",
                     "FFToday", "FleaFlicker", "NumberFire")

  if(week > 0 & any(sources %in% season_only)){
    season_src <- intersect(sources, season_only)
    warning(paste(season_src, collapse = ", "),
            " does not have weekly data and will not be scraped.", call. = FALSE)

    sources <- setdiff(sources, season_only)
  }

  non_playoff <- setdiff(sources, playoffs)
  if(week > 17 & length(non_playoff) > 0){
       warning(paste(non_playoff, collapse = ", "),
            " does not have playoff data and will not be scraped.", call. = FALSE)

    sources <- intersect(sources, playoffs)
  }


  if(any(position == "DST"))
    position <- c(position, "DEF")

  scrape_func = list("CBS" = scrape_cbs,
                     "Yahoo" = scrape_yahoo,
                     "ESPN" = scrape_espn,
                     "FantasyData" = scrape_fantasydata,
                     "FantasyPros" = scrape_fantasypros,
                     "FantasySharks" = scrape_fantasysharks,
                     "FFToday" = scrape_fftoday,
                     "FleaFlicker" = scrape_fleaflick,
                     "NFL" = scrape_nfl,
                     "NumberFire" = scrape_numfire,
                     "RTSports" = scrape_rts,
                     "Walterfootball" = scrape_wf)

  source_result <- lapply(sources, function(src){
    src_formals <- formals(scrape_func[[src]])

    src_pos <- intersect(eval(src_formals$position), position)
    if(src == "NumberFire" & any(position %in% c("DL", "LB", "DB")))
      src_pos <- c(src_pos, "IDP")

    if(src == "FFToday" & any(position %in% c("DL", "LB", "DB")) & week > 0)
      src_pos <- setdiff(src_pos,c("DL", "LB", "DB"))

    src_args <- list()

    if(any(names(src_formals) =="season"))
      src_args$season <- season
    if(any(names(src_formals) =="week"))
      src_args$week <- week

    src_result <- lapply(src_pos, function(p){
      src_args$position = p
      res <- do.call(scrape_func[[src]], src_args)
      res$data_src <- src
      id_column <- names(select(res, ends_with("_id")))
      names(id_column) <- "src_id"
      res <- rename(res, !!!id_column) %>% mutate(src_id = as.character(src_id))

      return(res)
    })

    names(src_result) <- src_pos

    return(src_result)
  })

  names(source_result) <- sources

  result_by_pos <- lapply(position, function(p){
    p_data <- bind_rows(lapply(source_result, `[[`, p))
    p_cols <- select(p_data, one_of(c("id", "src_id", "data_src")))
    stat_cols <- select(p_data, matches("^pass|^rush|^rec|^fumb|^xp|^fg|^dst|^idp|^two|^sack|^ret"))
    return(bind_cols(p_cols, stat_cols))
  })
  names(result_by_pos) <- position
  return(result_by_pos)
}


