#' @export
scrape_data <- function(season = NULL, week = 0,
                        position = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB"),
                        sources = c("CBS", "Yahoo", "ESPN", "FantasyData", "FantasyPros",
                                    "FantasySharks", "FFToday", "FleaFlicker", "NFL",
                                    "NumberFire", "RTSports", "Walterfootball")){

  if(is.null(week))
    week <- 0

  season_only <- c("RTSports", "Walterfootball")

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

  return(source_result)
}


