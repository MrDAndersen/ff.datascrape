#' Helper objects for data sources
#'
#' \describe{
#'   \item{\code{scrape_func}}{A list with the names of the functions used to
#'   scrape data from a source}
#'   \item{\code{season_only_sources}}{Vector with names of sources that only
#'   provide seasonal data}
#'   \item{\code{playoff_sources}}{Vector with names of sources that
#'   provide data for playoffs (week 17-20)}
#' }
#'
#' @include scrape_cbs.R scrape_yahoo.R scrape_espn.R scrape_fantasyData.R
#'   scrape_fantasypros.R scrape_fantasyshark.R scrape_fftoday.R scrape_flea.R
#'   scrape_nfl.R scrape_numfire.R scrape_rts.R scrape_wf.R
#' @name data_sources
#' @format NULL
#' @usage NULL
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

#' @rdname data_sources
#' @usage NULL
#' @format NULL
season_only_sources <- c("RTSports", "Walterfootball")

#' @rdname data_sources
#' @usage NULL
#' @format NULL
playoff_sources <- c("CBS", "FantasyData", "FantasyPros", "FantasySharks",
                     "FFToday", "FleaFlicker", "NumberFire")

check_sources <- function(week, sources){
  if(week > 0 & any(sources %in% season_only_sources)){
    season_src <- intersect(sources, season_only_sources)
    warning(paste(season_src, collapse = ", "),
            " does not have weekly data and will not be scraped.", call. = FALSE)

    sources <- setdiff(sources, season_only_sources)
  }

  non_playoff <- setdiff(sources, playoff_sources)
  if(week > 17 & length(non_playoff) > 0){
    warning(paste(non_playoff, collapse = ", "),
            " does not have playoff data and will not be scraped.", call. = FALSE)

    sources <- intersect(sources, playoff_sources)
  }
  return(sources)
}

get_src_data <- function(season, week, position, sources){
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

    if(any(names(src_formals) =="position")){
      src_result <- lapply(src_pos, function(p){
        src_args$position = p
        res <- do.call(scrape_func[[src]], src_args)
        if(nrow(res)>0)
          res$data_src <- src
        if(any(grepl("_id$", names(res)))){
          id_column <- names(select(res, ends_with("_id")))
          names(id_column) <- "src_id"
          res <- rename(res, !!!id_column) %>% mutate(src_id = as.character(src_id))
        }
        return(res)
      })

      names(src_result) <- src_pos

      if(any(src_pos == "IDP")){
        idp_res <- src_result$IDP
        src_result <- append(src_result, split(idp_res, idp_res$pos))
        src_result$IDP <- NULL
      }
    } else {
      src_result <- do.call(scrape_func[[src]], src_args)

      src_result <- lapply(src_result, function(t){
        t$data_src <- src
        if(!any(grepl("_id$", names(t))))
          t$src_id <- NA
        return(t)
      })
    }

    return(src_result)
  })

  names(source_result) <- sources


  result_by_pos <- lapply(position, function(p){
    p_data <- bind_rows(lapply(source_result, `[[`, p))
    p_cols <- select(p_data, one_of(c("id", "src_id", "data_src")))
    stat_cols <- select(p_data, matches("^pass|^rush|^rec|^fumb|^xp|^fg|^dst|^idp|^two|^sack|^ret|^reg"))
    return(bind_cols(p_cols, stat_cols))
  })
  names(result_by_pos) <- position
  return(result_by_pos)
}
