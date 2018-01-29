#' Scrape data from FFToday
#'
#' Use this function to srape fantasy football projections from FFToday
#' @param season The year that data will be scraped for. If ommitted the current
#' season will be used
#' @param week The week that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")}. If omitted QB
#' data will be scraped. Note that weekly data for DST and IDP positions are not
#' available.
#' @export
scrape_fftoday <- function(
  season = NULL, week = NULL,
  position = c("QB", "RB", "WR", "TE", "K", "DST", "DL", "LB", "DB")
)
{
  fft_positions <- c(QB = 10, RB = 20, WR = 30, TE = 40, K = 80,
                     DST = 99, DL = 50, LB = 60, DB =70)

  position <- match.arg(position)

  fft_base <- httr:: build_url(
    httr::parse_url("http://www.fftoday.com/rankings/")
  )

  if(is.null(season))
    season <- current_season()

  if(!is.null(week) && week != 0){
    if(!(week %in% 1:21))
      stop("When specifying a week please only use numbers between 1 and 21", call. = FALSE)
  }

  if(season > current_season()){
    stop("Invalid season. Please specify ", current_season(), " or earlier", call. = FALSE)
  }

  fft_qry <- list(Season = season)

  fft_file <- as.character()

  if(is.null(week) || week == 0){
    fft_file <- "playerproj.php"
  } else {
    fft_file <- "playerwkproj.php"
    if(week > 0 & position %in% c("DST", "DL", "LB", "DB"))
      message("NOTE:\tFFToday does not provide weekly defensive projections.\n",
              "\tTable is empty.")
  }

  fft_path <- paste0(httr::parse_url(fft_base)$path, fft_file)

  if(!is.null(week) && week > 0)
    fft_qry$GameWeek <- week + ifelse(week > 17, 3, 0)

  fft_qry$LeagueID <- 1
  fft_qry$PosID <- fft_positions[[position]]
  fft_qry$cur_page <- 0

  fft_url <- httr::modify_url(fft_base, path = fft_path, query = fft_qry)

  fft_data <- scrape_html_data(fft_url)

  if(position %in% c("QB", "RB", "WR", "TE"))
     names(fft_data) <- offensive_columns(names(fft_data))

  if(position == "K"){
    names(fft_data) <- names(fft_data) %>%
      gsub("^FG$", "FG Pct", .) %>%
      gsub("^EP", "XP", .) %>%
      gsub("Made", "", .) %>%
      gsub("Miss", " Miss", .) %>%
      gsub("^FGM$", "fg", .) %>%
      gsub("^FGA$", "fg att", .) %>%
      gsub("^XPM$", "xp", .) %>%
      gsub("^XPA$", "xp att", .)
  }

  if(position == "DST"){
    names(fft_data) <- names(fft_data) %>%
      gsub("(Sack|INT|Safety)", "dst \\1", .) %>%
      gsub("FR", "dst fum rec", .) %>%
      gsub("DefTD", "dst td", .) %>%
      gsub("PA", "dst pts_Allow", .) %>%
      gsub("PaYdG", "dst pass yds game", .) %>%
      gsub("RuYdG", "dst rush yds game", .) %>%
      gsub("KickTD", "dst kick ret td", .)

    print(names(fft_data))
    fft_data <- add_column(fft_data,
               id = ff_player_data$id[match(fft_data$Team, ff_player_data$name)],
               .before = 1)
  }

  if(position %in% c("DL", "LB", "DB")){
    fft_data <- fft_data %>% rename(fum_force = "FF", fum_rec = "FR", solo = "Tackle", asst = "Assist")

    names(fft_data) <- names(fft_data) %>%
      gsub("(solo|asst|SACK|PD|int|fum_force|fum_rec)", "idp_\\1", ., ignore.case = TRUE ) %>%
      gsub("Fantasy ", "", . , ignore.case = TRUE)
  }

  fft_data <- ff_clean_names(fft_data)

  structure(fft_data, source = "FFToday", season = season, week = week, position = position)
}

