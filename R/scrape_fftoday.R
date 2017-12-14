#' Scrape data from FFToday
#'
#' @export
scrape_fftoday <- function(
  season = NULL, week = NULL,
  position = c("QB", "RB", "WR", "TE", "K", "DEF", "DL", "LB", "DB")
)
{
  fft_positions <- c(QB = 10, RB = 20, WR = 30, TE = 40, K = 80,
                     DEF = 99, DL = 50, LB = 60, DB =70)

  position <- match.arg(position)

  fft_base <- httr:: build_url(
    httr::parse_url("http://www.fftoday.com/rankings/")
  )

  if(is.null(week) & is.null(season))
    stop("Please provide season and/or week to scrape for", call. = FALSE)

  if(!is.null(week) & is.null(season))
    stop("Please provide season for which to scrape the weekly data for", call. = FALSE)


  if(!is.null(week)){
    week <- as.character(week)
    week <- match.arg(week, choices = 1:21)
  }

  fft_qry <- list(Season = season)


  fft_file <- as.character()

  if(is.null(week)){
    fft_file <- "playerproj.php"
  } else {
    fft_file <- "playerwkproj.php"
    if(week > 0 & position %in% c("DEF", "DL", "LB", "DB"))
      message("NOTE:\tFFToday does not provide weekly defensive projections.\n",
              "\tTable is empty.")
  }


  fft_path <- paste0(httr::parse_url(fft_base)$path, fft_file)

  if(!is.null(week))
    fft_qry$GameWeek <- week

  fft_qry$LeagueID <- 1
  fft_qry$PosID <- fft_positions[[position]]
  fft_qry$cur_page <- 0

  fft_url <- httr::modify_url(fft_base, path = fft_path, query = fft_qry)

  fft_session <- rvest::html_session(fft_url)

  fft_data <- data.frame()

  repeat({

    fft_page <- xml2::read_html(fft_session)

    fft_table <- rvest::html_table(rvest::html_nodes(fft_page, "table")[[11]], header = TRUE)

    names(fft_table) <- trimws(
      paste(names(fft_table), gsub("[^[:alpha:]]|Sort|First:|Last:", "", fft_table[1,])
      ))

    fft_table <- fft_table[-1,]

    player_links <- rvest::html_attr(rvest::html_nodes(fft_page, "a[href *='stats/players/']"), "href")

    player_id <- stringr::str_extract(player_links, "[0-9]{2,6}")

    if(length(player_id) == nrow(fft_table))
      fft_table <- tibble::add_column(fft_table, id = player_id, .before = 1)
    
    if(!any(names(fft_table) == "Player"))
      fft_table <- fft_table %>% rename(Player = Team)
    
    fft_table <- tibble::add_column(fft_table, Pos =  position, .after =  which(names(fft_table) == "Player"))

    fft_data <- dplyr::bind_rows(fft_data, fft_table)

    next_url <- rvest::html_attr(rvest::html_node(fft_page, "a:contains('Next')"), "href")

    if(is.na(next_url))
      break

    fft_session <- rvest::jump_to(fft_session, next_url)

  })
  
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
  
  if(position == "DEF"){
    names(fft_data) <- names(fft_data) %>%
      gsub("(Sack|INT|Safety)", "dst \\1", .) %>%
      gsub("FR", "dst fum rec", .) %>%
      gsub("DefTD", "dst tds", .) %>%
      gsub("PA", "dst pts_Allow", .) %>%
      gsub("PaYdG", "dst pass yds game", .) %>%
      gsub("RuYdG", "dst rush yds game", .) %>%
      gsub("KickTD", "dst kick ret td", .)
  }
  

  fft_data <- fft_data %>% janitor::clean_names()
  structure(fft_data, source = "FFToday", season = season, week = week, position = position)
}

