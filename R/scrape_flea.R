#' Scrape data from FleaFlicker
#'
#' Use this function to srape fantasy football projections from FleaFlicker
#' @param week The week that data will be scraped for. If omitted, season data
#' will be scraped.
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "Flex", "K", "DST", "DB", "DL", "LB", "IDP")}.
#' If omitted QB data will be scraped. Specifying \code{"Flex"} will scrape RB,
#' WR, and TE data. Specifying \code{"IDP"} will scrape DB, DL, and LB data.
#'
#' @export
scrape_fleaflick <- function(week = NULL,
  position = c("QB", "RB", "WR", "TE", "Flex", "K", "DST", "DB", "DL", "LB", "IDP")
  )
{
  position <- match.arg(position)

  if(is.null(week))
    week <- 0

  if(week != 0 & !(week %in% 1:21))
    stop("When specifying a week please only use numbers between 1 and 21", call. = FALSE)

  flea_positions = c("QB" = 4, "RB" = 1, "WR" = 2, "TE" = 8, "Flex" = 11, "K" = 16,
                     "DST" = 256, "DB" = 32, "DL" = 64, "LB" = 128, "IDP" = 224)

  flea_base <- str_to_url("https://www.fleaflicker.com/nfl/leaders")
  sort_mode <- ifelse(week < 18, 1, 7)
  flea_qry <- list(statType = 7, sortMode = sort_mode)

  flea_qry$position <- flea_positions[[position]]
  flea_qry$tableOffset <- 0

  flea_url <- modify_url(flea_base, query = flea_qry)

  flea_data <- data.frame()

  flea_session <- html_session(flea_url)
  repeat({

    flea_page <- read_html(flea_session)

    flea_table <- flea_page %>%
      html_node("#body-center-main table") %>%
      html_table()

    names(flea_table) <- trimws(paste(gsub("^Projected |\\sWeek [0-9]+$|Wild Card$|Divisional$|Conference$", "", names(flea_table)),
                               flea_table[1,]))

    flea_table <- flea_table[-1,]

    flea_table <- flea_table %>% repair_names(prefix = "")

    del_cols <- intersect(names(flea_table), as.character(1:9))

    flea_table <- flea_table %>% select(-one_of(del_cols))

    if(position == "K"){
      names(flea_table) <- gsub("(\\%|Att)$", "FG \\1", names(flea_table))
      names(flea_table) <- gsub("(\\%|Att)1$", "XP \\1", names(flea_table))
      names(flea_table) <- gsub("%", "Pct", names(flea_table))
      names(flea_table) <- gsub("Kicking\\s", "", names(flea_table))
    }

    flea_table[, 1] <- gsub("^(Q|D|OUT|SUS|IR)([A-Z])", "\\2", flea_table[, 1])

    if(length(grep( "Next", flea_table[,1]) >0))
      flea_table <- flea_table[1:(nrow(flea_table) - 1),]

    flea_table <- flea_table %>%
      extract("Player Name", c("Player", "Pos", "Team", "Bye"),
              "([A-Za-z0-9'-.\\s]+)\\s([QRWTDKL][BRESL/]*[SFT]*[T]*)\\s([ABCDFGHIJKLMNOPSTW][ACIOUHFTELNYBR][TLCFIUSRNEDGJKA]*)\\s*\\(*([0-9]*)\\)*"
      )

    player_id <- flea_page %>%
      html_nodes("a.player-text") %>%
      html_attr("href") %>%
      str_extract("[0-9]{3,}$")

    flea_table <- flea_table %>% add_column(fleaflicker_id = player_id, .before =1)

    flea_data <- bind_rows(flea_data, flea_table)

    next_url <- flea_page %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")

    if(is.na(next_url))
      break

    flea_session <- flea_session %>% jump_to(next_url)
  })

  names(flea_data) <- names(flea_data) %>%
    gsub("Fantasy FPts", "Pts", .) %>%
    gsub("Availability ", "", .) %>%
    gsub("Week [0-9]+ ", "", .)

  flea_data$Pts <- as.numeric(flea_data$Pts)

  if(position %in% c("QB", "RB", "WR", "TE", "Flex")){
    names(flea_data) <- names(flea_data) %>%
      gsub("Rat$", "rate", .) %>%
      gsub("%$", "comp_pct", .) %>%
      gsub("Tar$", "tgt", .) %>%
      gsub("Misc Fum", "fumbles lost", .) %>%
      gsub("Misc", "Return", .) %>%
      offensive_columns()
  }

  if(position == "DST"){
    names(flea_data) <- names(flea_data) %>%
      gsub("defense", "dst", ., ignore.case = TRUE) %>%
      gsub("ff$", "fum_force", ., ignore.case = TRUE) %>%
      gsub("fr$", "fum_rec", ., ignore.case = TRUE) %>%
      gsub("ff$", "fum_force", ., ignore.case = TRUE) %>%
      gsub("dst pts", "dst_pts_allow", ., ignore.case = TRUE) %>%
      gsub("dst yd", "dst_yds_allow", ., ignore.case = TRUE)
  }

  if(position %in% c("DB", "DL", "LB", "IDP")){
    names(flea_data) <- names(flea_data) %>%
      gsub("defense", "idp", ., ignore.case = TRUE) %>%
      gsub("ast$", "asst", ., ignore.case = TRUE) %>%
      gsub("ff$", "fum_force", ., ignore.case = TRUE) %>%
      gsub("fr$", "fum_rec", ., ignore.case = TRUE) %>%
      gsub("ff$", "fum_force", ., ignore.case = TRUE)
  }

  flea_data <- janitor::clean_names(flea_data) %>%
    clean_format() %>%  type_convert()

  if(position == "QB" & any(names(flea_data) == "pass_comp_pct")){
    flea_data <- flea_data %>%
      mutate(pass_comp_pct = sapply(pass_comp_pct, function(x)eval(parse(text=x))*100, USE.NAMES=FALSE))
  }

  if(position == "K"){
    flea_data <- flea_data %>%
      mutate(fg_pct = sapply(fg_pct, function(x)eval(parse(text=x)), USE.NAMES=FALSE),
             xp_pct = sapply(xp_pct, function(x)eval(parse(text=x)), USE.NAMES=FALSE))
  }

  if(any(names(flea_data) =="fpts"))
    flea_data$fpts <- as.numeric(flea_data$fpts)

  if(any(names(flea_data) == "fleaflicker_id"))
    flea_data <- flea_data %>% add_column(id = id_col(flea_data$fleaflicker_id, "fleaflicker_id"), .before = 1)

  structure(flea_data, source = "FleaFlicker", position = position, season = current_season(), week = week)
}

