#' Scrape data from FantasySharks
#'
#' Use this function to srape fantasy football projections from FantasySharks
#' @param season The year that data will be scraped for. If ommitted the current
#' season will be used
#' @param week The week that data will be scraped for. If \code{= 0} or omitted
#' season data will be scraped
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#' @export
scrape_fantasysharks <- function(season = NULL,  week = NULL,
                                 position =c("QB", "RB", "WR", "TE", "K", "DST",
                                             "DL", "LB", "DB")){

  position <- match.arg(position)

  if(is.null(week))
    week <- 0

  if(is.null(season))
    season <- current_season()

  if(season > current_season()){
    stop("Invalid season. Please specify ", current_season(), " or earlier", call. = FALSE)
  }

  fs_pos <- c("QB" = 1, "RB" =2 , "WR" = 4, "TE" = 5, "K" = 7, "DST" = 6, "DL" = 8, "LB"=9, "DB" = 10)

  fs_base <- str_to_url("https://www.fantasysharks.com/apps/bert/forecasts/projections.php")

  fs_qry <- list(League=-1, scoring=1, uid=4)

  fs_qry$Segment <- shark_segment(season, week)

  fs_qry$Position <- fs_pos[position]

  fs_url <- modify_url(fs_base, query = fs_qry)

  fs_table <- scrape_html_data(fs_url)

  if(any(names(fs_table) %in% c(">= 50 yd1", ">= 100 yd1" ))){
    fs_table <- fs_table %>%
      rename("Rush 50 Yds" = ">= 50 yd", "Rush 100 yds" = ">= 100 yd",
             "Rec 50 Yds" = ">= 50 yd1", "Rec 100 yds" = ">= 100 yd1")
  }

  if(any(names(fs_table) %in% c(">= 50 yd", ">= 100 yd" ))){
    fs_table <- fs_table %>%
      rename("Rec 50 Yds" = ">= 50 yd", "Rec 100 yds" = ">= 100 yd")
  }

  if(any(names(fs_table) %in% c(">= 150 yd", ">= 200 yd" ))){
    if(position == "RB"){
      fs_table <- fs_table %>%
        rename("Rush 150 Yds" = ">= 150 yd", "Rush 200 yds" = ">= 200 yd")
    }

    if(position == "WR"){
      fs_table <- fs_table %>%
        rename("Rec 150 Yds" = ">= 150 yd", "Rec 200 yds" = ">= 200 yd")
    }
  }

  if(position %in% c("QB", "RB", "WR", "TE", "K")){
    names(fs_table) <- names(fs_table) %>%
      gsub(pattern = "(^att$|^comp$)", replacement = "pass \\1", ignore.case = TRUE) %>%
      gsub("([0-9]{1,2}\\-*\\+*[0-9]{0,2})\\s(Rsh|Rec|Pass|FGM)\\s*(TDs)*", "\\2 \\3 \\1", .) %>%
      gsub("  ", " ",.) %>%
      gsub("Miss", "fg Miss", .) %>%
      gsub("^FGM", "fg", .) %>%
      gsub("^FGA$", "fg att", .) %>%
      gsub("^XPM$", "xp", .) %>%
      gsub("^XPA$", "xp att", .) %>%
      gsub("Fum", "fumbles lost", .) %>%
      gsub("Sck", "sacks", .) %>%
      gsub("[[:punct:]]", "", .) %>%
      gsub("Opp1", "scoring_opp", .) %>%
      gsub("tgt", "rec_tgt", ., ignore.case = TRUE) %>%
      gsub("Rsh", "Rush", ., ignore.case = TRUE) %>%
      offensive_columns()
  }

  if(position == "DST"){
    names(fs_table) <- names(fs_table) %>%
      gsub("yds allowed", "dst_yds_allow", ., ignore.case = TRUE) %>%
      gsub("pts agn", "dst_pts_allow", ., ignore.case = TRUE) %>%
      gsub("scks", "dst_sack", ., ignore.case = TRUE) %>%
      gsub("int", "dst_int", ., ignore.case = TRUE) %>%
      gsub("fum", "dst_fum_rec", ., ignore.case = TRUE) %>%
      gsub("deftd", "dst_td", ., ignore.case = TRUE) %>%
      gsub("safts", "dst_safety", ., ignore.case = TRUE)
  }

  if(position %in% c("DL", "LB", "DB")){
    names(fs_table) <- names(fs_table) %>%
      gsub("tack", "idp_solo", ., ignore.case = TRUE) %>%
      gsub("asst", "idp_asst", ., ignore.case = TRUE) %>%
      gsub("scks", "idp_sack", ., ignore.case = TRUE) %>%
      gsub("int", "idp_int", ., ignore.case = TRUE) %>%
      gsub("fumfrc", "idp_fum_force", ., ignore.case = TRUE) %>%
      gsub("^fum$", "idp_fum_rec", ., ignore.case = TRUE) %>%
      gsub("deftd", "idp_td", ., ignore.case = TRUE) %>%
      gsub("passdef", "idp_pd", ., ignore.case = TRUE)
  }

  fs_table <- ff_clean_names(fs_table) %>% rename(team=tm)

  structure(fs_table, source = "FantasySharks", season = season, week = week, position = position)
}
