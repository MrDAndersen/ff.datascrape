#' Scrape data from FantasyPros
#' 
#' Use this function to srape fantasy football projections from FantasyPros
#' @param week The week that data will be scraped for. If omitted, season data 
#' will be scraped.
#' @param position The player position to scrape data for. Has to be one of
#' \code{c("QB", "RB", "WR", "TE", "K", "DST")}. If omitted QB data will be scraped.
#' @export
scrape_fantasypros <- function(week = NULL,
                               position = c("QB", "RB", "WR", "TE", "K", "DST")){

  position <- match.arg(position)
  
  fp_url <- str_to_url(
    sprintf("https://www.fantasypros.com/nfl/projections/%s.php", tolower(position))
  )

  if(is.null(week) || week == 0){
    fp_url <- modify_url(fp_url, query = list(week="draft"))
  } else {
    if(!(week %in% 1:21))
      stop("When specifying a week please only use numbers between 1 and 21", call. = FALSE)
  }

  fp_session <- html_session(fp_url)

  fp_page <- read_html(fp_session)

  fp_table <- fp_page %>% html_node("#data") %>% html_table(header = TRUE)

  if(is.na(as.numeric(fp_table[1, length(fp_table)]))){
    names(fp_table) <- trimws(paste(names(fp_table), fp_table[1,]))
    fp_table <- fp_table[-1,]
  }

  if(position != "DST"){
    fp_table <- fp_table %>%
      extract("Player", c("Player", "Team"), "(.+)\\s([A-Z]{2,3}$)")
  }

  fp_ids <- fp_page %>%
    html_nodes("a.player-name") %>%
    html_attr("href") %>%
    basename() %>% gsub(".php", "", .)


  fp_table <- fp_table %>%  add_column(fantasypro_id = fp_ids, .before = 1)

  fp_table <- fp_table %>%  add_column(Pos = position, .after = "Player")

  if(position %in% c("QB", "RB", "WR", "TE")){
    names(fp_table) <- names(fp_table) %>%
      gsub("MISC FL", "fumbles lost", .) %>%
      gsub("MISC FPTS", "fpts", .) %>%
      offensive_columns()
  }

  if(position == "K"){
    names(fp_table) <- names(fp_table) %>%
      gsub("FGA", "fg_att", .) %>%
      gsub("XPT", "xp", .)
  }

  if(position == "DST"){
    names(fp_table) <- names(fp_table) %>%
      gsub("yds agn", "dst_yds_allow", ., ignore.case = TRUE) %>%
      gsub("pa", "dst_pts_allow", ., ignore.case = TRUE) %>%
      gsub("sack", "dst_sack", ., ignore.case = TRUE) %>%
      gsub("int", "dst_int", ., ignore.case = TRUE) %>%
      gsub("^fr$", "dst_fum_rec", ., ignore.case = TRUE) %>%
      gsub("^ff$", "dst_fum_force", ., ignore.case = TRUE) %>%
      gsub("^td$", "dst_td", ., ignore.case = TRUE) %>%
      gsub("assist", "dst_asst", ., ignore.case = TRUE) %>%
      gsub("safety", "dst_safety", ., ignore.case = TRUE)
  }

  fp_table <- janitor::clean_names(fp_table) %>%
    clean_format() %>%  type_convert()

  if(any(names(fp_table) == "fantasypro_id"))
    fp_table <- fp_table %>% add_column(id = id_col(fp_table$fantasypro_id, "fantasypro_id"), .before = 1)
  
  structure(fp_table, source = "FantasyPros", season = current_season(), week = week, position = position)
}
