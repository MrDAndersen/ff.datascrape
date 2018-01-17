
#' @export
str_to_url <- function(u)httr::build_url(httr::parse_url(u))

#' @export
shark_segment <- function(season, week){
  shark_season <- c("2017"= 586)

  segment <- shark_season[as.character(season)] + week + 9 * (week > 0)
  return(segment)
}


#' @export
offensive_columns <- function(tbl_columns){
  tbl_columns <- tbl_columns %>%
    gsub(pattern = "(Rush|Pass|Rec)(ing|eiving)", replacement = "\\L\\1", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "(TD|YD)$", replacement = "\\L\\1s",  ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "^ru*sh$", replacement = "rush att", ignore.case = TRUE) %>%
    gsub(pattern = "^int$", replacement = "pass int", ignore.case = TRUE) %>%
    gsub(pattern = "rush$", replacement = "att", ignore.case = TRUE) %>%
    gsub(pattern = "Cmp", replacement = "Comp", ignore.case = TRUE,  perl = TRUE) %>%
    gsub(pattern = "(Att)(empt)*s*", replacement = "\\L\\1",  ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "(Comp)(letion)*s*", replacement = "\\L\\1", ignore.case = TRUE,  perl = TRUE) %>%
    gsub(pattern = "(Y)a*r*(d)s*", replacement = "\\L\\1\\L\\2s", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "(T)(ouch)*(d)(own)*s*", replacement = "\\L\\1\\L\\3s", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "(Rec)e*p*t*(ion)*s*$", replacement = "\\L\\1", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "(Int)(erception)*s*$", replacement = "\\L\\1", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "\\b(\\w+)([[:punct:]]|\\s)+\\1\\b", replacement = "\\1", ignore.case = TRUE) %>%
    gsub(pattern = "(Percent)(age)*", replacement = "pct", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "Per", replacement = "per", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern ="(pass|rush|rec)([[:alpha:]])", replacement = "\\1 \\2") %>%
    gsub(pattern ="ydsperatt", replacement = "avg") %>%
    gsub(pattern ="comppct", replacement = "comp pct", ignore.case= TRUE) %>%
    gsub(pattern ="Return", replacement = "ret", ignore.case= TRUE)

  return(tbl_columns)
}
#' @export
clean_format <- function(df){
  formatted_num <- intersect(names(df), c("pass_yds", "rush_yds", "rec_yds"))
  remove_format <- function(x)gsub("\\,", "", x)
  if(length(formatted_num) > 0)
    df <- df %>% mutate_at(formatted_num, remove_format)
  return(df)
}

id_col <- function(x, match_col){
  player_ids$id[match(x, player_ids[[match_col]])]
}

current_season <- function(){
  cur_year <- as.numeric(format(Sys.Date(), "%Y"))
  cur_month <- as.numeric(format(Sys.Date(), "%m"))

  if(cur_month == 1)
    return(cur_year - 1)
  else
    return(cur_year)

}


clean_pname <- function(x){
  gsub("[J|S]r\\.*$|[[:punct:]]|\\s",  "", x)
}


match_by_col <- function(x, y, match_col, id_vars){
  x_col <- x[[match_col]]
  y_col <- y[[match_col]]

  x_dups <- x_col[duplicated(x_col)]
  y_dups <- y_col[duplicated(y_col)]

  val_match <- intersect(x_col[!(x_col %in% x_dups)], y_col[!(y_col %in% y_dups)])

  xy_match <- inner_join(x[x[[match_col]] %in% val_match, c(match_col, id_vars[1])],
                         y[y[[match_col]] %in% val_match, c(match_col, id_vars[2])],
                         by = match_col) %>% select(id_vars)
  return(xy_match)
}


match_players <- function(x){
  x <- mutate(x, pos = recode(pos, !!!pos_corrections), team = recode(team, !!!team_corrections))
  p_tbl <- mutate(ff_player_data, position = recode(position, !!!pos_corrections),
                  team = recode(team, !!!team_corrections))

  match_pos <- unique(x$pos)

  p_tbl <- filter(p_tbl, position %in% match_pos) %>%
    mutate(match_name = tolower(clean_pname(recode(name, !!!name_corrections ))),
           match_name_pos = paste(match_name, tolower(position), sep = "-"),
           match_name_pos_tm = paste(match_name_pos, tolower(team), sep = "-"))

  x <- x %>%
    mutate(match_name = tolower(clean_pname(recode(player, !!!name_corrections ))),
           match_name_pos = paste(match_name, tolower(pos), sep = "-"),
           match_name_pos_tm = paste(match_name_pos, tolower(team), sep = "-"))

  x <- add_column(x, tmp_id = 1:nrow(x))

  matched <- data.frame(tmp_id=as.integer(NA), id = as.character(NA), stringsAsFactors = FALSE)[-1,]

  for(col in c("match_name_pos_tm", "match_name_pos", "match_name")){
    x_tbl <- filter(x, !(x$tmp_id %in% matched$tmp_id))
    y_tbl <- filter(p_tbl, !(p_tbl$id %in% matched$id))
    match_ids <- match_by_col(x_tbl, y_tbl, col, c("tmp_id", "id"))
    matched <- bind_rows(list(matched, match_ids))
  }

  return(matched$id[match(x$tmp_id, matched$tmp_id)])
}



