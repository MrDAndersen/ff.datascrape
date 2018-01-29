
#' @export
str_to_url <- function(u)httr::build_url(httr::parse_url(u))

#' @export
shark_segment <- function(season, week){
  shark_season <- c("2017"= 586, "2018"=618, "2019" = 650, "2020"=682)

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
    gsub(pattern = "(Percent)(age)*", replacement = "pct", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern = "Per", replacement = "per", ignore.case = TRUE, perl = TRUE) %>%
    gsub(pattern ="(pass|rush|rec)([[:alpha:]])", replacement = "\\1 \\2") %>%
    gsub(pattern ="ydsperatt", replacement = "avg") %>%
    gsub(pattern ="comppct", replacement = "comp pct", ignore.case= TRUE) %>%
    gsub(pattern ="Return", replacement = "ret", ignore.case= TRUE) %>%
    gsub(pattern = "\\b(\\w+)([[:punct:]]|\\s)+\\1\\b", replacement = "\\1", ignore.case = TRUE)
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

#' @export
rate_stat <- function(x, y)ifelse(y != 0, x / y, 0)

#' @export
from_rate <- function(var1, var2, rate)ifelse(is.na(var1) & rate > 0, var2 * rate, var1)

#' @export
nan_zero <- function(x)ifelse(is.nan(x) | is.infinite(x), 0, x)

#' @export
val_from_rate <- function(tbl, var_1, var_2){
  v1 <- enquo(var_1)
  v2 <- enquo(var_2)

  var_tbl <- select(tbl, id, !!v1, !!v2, data_src) %>% filter(!is.na(id))

  miss_var <- var_tbl[!complete.cases(var_tbl),]

  if(nrow(miss_var) > 0){
    tvars <- names(var_tbl)[2:3]
    miss_tbl <- var_tbl[complete.cases(var_tbl),] %>%
      transmute(id, rt = rate_stat(!!v1, !!v2)) %>% group_by(id) %>%
      summarise(rate_var = mean(rt, na.rm = TRUE)) %>%
      inner_join(x=miss_var, by = "id") %>%
      mutate(!!tvars[1] := nan_zero(from_rate(!!v1, !!v2, rate_var)),
             !!tvars[2] := nan_zero(from_rate(!!v2, !!v1, 1/rate_var))) %>%
      select(-rate_var)

    var_tbl <- bind_rows(var_tbl[complete.cases(var_tbl),], miss_tbl)
  }

  return(var_tbl)
}

#' @export
val_from_calc <- function(calc_tbl, stat_tbl, var_1, var_2){
  v1 <- enquo(var_1)
  v2 <- enquo(var_2)

  calc_vars <- paste0("^", paste(setdiff(names(calc_tbl), c("id", "data_src")), collapse = "$|^"), "$")
  stat_tbl <- select(stat_tbl, id, data_src, !!v1)

  if(nrow(stat_tbl[!complete.cases(stat_tbl),]) > 0){
    var_tbl <- calc_tbl %>%
      inner_join(stat_tbl, by = c("id", "data_src")) %>%
      val_from_rate(!!v1, !!v2) %>% select(-matches(calc_vars)) %>%
      right_join(calc_tbl, by = c("id", "data_src"))
  } else {
    var_tbl <- stat_tbl %>% inner_join(calc_tbl, by = c("id", "data_src"))
  }
  return(var_tbl)
}

#' @export
miss_rate <- function(tbl_rate, tbl_raw, grp_var, avg_var){
  fv <- enquo(grp_var)
  av <- enquo(avg_var)

  res <- tbl_raw %>%
    transmute(var_lim = ceiling(!!fv), var_tgt = !!av) %>%
    group_by(var_lim) %>% summarise(avg = mean(var_tgt, na.rm = TRUE)) %>%
    filter(!is.na(var_lim))

  var_name <- quo_name(av)

  res <- tbl_rate %>% mutate(var_lim = ceiling(!!fv)) %>%
    left_join(res, by = "var_lim") %>%
    mutate(!!var_name := ifelse(is.na(!!av), avg, !!av)) %>%
    select(-var_lim, -avg)

  return(res)
}

dist_rate <- function(rate_tbl, stat_tbl, base_var, ...){
  b_var <- enquo(base_var)
  d_var <- quos(...)

  for(i in seq_along(d_var)){
    rate_tbl <- rate_tbl %>% val_from_calc(stat_tbl, !!d_var[[i]], !!b_var)
  }

  return(rate_tbl)
}

get_stat_cols <- function(tbl, match_pattern){
  id_cols <- select(tbl, id, data_src)

  stat_cols <- select(tbl, matches(match_pattern))

  check_cols <- stat_cols %>% is.na() %>% rowSums()

  check_sums <-  stat_cols %>% rowSums(na.rm=TRUE)

  if(length(stat_cols) > 0){
    stat_tbl <- bind_cols(id_cols, stat_cols)
    stat_tbl <- stat_tbl[check_cols < length(stat_cols),]
    stat_tbl <- stat_tbl[check_sums != 0,]
    return(stat_tbl)
  }
  return(data.frame())
}

sum_columns <- function(tbl, ..., na.rm = FALSE){
  sum_vars <- quos(...)
  select(tbl, !!! sum_vars) %>% rowSums(na.rm = na.rm)
}
