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

yahoo_def <- c("jac" = "30", "bal" = "33", "lar" = "14", "phi" = "21", "det" = "8",
               "lac" = "24", "nor" = "18", "sea" = "26", "chi" = "3",  "car" = "29",
               "pit" = "23", "nwe" = "17",  "kan" = "12", "min" = "16", "dal" = "6",
               "was" = "28", "den" = "7", "ari" = "22", "ten" = "10", "tam" = "27",
               "buf" = "2", "cin" = "4", "atl" = "1", "gnb" = "9", "mia" = "15",
               "ind" = "11", "nyg" = "19",  "hou" = "34", "sfo" = "25", "cle" = "5",
               "nyj" = "20", "oak" = "13")


nflTeam.abb <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
                 "DAL", "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC",
                 "MIA", "MIN", "NO",  "NE",  "NYG", "NYJ", "PHI", "PIT",
                 "LA",  "SF",  "LAC", "TB",  "TEN", "WAS", "SEA", "OAK")

nflTeam.id <- c("100026", "100001", "100002", "100003", "100004", "100005", "100006", "100007",
                "100008", "100009", "100010", "100011", "100013", "100014", "100015", "100016",
                "100019", "100020", "100022", "100021", "100023", "100024", "100025", "100027",
                "100017", "100029", "100028", "100031", "100012", "100032", "100030", "100018")

cbs_def <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
             "DET", "GB", "HOU", "IND", "JAC", "KC",  "LAC", "LAR", "MIA", "MIN",
             "NE" , "NO",  "NYG", "NYJ", "OAK", "PHI", "PIT", "SEA", "SF",  "TB",
             "TEN", "WAS")

cbs_def_id <- c("1901", "1902", "1903", "1904", "1905", "1906", "1907", "1930", "1908", "1909",
                "1910", "1911", "1932", "1912", "1913", "1914", "1924", "1923", "1915", "1916",
                "1931", "1917", "1918", "1919", "1920", "1921", "1922", "1926", "1925", "1927",
                "1928", "1929")
