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
    gsub(pattern ="comppct", replacement = "comp pct", ignore.case= TRUE)

  return(tbl_columns)
}
