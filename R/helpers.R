#' @export
str_to_url <- function(u)httr::build_url(httr::parse_url(u))

#' @export
shark_segment <- function(season, week){
  shark_season <- c("2017"= 586)

  segment <- shark_season[as.character(season)] + week + 9 * (week > 0)
  return(segment)
}

