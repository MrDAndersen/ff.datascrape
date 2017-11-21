#' @export
str_to_url <- function(u)httr::build_url(httr::parse_url(u))