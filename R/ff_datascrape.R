.onLoad <- function(libname, pkgname){
  op <- options()
  op.ffdata <- list(
    ffdata.yahoo_league = NULL,
    ffdata.ffn_api = "test"
  )
  toset <- !(names(op.ffdata) %in% names(op))
  if(any(toset)) options(op.ffdata[toset])

  invisible()
}
