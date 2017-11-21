#' @export
scrape_fantasydata <- function(week = -1, position = c("QB", "RB", "WR", "TE", "K", "DST")){

  fd_positions = c("QB" = 1, "RB" = 2, "WR" = 3, "TE"= 4, "K" = 5, "DST"= 6)  
  
  fd_base <- str_to_url("https://fantasydata.com/nfl-stats/fantasy-football-weekly-projections.aspx")
  
  fd_qry <- list(fs = 0, stype = 0, sn = 0, scope = 1, w = -1, ew = -1, s = "", t = 0, p = 9, 
                 st="FantasyPoints", d = 1, ls = "", live="false", pid="true", minsnaps=4)
  
  if(week > 0){
    fd_qry$w <- week
    fd_qry$ew <- week
  }
  
  fd_qry$p <- fd_positions[[position]]
  
  fd_url <- httr::modify_url(fd_base, query = fd_qry)
  
  fd_page <-RCurl::getURL(fd_url)
  
  
  fd_table <- XML::readHTMLTable(fd_page, which=1, stringsAsFactors = FALSE)
  th_list <- XML::getNodeSet(XML::htmlParse(fd_page), "//table//tr//th")
  
  td_list <-  lapply(th_list, XML::xmlChildren)
  
  a_list <- unlist(lapply(td_list, function(td)td[which(names(td) == "a")]))
  
  col_href <-unlist(lapply(a_list, XML::xmlGetAttr, name = "href"), use.names = FALSE)
  
  col_names <- gsub("[\\$'\\)]", "",  stringr::str_extract(col_href,"\\$[A-Za-z]+'\\)$" ))
  
  names(fd_table) <- c("Rank", col_names)
  
  return(data.table::as.data.table(fd_table))
}