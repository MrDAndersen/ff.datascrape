#' Scrape data from WalterFootball
#' 
#' Use this function to srape fantasy football projections from WalterFootball Only
#' seasonal projections are available.
#' @param season The season to scrape data for. 
#' @export
scrape_wf <- function(season = NULL){
  if(is.null(season))
    season <- current_season()
  
  if(season > current_season()){
    stop("Invalid season. Please specify ", current_season(), " or earlier", call. = FALSE)
  }
  
  wf_file <-  tempfile("wf", fileext = ".xlsx")

  fname <- sprintf("http://walterfootball.com/fantasy%srankingsexcel.xlsx", as.character(season))
  wd_dl <- download.file(fname, wf_file, mode = "wb")

  sheet_name <- c("QBs", "RBs", "WRs", "TEs", "Ks")

  wf_data <- data.frame(dplyr::bind_rows(sapply(sheet_name, readxl::read_xlsx, path = wf_file)))

  wf_data <- wf_data %>% unite("Player", First.Name, Last.Name, sep = " ")

  remove_nacol <- function(x)any(!is.na(x))
  wf_data <- wf_data %>% select_if(remove_nacol)
  wf_data <- wf_data %>% select(-matches("\\.{2}|__|VBD|Points|POINTS"))
  wf_data <- wf_data %>% select(-one_of(c("Bonus", "Dynst", "QB", "BYE", "Bye",
                                          intersect(names(wf_data), sheet_name),
                                          "Tes", "Vmodes"))) %>%
    rename(pass_tds = PASS.TD, pass_int = INT, rec = CATCH, reg_tds = REG.TD,
           fg_0039 = FG.1.39, fg_4049 = FG.40.49, fg_50 = FG.50.) %>%
    janitor::clean_names()

  structure(wf_data, source = "WalterFootball", season = season)

}
