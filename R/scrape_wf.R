#' @export
scrape_wf <- function(season = NULL){

  wf_file <-  file.path(tempdir(), "wf.xlsx")

  fname <- sprintf("http://walterfootball.com/fantasy%srankingsexcel.xlsx", as.character(season))
  wd_dl <- download.file(fname, wf_file)

  sheet_name <- c("QBs", "RBs", "WRs", "TEs", "Ks")

  wf_data <- data.frame(dplyr::bind_rows(sapply(sheet_name, readxl::read_xlsx, path = wf_file)))

  wf_data <- wf_data %>% unite("Player", First.Name, Last.Name, sep = " ")

  remove_nacol <- function(x)any(!is.na(x))
  wf_data <- wf_data %>% select_if(remove_nacol)
  wf_data <- wf_data %>% select(-matches("\\.{2}|__|VBD|Points|POINTS"))
  wf_data <- wf_data %>% select(-one_of(c("Bonus", "Dynst", "QB", "BYE", "Bye",
                                          sheet_name, "Tes", "Vmodes")))

  structure(wf_data, source = "WalterFootball", season = season)

}
