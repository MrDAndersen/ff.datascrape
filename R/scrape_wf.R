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

  sheet_name <- c(QB = "QBs", RB = "RBs", WR = "WRs", TE = "TEs", K = "Ks")

  remove_nacol <- function(x)any(!is.na(x))
  wf_data <- lapply(sheet_name, function(s){
      wf_sheet <- readxl::read_xlsx(path = wf_file, sheet = s)
      wf_col <- c(pass_tds = "PASS TD", pass_int = "INT", rec = "CATCH",
          reg_tds = "REG TD", fg_0039 = "FG 1-39",
          fg_4049 = "FG 40-49", fg_50 = "FG 50+")

      rename_col <- wf_col[which(wf_col %in%  names(wf_sheet))]
      remove_cols <- intersect(names(wf_sheet),
                              c("Bonus", "Dynst", "QB", "BYE", "Bye", s, "Tes", "Vmodes"))

      wf_sheet <- wf_sheet %>%
        unite("Player", "First Name", "Last Name", sep = " ") %>%
        rename(!!!rename_col) %>%
        select_if(remove_nacol) %>%
        select(-matches("\\.{2}|__|VBD|Points|POINTS")) %>%
        select(-one_of(remove_cols)) %>%
        clean_names() %>%
        as.data.frame() %>%
        mutate(team = mflTeam.abb[match(team, nflTeam.name)])

      wf_sheet <- add_column(wf_sheet, id = match_players(wf_sheet), .before = 1) %>%
        structure(source = "WalterFootball", season = season)
  })

}
