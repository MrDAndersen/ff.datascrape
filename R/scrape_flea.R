
#' @export
scrape_fleaflick <- function(
  position = c("QB", "RB", "WR", "TE", "Flex", "K", "DST", "DB", "DL", "LB", "IDP")
  )
{
  flea_positions = c("QB" = 4, "RB" = 1, "WR" = 2, "TE" = 8, "Flex" = 11, "K" = 16,
                     "DST" = 256, "DB" = 32, "DL" = 64, "LB" = 128, "IDP" = 224)

  flea_base <- str_to_url("https://www.fleaflicker.com/nfl/leaders")
  flea_qry <- list(statType = 7, sortMode = 1)

  flea_qry$position <- flea_positions[[position]]
  flea_qry$tableOffset <- 0

  flea_url <- modify_url(flea_base, query = flea_qry)

  flea_data <- data.frame()

  flea_session <- html_session(flea_url)
  repeat({

    flea_page <- read_html(flea_session)

    flea_table <- flea_page %>%
      html_node("#body-center-main table") %>%
      html_table()

    names(flea_table) <- trimws(paste(gsub("^Projected |\\sWeek [0-9]+$", "", names(flea_table)),
                               flea_table[1,]))

    flea_table <- flea_table[-1,]

    flea_table <- flea_table %>% repair_names(prefix = "")

    del_cols <- intersect(names(flea_table), as.character(1:9))

    flea_table <- flea_table %>% select(-one_of(del_cols))

    flea_table[, 1] <- gsub("^(Q|D|OUT|SUS|IR)([A-Z])", "\\2", flea_table[, 1])

    flea_table <- flea_table[1:(nrow(flea_table) - 1),]

    flea_table <- flea_table %>%
      extract("Player Name", c("Player", "Pos", "Team", "Bye"),
              "([A-Za-z0-9'-.\\s]+)\\s([QRWTDKL][BRESL/]*[SFT]*[T]*)\\s([ABCDFGHIJKLMNOPSTW][ACIOUHFTELNYBR][TLCFIUSRNEDGJKA]*)\\s*\\(*([0-9]*)\\)*"
      )

    player_id <- flea_page %>%
      html_nodes("a.player-text") %>%
      html_attr("href") %>%
      str_extract("[0-9]{3,}$")

    flea_table <- flea_table %>% add_column(id = player_id, .before =1)
    flea_table$id <- player_id

    flea_data <- bind_rows(flea_data, flea_table)

    next_url <- flea_page %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")

    if(is.na(next_url))
      break

    flea_session <- flea_session %>% jump_to(next_url)
  })

  return(flea_data)
}

